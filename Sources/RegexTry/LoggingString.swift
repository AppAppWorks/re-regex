//
//  LoggingString.swift
//
//
//  Created by Lau Chun Kai on 14/3/2024.
//

struct DebugString : LoggingString {
    struct StringInterpolation : LogginStringInterpolation {
        mutating func appendLiteral(_ literal: String) {
            proxy.appendLiteral(literal)
        }
        
        mutating func appendInterpolation(_ value: some (CustomDebugStringConvertible & CustomStringConvertible & TextOutputStreamable)) {
            proxy.appendInterpolation(value as (any CustomDebugStringConvertible))
        }
        
        mutating func appendInterpolation(_ value: some Any) {
            proxy.appendInterpolation(value)
        }
        
        mutating func appendInterpolation(_ value: some (CustomStringConvertible & CustomDebugStringConvertible)) {
            proxy.appendInterpolation(String(reflecting: value))
        }
        
        mutating func appendInterpolation(_ value: some CustomStringConvertible) {
            proxy.appendInterpolation(value)
        }
        
        mutating func appendInterpolation(_ value: some TextOutputStreamable) {
            proxy.appendInterpolation(value)
        }
        
        mutating func appendInterpolation(_ value: some (CustomStringConvertible & TextOutputStreamable)) {
            proxy.appendInterpolation(value)
        }
        
        mutating func appendInterpolation(_ value: Any.Type) {
            proxy.appendInterpolation(value)
        }
        
        mutating func appendInterpolation<each T>(tuple: (repeat each T)) {
            appendInterpolation("(")
            repeat appendWithComma(each tuple)
            appendInterpolation(")")
        }
        
        mutating func appendInterpolation<each T>(tuple: (repeat each T)?) {
            if let tuple {
                appendInterpolation("(")
                repeat appendWithComma(each tuple)
                appendInterpolation(")")
            } else {
                appendInterpolation(type(of: tuple))
            }
        }
        
        fileprivate mutating func appendWithComma(_ value: some Any) {
            if let cdsc = value as? any CustomDebugStringConvertible {
                appendInterpolation(cdsc)
            } else {
                appendInterpolation(value)
            }
            appendInterpolation(",")
        }
        
        var proxy: DefaultStringInterpolation

        init(literalCapacity: Int, interpolationCount: Int) {
            proxy = .init(literalCapacity: literalCapacity, interpolationCount: interpolationCount)
        }
        
        mutating func appendInterpolation(_ any: some CustomDebugStringConvertible) {
            proxy.appendInterpolation(String(reflecting: any))
        }
        
        mutating func appendInterpolation(seq: some Sequence<some CustomDebugStringConvertible>, separator: String) {
            appendSequence(seq: seq, appending: { $0.appendInterpolation($1) }, separator: separator)
        }
        
        mutating func appendInterpolation(seq: some Sequence<some (CustomStringConvertible & CustomDebugStringConvertible)>, separator: String) {
            appendSequence(seq: seq, appending: { $0.appendInterpolation($1) }, separator: separator)
        }
        
        mutating func appendInterpolation(seq: some Sequence<some (CustomStringConvertible & CustomDebugStringConvertible & TextOutputStreamable)>, separator: String) {
            appendSequence(seq: seq, appending: { $0.appendInterpolation($1) }, separator: separator)
        }
        
        var description: String {
            proxy.description
        }
    }
    
    let description: String
    
    init(stringLiteral value: String) {
        description = value
    }
    
    init(stringInterpolation: StringInterpolation) {
        description = stringInterpolation.description
    }
}

protocol LoggingString : ExpressibleByStringInterpolation, CustomStringConvertible where StringInterpolation : LogginStringInterpolation, StringLiteralType == String {
}

protocol LogginStringInterpolation : StringInterpolationProtocol, CustomStringConvertible where StringLiteralType == String {
    mutating func appendInterpolation(_ value: some (CustomStringConvertible & TextOutputStreamable))

    mutating func appendInterpolation(_ value: some TextOutputStreamable)
    
    mutating func appendInterpolation(_ value: some (CustomDebugStringConvertible & CustomStringConvertible & TextOutputStreamable))

    mutating func appendInterpolation(_ value: some CustomStringConvertible)
    
    mutating func appendInterpolation(_ value: some CustomDebugStringConvertible)
    
    mutating func appendInterpolation(_ value: some (CustomStringConvertible & CustomDebugStringConvertible))

    mutating func appendInterpolation(_ value: some Any)

    mutating func appendInterpolation(_ value: Any.Type)
    
    mutating func appendInterpolation(opt: (some (CustomStringConvertible & TextOutputStreamable))?)
    
    mutating func appendInterpolation(opt: (some TextOutputStreamable)?)
    
    mutating func appendInterpolation<T : CustomDebugStringConvertible & CustomStringConvertible & TextOutputStreamable>(opt: T?)

    mutating func appendInterpolation(opt: (some CustomStringConvertible)?)
    
    mutating func appendInterpolation<T : CustomDebugStringConvertible>(opt: T?)
    
    mutating func appendInterpolation<T : CustomStringConvertible & CustomDebugStringConvertible>(opt: T?)

    mutating func appendInterpolation(opt: (some Any)?)

    mutating func appendInterpolation(opt: Any.Type?)
    
    mutating func appendInterpolation<each T>(tuple: (repeat each T))
    
    mutating func appendInterpolation<each T>(tuple: (repeat each T)?)
    
    mutating func appendInterpolation(seq: some Sequence<some (CustomStringConvertible & TextOutputStreamable)>, separator: String)
    
    mutating func appendInterpolation(seq: some Sequence<some TextOutputStreamable>, separator: String)
    
    mutating func appendInterpolation(seq: some Sequence<some CustomDebugStringConvertible>, separator: String)
    
    mutating func appendInterpolation(seq: some Sequence<some (CustomStringConvertible & CustomDebugStringConvertible)>, separator: String)
    
    mutating func appendInterpolation(seq: some Sequence<some (CustomStringConvertible & CustomDebugStringConvertible & TextOutputStreamable)>, separator: String)
    
    mutating func appendInterpolation(seq: some Sequence<some CustomStringConvertible>, separator: String)
    
    mutating func appendInterpolation(seq: some Sequence<Any.Type>, separator: String)
}

extension LogginStringInterpolation {
    mutating func appendInterpolation<T : CustomDebugStringConvertible & CustomStringConvertible & TextOutputStreamable>(opt: T?) {
        if let opt {
            appendInterpolation(opt)
        } else {
            appendInterpolation(T.self)
            appendInterpolation(".nil")
        }
    }
    
    mutating func appendInterpolation<T : CustomDebugStringConvertible>(opt: T?) {
        if let opt {
            appendInterpolation(opt)
        } else {
            appendInterpolation(T.self)
            appendInterpolation(".nil")
        }
    }
    
    mutating func appendInterpolation<T : CustomStringConvertible & CustomDebugStringConvertible>(opt: T?) {
        if let opt {
            appendInterpolation(opt)
        } else {
            appendInterpolation(T.self)
            appendInterpolation(".nil")
        }
    }

    mutating func appendInterpolation<T : CustomStringConvertible & TextOutputStreamable>(opt: T?) {
        if let opt {
            appendInterpolation(opt)
        } else {
            appendInterpolation(T.self)
            appendInterpolation(".nil")
        }
    }
    
    mutating func appendInterpolation<T : TextOutputStreamable>(opt: T?) {
        if let opt {
            appendInterpolation(opt)
        } else {
            appendInterpolation(T.self)
            appendInterpolation(".nil")
        }
    }

    mutating func appendInterpolation<T : CustomStringConvertible>(opt: T?) {
        if let opt {
            appendInterpolation(opt)
        } else {
            appendInterpolation(T.self)
            appendInterpolation(".nil")
        }
    }

    mutating func appendInterpolation<T>(opt: T?) {
        if let opt {
            appendInterpolation(opt)
        } else {
            appendInterpolation(T.self)
            appendInterpolation(".nil")
        }
    }

    mutating func appendInterpolation(opt: Any.Type?) {
        appendInterpolation(tuple: ("", "", 1))
        if let opt {
            appendInterpolation(opt)
        } else {
            appendInterpolation("Any.Type.nil")
        }
    }
    
    mutating func appendInterpolation<each T>(tuple: (repeat each T)) {
        appendInterpolation("(")
        repeat appendWithComma(each tuple)
        appendInterpolation(")")
    }
    
    mutating func appendInterpolation<each T>(tuple: (repeat each T)?) {
        if let tuple {
            appendInterpolation("(")
            repeat appendWithComma(each tuple)
            appendInterpolation(")")
        }
    }
    
    fileprivate mutating func appendWithComma<T>(_ value: T) {
        appendInterpolation(value)
        appendInterpolation(",")
    }
    
    mutating func appendInterpolation(seq: some Sequence, separator: String) {
        appendSequence(seq: seq, appending: { $0.appendInterpolation($1) }, separator: separator)
    }
    
    mutating func appendInterpolation(seq: some Sequence<some (CustomStringConvertible & TextOutputStreamable)>, separator: String) {
        appendSequence(seq: seq, appending: { $0.appendInterpolation($1) }, separator: separator)
    }
    
    mutating func appendInterpolation(seq: some Sequence<some TextOutputStreamable>, separator: String) {
        appendSequence(seq: seq, appending: { $0.appendInterpolation($1) }, separator: separator)
    }
    
    mutating func appendInterpolation(seq: some Sequence<some CustomStringConvertible>, separator: String) {
        appendSequence(seq: seq, appending: { $0.appendInterpolation($1) }, separator: separator)
    }
    
    mutating func appendInterpolation(seq: some Sequence<Any.Type>, separator: String) {
        appendSequence(seq: seq, appending: { $0.appendInterpolation($1) }, separator: separator)
    }
    
    fileprivate mutating func appendSequence<T>(seq: some Sequence<T>, appending: (inout Self, T) -> Void, separator: String) {
        guard let first = seq.first(where: { _ in true }) else {
            return
        }
        appending(&self, first)
        for next in seq.dropFirst() {
            appendLiteral(separator)
            appending(&self, next)
        }
    }
}

extension String : LoggingString {}

extension DefaultStringInterpolation : LogginStringInterpolation {}

protocol CustomLoggingStringConvertible {
    func loggingString<ST>() -> ST where ST : LoggingString
}

extension CustomLoggingStringConvertible where Self : CustomDebugStringConvertible {
    var debugDescription: String {
        let ds: DebugString = loggingString()
        return String(reflecting: Self.self) + ds.description
    }
}

extension CustomLoggingStringConvertible where Self : CustomStringConvertible {
    var description: String {
        let normal: String = loggingString()
        return normal
    }
}

protocol CustomLoggingStringConvertibleStruct : CustomLoggingStringConvertible {}

extension CustomLoggingStringConvertibleStruct where Self : CustomDebugStringConvertible {
    var debugDescription: String {
        let ds: DebugString = loggingString()
        return String(reflecting: Self.self) + "(" + ds.description + ")"
    }
}

protocol CustomDebugStringConvertibleEnum : CustomLoggingStringConvertible {}

extension CustomDebugStringConvertibleEnum where Self : CustomDebugStringConvertible {
    var debugDescription: String {
        let ds: DebugString = loggingString()
        return String(reflecting: Self.self) + "." + ds.description
    }
}
