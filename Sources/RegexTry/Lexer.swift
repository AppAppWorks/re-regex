//
//  Lexer.swift
//  
//
//  Created by Lau Chun Kai on 25/2/2024.
//

public enum Lexer {
    static func handleByte(worker: inout Worker, ret: inout [Lexed], byte: UInt8, offset: Int) throws {
        if !worker.composite.isEmpty {
            try handleComposite(worker: &worker, ret: &ret, byte: byte, offset: offset)
        } else if let result = try worker.handleByte(byte, offset: offset) {
            ret.append(result)
//            print("offset(\(offset)): intermediates: \(worker.intermediates), last: \((ret.last?.lexeme)?.description ?? "null")")
        }
    }

    
    static func handleComposite(worker: inout Worker, ret: inout [Lexed], byte: UInt8, offset: Int) throws {
        guard let compositeResult = worker.handleComposite(newByte: byte, offset: offset) else {
            return;
        }
        
        ret.append(compositeResult.composite)
//        print("offset(\(offset)): intermediates: \(worker.intermediates), last: \((ret.last?.lexeme)?.description ?? "null")")
        
        if let (otherBytes, otherOffset) = compositeResult.other {
            for (i, oldByte) in otherBytes.enumerated() {
                try handleByte(worker: &worker, ret: &ret, byte: oldByte, offset: otherOffset + i)
            }
        }
    }
    
    struct Worker {
        var composite = [UInt8]()
        var other = [UInt8]()
        var intermediates = [Intermediate]()
    }
}

public extension Lexer {
    typealias Lexed = (lexeme: Lexeme, offset: Int)
    
    struct LexerError : Error {
        let lexeme: Lexeme
        let context: Context
        let offset: Int
        
        enum Context {
            case plain
            case grouping
            case escaping
            case repeating
            case matching
        }
    }
    
    static func lex(s: String) throws -> [Lexed]  {
        var ret = [(lexeme: Lexeme, offset: Int)]()
        ret.reserveCapacity(s.utf8.count)
        
        var worker = Worker()
        
        for (offset, byte) in s.utf8.enumerated() {
            try handleByte(worker: &worker, ret: &ret, byte: byte, offset: offset)
        }
        
        if let intermediate = worker.intermediates.last {
            let context: LexerError.Context =
            switch intermediate {
            case .grouping:
                .grouping
            case .escaping:
                .escaping
            case .matching:
                .matching
            case .repeating:
                .repeating
            }
            
            throw LexerError(byte: 0, context: context, offset: s.utf8.count)
        }
        
        return ret
    }
}

extension Lexer.Worker {
    enum Intermediate {
        case grouping
        case escaping
        case matching
        case repeating
    }
    
    typealias CompositeResult = ((composite: (Lexeme, Int), other: ([UInt8], Int)?))?
    
    mutating func handleByte(_ newByte: UInt8, offset: Int) throws -> Lexer.Lexed? {
        assert(composite.isEmpty)
        
        return switch intermediates.last {
        case .escaping:
            try handleEscaping(newByte: newByte, offset: offset)
        case .grouping:
            try handleGrouping(newByte: newByte, offset: offset)
        case .matching:
            handleMatching(newByte: newByte, offset: offset)
        case .repeating:
            try handleRepeating(newByte: newByte, offset: offset)
        case nil:
            try handleNormal(newByte: newByte, offset: offset)
        }
    }
    
    mutating func handleNormal(newByte: UInt8, offset: Int) throws -> Lexer.Lexed? {
        switch newByte {
        case "\\":
            intermediates.append(.escaping)
            return nil
        case "{":
            intermediates.append(.repeating)
            return (.reserved(.repetitionStart), offset)
        case "(", "[", "*", "?", "+":
            composite.append(newByte)
            return nil
        case "}":
            throw Lexer.LexerError.plain(reserved: .repetitionEnd, offset: offset)
        case ")":
            throw Lexer.LexerError.plain(reserved: .groupEnd, offset: offset)
        case "]":
            throw Lexer.LexerError.plain(reserved: .matchEnd, offset: offset)
        case ".":
            return (.reserved(.anyChar), offset)
        case "|":
            return (.reserved(.or), offset)
        case "^":
            return (.reserved(.start), offset)
        case "$":
            return (.reserved(.end), offset)
        case _:
            return (.utf8(newByte), offset)
        }
    }
    
    mutating func handleRepeating(newByte: UInt8, offset: Int) throws -> Lexer.Lexed? {
        assert(intermediates.last == .repeating)
        
        switch newByte {
        case Unicode.Scalar("0")..."9":
            return (.utf8(newByte), offset)
        case ",":
            return (.reserved(.repetitionComma), offset)
        case "}":
            intermediates.removeLast()
            composite.append(newByte)
            return nil
        case _:
            throw Lexer.LexerError.repeating(byte: newByte, offset: offset)
        }
    }
    
    mutating func handleMatching(newByte: UInt8, offset: Int) -> Lexer.Lexed? {
        assert(intermediates.last == .matching)
        
        switch newByte {
        case "\\":
            intermediates.append(.escaping)
            return nil
        case "]":
            intermediates.removeLast()
            return (.reserved(.matchEnd), offset)
        case "-":
            return (.reserved(.matchRange), offset)
        case _:
            return (.utf8(newByte), offset)
        }
    }
    
    mutating func handleGrouping(newByte: UInt8, offset: Int) throws -> Lexer.Lexed? {
        assert(intermediates.last == .grouping)
        
        switch newByte {
        case ")":
            intermediates.removeLast()
            return (.reserved(.groupEnd), offset)
        case _:
            return try handleNormal(newByte: newByte, offset: offset)
        }
    }
    
    mutating func handleEscaping(newByte: UInt8, offset: Int) throws -> Lexer.Lexed {
        let ret: Lexeme =
        switch newByte {
        case "\\", "{", "}", "*", "?", "+", ".", "(", ")", "^", "$", "|", "[", "]":
            .utf8(newByte)
        case "w":
            .reserved(.allAlphanumeric)
        case "W":
            .reserved(.allNonAlphanumeric)
        case "d":
            .reserved(.allDigits)
        case "D":
            .reserved(.allNonDigits)
        case "s":
            .reserved(.allWhitespaces)
        case "S":
            .reserved(.allNonWhitespaces)
        case _:
            throw Lexer.LexerError.escaping(byte: newByte, offset: offset)
        }
        
        intermediates.removeLast()
        
        return (ret, offset - 1)
    }
    
    mutating func handleComposite(newByte: UInt8, offset: Int) -> CompositeResult {
        assert(!composite.isEmpty)
        
        return switch composite[0] {
        case "(":
            formingGroup(newByte: newByte, offset: offset)
        case "[":
            formingMatch(newByte: newByte, offset: offset)
        case "*", "?", "+", "}":
            formingGreedy(newByte: newByte, offset: offset)
        case _:
            fatalError("impossible")
        }
    }
    
    mutating func formingGroup(newByte: UInt8, offset: Int) -> CompositeResult {
        assert("(" ~= composite[0])
        
        if composite.count == 1 {
            switch newByte {
            case "?", "<":
                composite.append(newByte)
                return nil
            case _:
                composite.removeAll()
                intermediates.append(.grouping)
                return ((.reserved(.groupStart), offset - 1), ([newByte], offset))
            }
        } else {
            assert("?" ~= composite[1] || "<" ~= composite[1])
            
            return switch composite[1] {
            case "?":
                formingGroupQuestionMark(newByte: newByte, offset: offset)
            case "<":
                formingGroupLookBehind(newByte: newByte, offset: offset)
            case _:
                fatalError("impossible")
            }
        }
    }
    
    mutating func formingGroupQuestionMark(newByte: UInt8, offset: Int) -> CompositeResult {
        assert("(" ~= composite[0])
        assert("?" ~= composite[1])
        
        defer {
            intermediates.append(.grouping)
            composite.removeAll()
        }
        
        switch newByte {
        case ":":
            return ((.reserved(.nonCapturingGroupStart), offset - 2), nil)
        case "=":
            return ((.reserved(.positiveLookAheadStart), offset - 2), nil)
        case "!":
            return ((.reserved(.negativeLookAheadStart), offset - 2), nil)
        case _:
            return ((.reserved(.groupStart), offset - 2), ([composite[1], newByte], offset - 1))
        }
    }
    
    mutating func formingGroupLookBehind(newByte: UInt8, offset: Int) -> CompositeResult {
        assert("(" ~= composite[0])
        assert("<" ~= composite[1])
        
        defer {
            intermediates.append(.grouping)
            composite.removeAll()
        }
        
        switch newByte {
        case "=":
            return ((.reserved(.positiveLookBehindStart), offset - 2), nil)
        case "!":
            return ((.reserved(.negativeLookBehindStart), offset - 2), nil)
        case _:
            return ((.reserved(.groupStart), offset - 2), ([composite[1], newByte], offset - 1))
        }
    }
 
    mutating func formingMatch(newByte: UInt8, offset: Int) -> CompositeResult {
        assert("[" ~= composite[0])
        
        intermediates.append(.matching)
        composite.removeAll()
        
        switch newByte {
        case "^":
            return ((.reserved(.notMatchStart), offset - 1), nil)
        case _:
            return ((.reserved(.matchStart), offset - 1), ([newByte], offset))
        }
    }
    
    mutating func formingGreedy(newByte: UInt8, offset: Int) -> CompositeResult {
        assert("}" ~= composite[0] || "*" ~= composite[0] || "+" ~= composite[0] || "?" ~= composite[0])
        
        defer {
            composite.removeAll()
        }
        
        let isNonGreedy = "?" ~= newByte
        
        let reserved: Lexeme.Reserved =
        switch composite[0] {
        case "}":
            isNonGreedy ? .repetitionEndNonGreedy : .repetitionEnd
        case "*":
            isNonGreedy ? .zeroOrMoreNonGreedy : .zeroOrMore
        case "+":
            isNonGreedy ? .oneOrMoreNonGreedy : .oneOrMore
        case "?":
            isNonGreedy ? .zeroOrOneNonGreedy : .zeroOrOne
        case _:
            fatalError("impossible")
        }
        
        return ((.reserved(reserved), offset - 1), isNonGreedy ? nil : ([newByte], offset))
    }
}

extension Lexer.LexerError {
    init(byte: UInt8, context: Context, offset: Int) {
        self.init(lexeme: .utf8(byte), context: context, offset: offset)
    }
    
    init(reserved: Lexeme.Reserved, context: Context, offset: Int) {
        self.init(lexeme: .reserved(reserved), context: context, offset: offset)
    }
    
    static func plain(reserved: Lexeme.Reserved, offset: Int) -> Self {
        Self(reserved: reserved, context: .plain, offset: offset)
    }
    
    static func escaping(byte: UInt8, offset: Int) -> Self {
        Self(byte: byte, context: .escaping, offset: offset)
    }
    
    static func repeating(byte: UInt8, offset: Int) -> Self {
        Self(byte: byte, context: .repeating, offset: offset)
    }
}

extension UInt8 {
    static func ~=(lhs: Unicode.Scalar, rhs: Self) -> Bool {
        rhs == .init(ascii: lhs)
    }
    
    static func ~=(lhs: ClosedRange<Unicode.Scalar>, rhs: Self) -> Bool {
        UInt8(ascii: lhs.lowerBound)...UInt8(ascii: lhs.upperBound) ~= rhs
    }
}
