//
//  Parser.swift
//  
//
//  Created by Lau Chun Kai on 26/2/2024.
//

public enum Parser {
    struct ParserError : Error {
        let offset: Int
    }
    
    static let alphaNumericMatch: (UInt8) -> Bool = { byte in
        switch byte {
        case "a"..."z", "A"..."Z", "0"..."9":
            true
        case _:
            false
        }
    }
    
    static let digitsMatch: (UInt8) -> Bool = { byte in
        switch byte {
        case "0"..."9":
            true
        case _:
            false
        }
    }
    
    static let whiteSpaceMatch: (UInt8) -> Bool = { byte in
        switch byte {
        case "\t", " ", "\n":
            true
        case _:
            false
        }
    }
    
    struct OperationEater {
        var topLevelOperations = [Automaton.Operation]()
        var operations = [Automaton.Operation]()
        var isOr = false
        
        mutating func eat(_ op: Automaton.Operation) {
            operations.append(op)
        }
        
        mutating func decorate(_ decorator: (inout Automaton.Operation) -> Void) -> Bool {
            if operations.isEmpty {
                return false
            } else {
                decorator(&operations[operations.count - 1])
                return true
            }
        }
        
        mutating func handleOr() {
            isOr = true
            
            switch operations.count {
            case 2...:
                topLevelOperations.append(.group(.init(operations: operations), nil))
            case 1:
                topLevelOperations.append(operations[0])
            case _:
                return
            }
            
            operations.removeAll()
        }
        
        mutating func wrapUp() {
            guard isOr && !topLevelOperations.isEmpty else {
                return
            }
                
            switch operations.count {
            case 2...:
                topLevelOperations.append(.group(.init(operations: operations), nil))
            case 1:
                topLevelOperations.append(operations[0])
            case _:
                break
            }
        }
    }
    
    static func parse(lexedList: [Lexer.Lexed]) throws -> Automaton {
        var utf8s = [UInt8]()
        
        var eater = OperationEater()
        var groupCount = 0
        
        var cursor = lexedList.makeIterator()
        
        while let lexed = cursor.next() {
            let result = try commonSwitch(lexed: lexed, lexedList: &cursor, utf8s: &utf8s, groupCount: &groupCount)
            if case let .utf8(utf8) = result {
                utf8s.append(utf8)
                continue
            } else {
                if !utf8s.isEmpty {
                    eater.eat(.bytes(utf8s))
                    utf8s.removeAll()
                }
            }
            
            switch result {
            case let .op(operation):
                eater.eat(operation)
            case let .notHandled(reserved):
                switch reserved {
                case .groupEnd:
                    throw ParserError(offset: lexed.offset)
                case .or:
                    eater.handleOr()
                case _:
                    throw ParserError(offset: lexed.offset)
                }
            case let .decorator(decorator):
                if !eater.decorate(decorator) {
                    throw ParserError(offset: lexed.offset)
                }
            default:
                throw ParserError(offset: lexed.offset)
            }
        }
        
        if !utf8s.isEmpty {
            eater.eat(.bytes(utf8s))
        }
        
        eater.wrapUp()
        
        return if eater.topLevelOperations.isEmpty {
            .init(groupCount: groupCount, operations: eater.operations)
        } else {
            .init(groupCount: groupCount, .or(eater.topLevelOperations))
        }
    }
    
    enum CommonSwitch {
        case op(Automaton.Operation)
        case decorator((inout Automaton.Operation) -> Void)
        case utf8(UInt8)
        case notHandled(Lexeme.Reserved)
    }
    
    static func commonSwitch(lexed: Lexer.Lexed, lexedList: inout some IteratorProtocol<Lexer.Lexed>, utf8s: inout [UInt8], groupCount: inout Int) throws -> CommonSwitch {
        switch lexed.lexeme {
        case let .reserved(reserved):
            switch reserved {
            case .groupStart:
                try .op(parseGroup(lexedList: &lexedList, groupCount: &groupCount))
            case .nonCapturingGroupStart:
                try .op(parseNonCapturingGroup(lexedList: &lexedList, groupCount: &groupCount))
            case .start:
                .op(.start)
            case .end:
                .op(.end)
            case .allAlphanumeric:
                .op(.match(alphaNumericMatch))
            case .allDigits:
                .op(.match(digitsMatch))
            case .allNonAlphanumeric:
                .op(.notMatch(alphaNumericMatch))
            case .allNonDigits:
                .op(.notMatch(digitsMatch))
            case .allWhitespaces:
                .op(.match(whiteSpaceMatch))
            case .allNonWhitespaces:
                .op(.notMatch(whiteSpaceMatch))
            case .anyChar:
                .op(.notMatch({ "\n" ~= $0 }))
            case .matchStart:
                try .op(parseMatch(lexedList: &lexedList))
            case .notMatchStart:
                try .op(parseNotMatch(lexedList: &lexedList))
            case .matchEnd:
                throw ParserError(offset: lexed.offset)
            case .zeroOrMore:
                .decorator(repeatZeroOrMore)
            case .zeroOrOne:
                .decorator(repeatZeroOrOne)
            case .oneOrMore:
                .decorator(repeatOneOrMore)
            case _:
                .notHandled(reserved)
            }
        case let .utf8(byte):
            .utf8(byte)
        }
    }
    
    static func repeater(times: Automaton.Repeater.Times, isGreedy: Bool) -> (inout Automaton.Operation) -> Void {
        { oldOp in
            oldOp = .repeater(.init(child: oldOp, times: times, isGreedy: isGreedy))
        }
    }
    
    static let repeatZeroOrMore = repeater(times: .atLeast(0), isGreedy: true)
    static let repeatOneOrMore = repeater(times: .atLeast(1), isGreedy: true)
    static let repeatZeroOrOne = repeater(times: .atMost(1), isGreedy: true)
    
    static func parseGroupCommon(lexedList: inout some IteratorProtocol<Lexer.Lexed>, groupCount: inout Int) throws -> Automaton.Grouper {
        var utf8s = [UInt8]()
        var eater = OperationEater()
    outer:
        while let lexed = lexedList.next() {
            let result = try commonSwitch(lexed: lexed, lexedList: &lexedList, utf8s: &utf8s, groupCount: &groupCount)
            if case let .utf8(utf8) = result {
                utf8s.append(utf8)
                continue
            } else {
                if !utf8s.isEmpty {
                    eater.eat(.bytes(utf8s))
                    utf8s.removeAll()
                }
            }
            
            switch result {
            case let .op(operation):
                eater.eat(operation)
            case let .notHandled(reserved):
                switch reserved {
                case .groupEnd:
                    break outer
                case .or:
                    eater.handleOr()
                case _:
                    throw ParserError(offset: lexed.offset)
                }
            case let .decorator(decorator):
                if !eater.decorate(decorator) {
                    throw ParserError(offset: lexed.offset)
                }
            default:
                throw ParserError(offset: lexed.offset)
            }
        }
        
        if !utf8s.isEmpty {
            eater.eat(.bytes(utf8s))
        }
        
        eater.wrapUp()
        
        return if eater.topLevelOperations.isEmpty {
            .init(operations: eater.operations)
        } else {
            .init(operations: [.or(eater.topLevelOperations)])
        }
    }
    
    static func parseGroup(lexedList: inout some IteratorProtocol<Lexer.Lexed>, groupCount: inout Int) throws -> Automaton.Operation {
        groupCount += 1
        let oldGroupCount = groupCount
        let grouper = try parseGroupCommon(lexedList: &lexedList, groupCount: &groupCount)
        return .group(grouper, oldGroupCount)
    }
    
    static func parseNonCapturingGroup(lexedList: inout some IteratorProtocol<Lexer.Lexed>, groupCount: inout Int) throws -> Automaton.Operation {
        let grouper = try parseGroupCommon(lexedList: &lexedList, groupCount: &groupCount)
        return .group(grouper, nil)
    }
    
    static func parseMatch(lexedList: inout some IteratorProtocol<Lexer.Lexed>) throws -> Automaton.Operation {
        try .match(parseMatchCommon(lexedList: &lexedList))
    }
        
    static func parseNotMatch(lexedList: inout some IteratorProtocol<Lexer.Lexed>) throws -> Automaton.Operation {
        try .notMatch(parseMatchCommon(lexedList: &lexedList))
    }
    
    static func parseMatchCommon(lexedList: inout some IteratorProtocol<Lexer.Lexed>) throws -> (UInt8) -> Bool {
        var stupidMap = [Bool](repeating: false, count: 256)
        
        while let lexed = lexedList.next() {
            switch lexed.lexeme {
            case let .utf8(byte):
                stupidMap[Int(byte)] = true
            case .reserved(.matchEnd):
                return { byte in
                    stupidMap[Int(byte)]
                }
            case _:
                throw ParserError(offset: lexed.offset)
            }
        }
        
        throw ParserError(offset: .max)
    }
}
