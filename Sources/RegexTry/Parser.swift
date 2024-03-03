//
//  Parser.swift
//  
//
//  Created by Lau Chun Kai on 26/2/2024.
//

public enum Parser<GS, GP> where GS : Graphemes, GP : GraphemePattern<GS.Element> {
    typealias G = GS.Element
    typealias A = Automaton<GS, GP>
    typealias Operation = A.Operation
    
    struct ParserError : Error {
        let offset: Int
    }
    
    struct OperationEater {
        var topLevelOperations = [Operation]()
        var operations = [Operation]()
        var isOr = false
        
        mutating func eat(_ op: Operation) {
            operations.append(op)
        }
        
        mutating func decorate(_ decorator: (inout Operation) -> Void) -> Bool {
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
    
    typealias Lexed = Lexer<G>.Lexed
    typealias LexedIterator = IteratorProtocol<Lexed>
    
    static func parse(lexedList: [Lexed]) throws -> A {
        var graphemes = GP()
        
        var eater = OperationEater()
        var groupCount = 0
        
        var cursor = lexedList.makeIterator()
        
        while let lexed = cursor.next() {
            let result = try commonSwitch(lexed: lexed, lexedList: &cursor, graphemes: &graphemes, groupCount: &groupCount)
            if case let .grapheme(grapheme) = result {
                graphemes.append(grapheme)
                continue
            } else {
                if !graphemes.isEmpty {
                    eater.eat(.graphemes(graphemes))
                    graphemes.removeAll()
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
        
        if !graphemes.isEmpty {
            eater.eat(.graphemes(graphemes))
        }
        
        eater.wrapUp()
        
        return if eater.topLevelOperations.isEmpty {
            .init(groupCount: groupCount, operations: eater.operations)
        } else {
            .init(groupCount: groupCount, .or(eater.topLevelOperations))
        }
    }
    
    enum CommonSwitch {
        case op(Operation)
        case decorator((inout Operation) -> Void)
        case grapheme(G)
        case notHandled(Lexeme<G>.Reserved)
    }
    
    static func commonSwitch(lexed: Lexed, lexedList: inout some LexedIterator, graphemes: inout GP, groupCount: inout Int) throws -> CommonSwitch {
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
                .op(.match { $0.isAlphanumeric })
            case .allDigits:
                .op(.match { $0.isNumber })
            case .allNonAlphanumeric:
                .op(.notMatch { $0.isAlphanumeric })
            case .allNonDigits:
                .op(.notMatch { $0.isNumber })
            case .allWhitespaces:
                .op(.match { $0.isWhitespace })
            case .allNonWhitespaces:
                .op(.notMatch { $0.isWhitespace })
            case .anyChar:
                .op(.notMatch({ "\n" ~= $0 }))
            case .matchStart:
                try .op(parseMatch(lexedList: &lexedList))
            case .notMatchStart:
                try .op(parseNotMatch(lexedList: &lexedList))
            case .matchEnd:
                throw ParserError(offset: lexed.offset)
            case .zeroOrMore:
                .decorator(repeatZeroOrMore())
            case .zeroOrOne:
                .decorator(repeatZeroOrOne())
            case .oneOrMore:
                .decorator(repeatOneOrMore())
            case _:
                .notHandled(reserved)
            }
        case let .grapheme(byte):
            .grapheme(byte)
        }
    }
    
    static func repeater(times: A.Repeater.Times, isGreedy: Bool) -> (inout Operation) -> Void {
        { oldOp in
            oldOp = .repeater(.init(child: oldOp, times: times, isGreedy: isGreedy))
        }
    }
    
    static func repeatZeroOrMore() -> (inout Operation) -> Void {
        repeater(times: .atLeast(0), isGreedy: true)
    }
    
    static func repeatOneOrMore() -> (inout Operation) -> Void {
        repeater(times: .atLeast(1), isGreedy: true)
    }
    
    static func repeatZeroOrOne() -> (inout Operation) -> Void {
        repeater(times: .atMost(1), isGreedy: true)
    }
    
    static func parseGroupCommon(lexedList: inout some LexedIterator, groupCount: inout Int) throws -> A.Grouper {
        var graphemes = GP()
        var eater = OperationEater()
    outer:
        while let lexed = lexedList.next() {
            let result = try commonSwitch(lexed: lexed, lexedList: &lexedList, graphemes: &graphemes, groupCount: &groupCount)
            if case let .grapheme(grapheme) = result {
                graphemes.append(grapheme)
                continue
            } else {
                if !graphemes.isEmpty {
                    eater.eat(.graphemes(graphemes))
                    graphemes.removeAll()
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
        
        if !graphemes.isEmpty {
            eater.eat(.graphemes(graphemes))
        }
        
        eater.wrapUp()
        
        return if eater.topLevelOperations.isEmpty {
            .init(operations: eater.operations)
        } else {
            .init(operations: [.or(eater.topLevelOperations)])
        }
    }
    
    static func parseGroup(lexedList: inout some LexedIterator, groupCount: inout Int) throws -> Operation {
        groupCount += 1
        let oldGroupCount = groupCount
        let grouper = try parseGroupCommon(lexedList: &lexedList, groupCount: &groupCount)
        return .group(grouper, oldGroupCount)
    }
    
    static func parseNonCapturingGroup(lexedList: inout some LexedIterator, groupCount: inout Int) throws -> Operation {
        let grouper = try parseGroupCommon(lexedList: &lexedList, groupCount: &groupCount)
        return .group(grouper, nil)
    }
    
    static func parseMatch(lexedList: inout some LexedIterator) throws -> Operation {
        try .match(parseMatchCommon(lexedList: &lexedList))
    }
        
    static func parseNotMatch(lexedList: inout some LexedIterator) throws -> Operation {
        try .notMatch(parseMatchCommon(lexedList: &lexedList))
    }
    
    static func parseMatchCommon(lexedList: inout some LexedIterator) throws -> (G) -> Bool {
        var matcher = G.matcher()
        
        while let lexed = lexedList.next() {
            switch lexed.lexeme {
            case let .grapheme(grapheme):
                matcher.add(grapheme)
            case .reserved(.matchEnd):
                return matcher.contains(_:)
            case _:
                throw ParserError(offset: lexed.offset)
            }
        }
        
        throw ParserError(offset: .max)
    }
}
