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
    
    struct OperationEater<G : Grapheme> {
        typealias Operation = Automaton<G>.Operation
        
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
    
    typealias Lexed<G : Grapheme> = Lexer.Lexed<G>
    typealias LexedIterator<G : Grapheme> = IteratorProtocol<Lexed<G>>
    typealias Operation<G : Grapheme> = Automaton<G>.Operation
    
    static func parse<G>(lexedList: [Lexed<G>]) throws -> Automaton<G> {
        var graphemes = [G]()
        
        var eater = OperationEater<G>()
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
    
    enum CommonSwitch<G : Grapheme> {
        case op(Operation<G>)
        case decorator((inout Operation<G>) -> Void)
        case grapheme(G)
        case notHandled(Lexeme<G>.Reserved)
    }
    
    static func commonSwitch<G>(lexed: Lexed<G>, lexedList: inout some LexedIterator<G>, graphemes: inout [G], groupCount: inout Int) throws -> CommonSwitch<G> {
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
    
    static func repeater<G>(times: Automaton<G>.Repeater.Times, isGreedy: Bool) -> (inout Operation<G>) -> Void {
        { oldOp in
            oldOp = .repeater(.init(child: oldOp, times: times, isGreedy: isGreedy))
        }
    }
    
    static func repeatZeroOrMore<G>() -> (inout Operation<G>) -> Void {
        repeater(times: .atLeast(0), isGreedy: true)
    }
    
    static func repeatOneOrMore<G>() -> (inout Operation<G>) -> Void {
        repeater(times: .atLeast(1), isGreedy: true)
    }
    
    static func repeatZeroOrOne<G>() -> (inout Operation<G>) -> Void {
        repeater(times: .atMost(1), isGreedy: true)
    }
    
    static func parseGroupCommon<G>(lexedList: inout some LexedIterator<G>, groupCount: inout Int) throws -> Automaton<G>.Grouper {
        var graphemes = [G]()
        var eater = OperationEater<G>()
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
    
    static func parseGroup<G>(lexedList: inout some LexedIterator<G>, groupCount: inout Int) throws -> Operation<G> {
        groupCount += 1
        let oldGroupCount = groupCount
        let grouper = try parseGroupCommon(lexedList: &lexedList, groupCount: &groupCount)
        return .group(grouper, oldGroupCount)
    }
    
    static func parseNonCapturingGroup<G>(lexedList: inout some LexedIterator<G>, groupCount: inout Int) throws -> Operation<G> {
        let grouper = try parseGroupCommon(lexedList: &lexedList, groupCount: &groupCount)
        return .group(grouper, nil)
    }
    
    static func parseMatch<G>(lexedList: inout some LexedIterator<G>) throws -> Operation<G> {
        try .match(parseMatchCommon(lexedList: &lexedList))
    }
        
    static func parseNotMatch<G>(lexedList: inout some LexedIterator<G>) throws -> Operation<G> {
        try .notMatch(parseMatchCommon(lexedList: &lexedList))
    }
    
    static func parseMatchCommon<G>(lexedList: inout some LexedIterator<G>) throws -> (G) -> Bool {
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
