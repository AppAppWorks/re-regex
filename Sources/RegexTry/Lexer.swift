//
//  Lexer.swift
//  
//
//  Created by Lau Chun Kai on 25/2/2024.
//

public enum Lexer {
    static func handleGrapheme<G>(worker: inout Worker<G>, ret: inout [Lexed<G>], grapheme: G, offset: Int) throws {
        if !worker.composite.isEmpty {
            try handleComposite(worker: &worker, ret: &ret, grapheme: grapheme, offset: offset)
        } else if let result = try worker.handleGrapheme(grapheme, offset: offset) {
            ret.append(result)
//            print("offset(\(offset)): intermediates: \(worker.intermediates), last: \((ret.last?.lexeme)?.description ?? "null")")
        }
    }

    
    static func handleComposite<G>(worker: inout Worker<G>, ret: inout [Lexed<G>], grapheme: G, offset: Int) throws {
        guard let compositeResult = worker.handleComposite(newGrapheme: grapheme, offset: offset) else {
            return;
        }
        
        ret.append(compositeResult.composite)
//        print("offset(\(offset)): intermediates: \(worker.intermediates), last: \((ret.last?.lexeme)?.description ?? "null")")
        
        if let (otherGraphemes, otherOffset) = compositeResult.other {
            for (i, oldGrapheme) in otherGraphemes.enumerated() {
                try handleGrapheme(worker: &worker, ret: &ret, grapheme: oldGrapheme, offset: otherOffset + i)
            }
        }
    }
    
    struct Worker<G : Grapheme> {
        var composite = [G]()
        var other = [G]()
        var intermediates = [Intermediate]()
    }
}

public extension Lexer {
    typealias Lexed<G : Grapheme> = (lexeme: Lexeme<G>, offset: Int)
    
    struct LexerError<G : Grapheme> : Error {
        let lexeme: Lexeme<G>
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
    
    static func lexUnicode(s: String) throws -> [Lexed<Character>] {
        try lex(s: s)
    }
    
    static func lexAscii(s: String) throws -> [Lexed<UInt8>] {
        try lex(s: s)
    }
    
    static func lex<G>(s: String) throws -> [Lexed<G>]  {
        var ret = [Lexed<G>]()
        let graphemes = G.convertToGraphemes(s: s)
        ret.reserveCapacity(graphemes.count)
        
        var worker = Worker<G>()
        
        for (offset, grapheme) in graphemes.enumerated() {
            try handleGrapheme(worker: &worker, ret: &ret, grapheme: grapheme, offset: offset)
        }
        
        if let intermediate = worker.intermediates.last {
            let context: LexerError<G>.Context =
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
            
            throw LexerError(grapheme: .zero, context: context, offset: graphemes.count)
        }
        
        // TODO: - tidying up the flushing of the last bytes
        
        
        if !worker.composite.isEmpty {
            try handleComposite(worker: &worker, ret: &ret, grapheme: .init(ascii: "*"), offset: s.utf8.count)
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
    
    typealias CompositeResult = ((composite: (Lexeme<G>, Int), other: ([G], Int)?))?
    typealias Lexed = Lexer.Lexed<G>
    typealias Error = Lexer.LexerError<G>
    
    mutating func handleGrapheme(_ newGrapheme: G, offset: Int) throws -> Lexed? {
        assert(composite.isEmpty)
        
        return switch intermediates.last {
        case .escaping:
            try handleEscaping(newGrapheme: newGrapheme, offset: offset)
        case .grouping:
            try handleGrouping(newGrapheme: newGrapheme, offset: offset)
        case .matching:
            handleMatching(newGrapheme: newGrapheme, offset: offset)
        case .repeating:
            try handleRepeating(newGrapheme: newGrapheme, offset: offset)
        case nil:
            try handleNormal(newGrapheme: newGrapheme, offset: offset)
        }
    }
    
    mutating func handleNormal(newGrapheme: G, offset: Int) throws -> Lexed? {
        switch newGrapheme {
        case "\\":
            intermediates.append(.escaping)
            return nil
        case "{":
            intermediates.append(.repeating)
            return (.reserved(.repetitionStart), offset)
        case "(", "[", "*", "?", "+":
            composite.append(newGrapheme)
            return nil
        case "}":
            throw Error.plain(reserved: .repetitionEnd, offset: offset)
        case ")":
            throw Error.plain(reserved: .groupEnd, offset: offset)
        case "]":
            throw Error.plain(reserved: .matchEnd, offset: offset)
        case ".":
            return (.reserved(.anyChar), offset)
        case "|":
            return (.reserved(.or), offset)
        case "^":
            return (.reserved(.start), offset)
        case "$":
            return (.reserved(.end), offset)
        case _:
            return (.grapheme(newGrapheme), offset)
        }
    }
    
    mutating func handleRepeating(newGrapheme: G, offset: Int) throws -> Lexed? {
        assert(intermediates.last == .repeating)
        
        switch newGrapheme {
        case "0"..."9":
            return (.grapheme(newGrapheme), offset)
        case ",":
            return (.reserved(.repetitionComma), offset)
        case "}":
            intermediates.removeLast()
            composite.append(newGrapheme)
            return nil
        case _:
            throw Error.repeating(grapheme: newGrapheme, offset: offset)
        }
    }
    
    mutating func handleMatching(newGrapheme: G, offset: Int) -> Lexed? {
        assert(intermediates.last == .matching)
        
        switch newGrapheme {
        case "\\":
            intermediates.append(.escaping)
            return nil
        case "]":
            intermediates.removeLast()
            return (.reserved(.matchEnd), offset)
        case "-":
            return (.reserved(.matchRange), offset)
        case _:
            return (.grapheme(newGrapheme), offset)
        }
    }
    
    mutating func handleGrouping(newGrapheme: G, offset: Int) throws -> Lexed? {
        assert(intermediates.last == .grouping)
        
        switch newGrapheme {
        case ")":
            intermediates.removeLast()
            return (.reserved(.groupEnd), offset)
        case _:
            return try handleNormal(newGrapheme: newGrapheme, offset: offset)
        }
    }
    
    mutating func handleEscaping(newGrapheme: G, offset: Int) throws -> Lexed {
        let ret: Lexeme =
        switch newGrapheme {
        case "\\", "{", "}", "*", "?", "+", ".", "(", ")", "^", "$", "|", "[", "]":
            .grapheme(newGrapheme)
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
            throw Error.escaping(grapheme: newGrapheme, offset: offset)
        }
        
        intermediates.removeLast()
        
        return (ret, offset - 1)
    }
    
    mutating func handleComposite(newGrapheme: G, offset: Int) -> CompositeResult {
        assert(!composite.isEmpty)
        
        return switch composite[0] {
        case "(":
            formingGroup(newGrapheme: newGrapheme, offset: offset)
        case "[":
            formingMatch(newGrapheme: newGrapheme, offset: offset)
        case "*", "?", "+", "}":
            formingGreedy(newGrapheme: newGrapheme, offset: offset)
        case _:
            fatalError("impossible")
        }
    }
    
    mutating func formingGroup(newGrapheme: G, offset: Int) -> CompositeResult {
        assert("(" ~= composite[0])
        
        if composite.count == 1 {
            switch newGrapheme {
            case "?", "<":
                composite.append(newGrapheme)
                return nil
            case _:
                composite.removeAll()
                intermediates.append(.grouping)
                return ((.reserved(.groupStart), offset - 1), ([newGrapheme], offset))
            }
        } else {
            assert("?" ~= composite[1] || "<" ~= composite[1])
            
            return switch composite[1] {
            case "?":
                formingGroupQuestionMark(newGrapheme: newGrapheme, offset: offset)
            case "<":
                formingGroupLookBehind(newGrapheme: newGrapheme, offset: offset)
            case _:
                fatalError("impossible")
            }
        }
    }
    
    mutating func formingGroupQuestionMark(newGrapheme: G, offset: Int) -> CompositeResult {
        assert("(" ~= composite[0])
        assert("?" ~= composite[1])
        
        defer {
            intermediates.append(.grouping)
            composite.removeAll()
        }
        
        switch newGrapheme {
        case ":":
            return ((.reserved(.nonCapturingGroupStart), offset - 2), nil)
        case "=":
            return ((.reserved(.positiveLookAheadStart), offset - 2), nil)
        case "!":
            return ((.reserved(.negativeLookAheadStart), offset - 2), nil)
        case _:
            return ((.reserved(.groupStart), offset - 2), ([composite[1], newGrapheme], offset - 1))
        }
    }
    
    mutating func formingGroupLookBehind(newGrapheme: G, offset: Int) -> CompositeResult {
        assert("(" ~= composite[0])
        assert("<" ~= composite[1])
        
        defer {
            intermediates.append(.grouping)
            composite.removeAll()
        }
        
        switch newGrapheme {
        case "=":
            return ((.reserved(.positiveLookBehindStart), offset - 2), nil)
        case "!":
            return ((.reserved(.negativeLookBehindStart), offset - 2), nil)
        case _:
            return ((.reserved(.groupStart), offset - 2), ([composite[1], newGrapheme], offset - 1))
        }
    }
 
    mutating func formingMatch(newGrapheme: G, offset: Int) -> CompositeResult {
        assert("[" ~= composite[0])
        
        intermediates.append(.matching)
        composite.removeAll()
        
        switch newGrapheme {
        case "^":
            return ((.reserved(.notMatchStart), offset - 1), nil)
        case _:
            return ((.reserved(.matchStart), offset - 1), ([newGrapheme], offset))
        }
    }
    
    mutating func formingGreedy(newGrapheme: G, offset: Int) -> CompositeResult {
        assert("}" ~= composite[0] || "*" ~= composite[0] || "+" ~= composite[0] || "?" ~= composite[0])
        
        defer {
            composite.removeAll()
        }
        
        let isNonGreedy = "?" ~= newGrapheme
        
        let reserved: Lexeme<G>.Reserved =
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
        
        return ((.reserved(reserved), offset - 1), isNonGreedy ? nil : ([newGrapheme], offset))
    }
}

extension Lexer.LexerError {
    init(grapheme: G, context: Context, offset: Int) {
        self.init(lexeme: .grapheme(grapheme), context: context, offset: offset)
    }
    
    init(reserved: Lexeme<G>.Reserved, context: Context, offset: Int) {
        self.init(lexeme: .reserved(reserved), context: context, offset: offset)
    }
    
    static func plain(reserved: Lexeme<G>.Reserved, offset: Int) -> Self {
        Self(reserved: reserved, context: .plain, offset: offset)
    }
    
    static func escaping(grapheme: G, offset: Int) -> Self {
        Self(grapheme: grapheme, context: .escaping, offset: offset)
    }
    
    static func repeating(grapheme: G, offset: Int) -> Self {
        Self(grapheme: grapheme, context: .repeating, offset: offset)
    }
}
