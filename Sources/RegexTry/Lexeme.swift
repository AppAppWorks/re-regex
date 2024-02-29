//
//  Lexeme.swift
//
//
//  Created by Lau Chun Kai on 27/2/2024.
//

public enum Lexeme<G : Grapheme> {
    case reserved(Reserved)
    case grapheme(G)
}

public extension Lexeme {
    enum Reserved : String {
        case start = "^"
        case end = "$"
        case anyChar = "."
        case zeroOrMore = "*"
        case zeroOrMoreNonGreedy = "*?"
        case oneOrMore = "+"
        case oneOrMoreNonGreedy = "+?"
        case zeroOrOne = "?"
        case zeroOrOneNonGreedy = "??"
        case matchStart = "["
        case notMatchStart = "[^"
        case matchRange = "-"
        case matchEnd = "]"
        case groupStart = "("
        case nonCapturingGroupStart = "(?:"
        case positiveLookAheadStart = "(?="
        case negativeLookAheadStart = "(?!"
        case positiveLookBehindStart = "(<="
        case negativeLookBehindStart = "(<!"
        case or = "|"
        case groupEnd = ")"
        case repetitionStart = "{"
        case repetitionComma = ","
        case repetitionEnd = "}"
        case repetitionEndNonGreedy = "}?"
        case allDigits = "\\d"
        case allNonDigits = "\\D"
        case allAlphanumeric = "\\w"
        case allNonAlphanumeric = "\\W"
        case allWhitespaces = "\\s"
        case allNonWhitespaces = "\\S"
    }
}

extension Lexeme.Reserved {
    init?(byte: UInt8) {
        self.init(rawValue: String(UnicodeScalar(UInt32(byte))!))
    }
}

extension Lexeme : CustomStringConvertible {
    public var description: String {
        switch self {
        case .reserved(let reserved):
            "Lexeme.\(reserved)"
        case .grapheme(let grapheme):
            grapheme.string
        }
    }
}
