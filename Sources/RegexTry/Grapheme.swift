//
//  LexemeGrapheme.swift
//
//
//  Created by Lau Chun Kai on 28/2/2024.
//

public protocol Grapheme : Equatable {
    var isAlphanumeric: Bool { get }
    
    var isNumber: Bool { get }
    
    var isWhitespace: Bool { get }
    
    var string: String { get }
    
    static func ~=(lhs: Unicode.Scalar, rhs: Self) -> Bool
    
    static func ~=(lhs: ClosedRange<Unicode.Scalar>, rhs: Self) -> Bool
    
    static var zero: Self { get }
    
    static func convertToGraphemes(s: String) -> [Self]
    
    associatedtype Matcher : GraphemeMatcher<Self>
    
    static func matcher() -> Matcher
    
    init(ascii: Unicode.Scalar)
}

public protocol GraphemeMatcher<G> {
    associatedtype G : Grapheme
    
    mutating func add(_ grapheme: G)
    
    func contains(_ grapheme: G) -> Bool
}

extension Set<Character> : GraphemeMatcher {
    public mutating func add(_ grapheme: Character) {
        insert(grapheme)
    }
    
    public typealias G = Character
    
}

extension Character : Grapheme {
    public static func matcher() -> Set<Character> {
        []
    }
    
    public static let zero: Character = "\u{0}"
    
    public var string: String {
        String(self)
    }
    
    public static func ~= (lhs: ClosedRange<Unicode.Scalar>, rhs: Self) -> Bool {
        lhs.contains(rhs.unicodeScalars.first!)
    }
    
    public static func ~= (lhs: Unicode.Scalar, rhs: Self) -> Bool {
        rhs.unicodeScalars.first == lhs
    }
    
    public var isAlphanumeric: Bool {
        isNumber || isLetter
    }
    
    public static func convertToGraphemes(s: String) -> [Character] {
        Array(s)
    }
    
    public init(ascii: Unicode.Scalar) {
        self.init(ascii)
    }
}

public struct BitBus : GraphemeMatcher {
    public mutating func add(_ grapheme: UInt8) {
        if grapheme > 63 {
            high |= 1 << (grapheme - 64)
        } else {
            low |= 1 << grapheme
        }
    }
    
    public func contains(_ grapheme: UInt8) -> Bool {
        if grapheme > 63 {
            high & 1 << (grapheme - 64) != 0
        } else {
            low & 1 << grapheme != 0
        }
    }
    
    var low: UInt64 = 0
    var high: UInt64 = 0
}

extension UInt8 : Grapheme {
    public var string: String {
        self < 128 ? "'\(Character(UnicodeScalar(UInt32(self))!))'" : "0x\(String(self, radix: 16))"
    }
    
    public var isAlphanumeric: Bool {
        switch self {
        case "a"..."z", "A"..."Z", "0"..."9":
            true
        case _:
            false
        }
    }
    
    public var isNumber: Bool {
        switch self {
        case "0"..."9":
            true
        case _:
            false
        }
    }
    
    public var isWhitespace: Bool {
        switch self {
        case "\t", " ", "\n":
            true
        case _:
            false
        }
    }
    
    public static func ~=(lhs: Unicode.Scalar, rhs: Self) -> Bool {
        rhs == .init(ascii: lhs)
    }
    
    public static func ~=(lhs: ClosedRange<Unicode.Scalar>, rhs: Self) -> Bool {
        UInt8(ascii: lhs.lowerBound)...UInt8(ascii: lhs.upperBound) ~= rhs
    }
    
    public static let zero: UInt8 = 0
    
    public static func convertToGraphemes(s: String) -> [UInt8] {
        Array(s.utf8)
    }
    
    public static func matcher() -> BitBus {
        .init()
    }
}
