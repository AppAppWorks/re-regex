//
//  LexemeGrapheme.swift
//
//
//  Created by Lau Chun Kai on 28/2/2024.
//

public protocol Grapheme : Equatable {
    var isLowercase: Bool { get }
    
    var isUppercase: Bool { get }
    
    var isLetter: Bool { get }
    
    var isAlphanumeric: Bool { get }
    
    var isNumber: Bool { get }
    
    var isWhitespace: Bool { get }
    
    var string: String { get }
    
    static func ~=(lhs: Unicode.Scalar, rhs: Self) -> Bool
    
    static func ~=(lhs: ClosedRange<Unicode.Scalar>, rhs: Self) -> Bool
    
    static var zero: Self { get }
    
    associatedtype Matcher : GraphemeMatcher<Self>
    
    static func matcher() -> Matcher
    
    init(ascii: Unicode.Scalar)
}

public protocol GraphemePattern<Element> : RangeReplaceableCollection, Equatable where Element : Grapheme {
    
    init()
    
    init(_ s: String)
    
    mutating func append(_ element: Element)
    
    func string() -> String
    
    var positiveInteger: Int? { get }
}

public protocol Graphemes<Element> : Sequence where Element : Grapheme {
    associatedtype Index : Comparable
    associatedtype SubSequence : Graphemes<Element> where SubSequence.SubSequence == SubSequence, Index == SubSequence.Index
    
    var underestimatedCount: Int { get }
    
    var offset: Int { get }
    
    func dropFirst() -> (Element, SubSequence)
    
    func dropFirst(_ k: Int) -> SubSequence
    
    var startIndex: Index { get }
    
    var endIndex: Index { get }
    
    var isEmpty: Bool { get }
    
    var eot: SubSequence { get }
    
    subscript(range: some RangeExpression<Index>) -> String { get }
    
    static func convertToGraphemes(s: String) -> Self
}

public protocol StringConvertibleCollection : Collection where SubSequence : StringConvertibleCollection {
    
    init(s: String)
    
    subscript(range: some RangeExpression<Index>) -> String { get }
    
    func string() -> String
}

protocol CollectionGraphemes<C> : Graphemes where Element : Grapheme, Element == C.Element, SubSequence : CollectionGraphemes<C.SubSequence> {
    
    associatedtype C : StringConvertibleCollection
    
    var c: C { get }
    
    init(c: C, offset: Int)
}

extension CollectionGraphemes {    
    public var startIndex: C.Index {
        c.startIndex
    }
    
    public var endIndex: C.Index {
        c.endIndex
    }
    
    public var isEmpty: Bool {
        c.isEmpty
    }
    
    public func dropFirst() -> (Element, SubSequence) {
        (c.first!, .init(c: c.dropFirst(1), offset: offset + 1))
    }
    
    public func dropFirst(_ k: Int) -> SubSequence {
        .init(c: c.dropFirst(k), offset: offset + k)
    }
    
    public func makeIterator() -> C.Iterator {
        c.makeIterator()
    }
    
    public subscript(range: some RangeExpression<C.Index>) -> String {
        c[range]
    }
}

public struct NoOpGraphemes<C : StringConvertibleCollection> : Graphemes, CollectionGraphemes, CustomStringConvertible where C.Element : Grapheme {
    public typealias Element = C.Element
    public typealias Iterator = C.Iterator
    
    public func dropFirst() -> (Element, SubSequence) {
        (c.first!, .init(c: c.dropFirst(1), offset: 1))
    }
    
    public func dropFirst(_ k: Int) -> SubSequence {
        .init(c: c.dropFirst(k), offset: k)
    }
    
    public typealias Index = C.Index
    
    public let c: C
    
    public let offset = 0
    
    public typealias SubSequence = NoOpSubGraphemes<C.SubSequence>
    
    public init(c: C) {
        self.c = c
    }
    
    init(c: C, offset: Int) {
        fatalError("don't use it")
    }
    
    public var underestimatedCount: Int {
        c.underestimatedCount
    }
    
    public static func convertToGraphemes(s: String) -> Self {
        .init(c: .init(s: s))
    }
    
    public var description: String {
        "[0]\(c.string())"
    }
    
    public var eot: SubSequence {
        .init(c: c[c.endIndex...], offset: 0)
    }
}

public struct NoOpSubGraphemes<C : StringConvertibleCollection> : Graphemes, CollectionGraphemes, CustomStringConvertible where C.Element : Grapheme, C == C.SubSequence {
    public typealias Element = C.Element
    public typealias Iterator = C.Iterator
    
    public let c: C
    
    public let offset: Int
    
    public typealias SubSequence = Self
    
    public init(c: C, offset: Int) {
        self.c = c
        self.offset = offset
    }
    
    public var underestimatedCount: Int {
        c.underestimatedCount
    }
    
    public static func convertToGraphemes(s: String) -> Self {
        fatalError()
    }
    
    public var description: String {
        "[\(offset)]\(c.string())"
    }
    
    public var eot: NoOpSubGraphemes<C> {
        .init(c: c[c.endIndex...], offset: 0)
    }
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

extension String : StringConvertibleCollection {
    public init(s: String) {
        self = s
    }
    
    public subscript(range: some RangeExpression<Index>) -> String {
        let ss: Substring = self[range]
        return String(ss)
    }
    
    public func string() -> String {
        self
    }
}

extension Substring : StringConvertibleCollection {
    public init(s: String) {
        fatalError()
    }
    
    public subscript(range: some RangeExpression<Index>) -> String {
        let ss: Substring = self[range]
        return String(ss)
    }
    
    public func string() -> String {
        String(self)
    }
}

extension String : GraphemePattern {
    public mutating func append(_ element: Character) {
        insert(element, at: endIndex)
    }
    
    public var positiveInteger: Int? {
        Int(self, radix: 10)
    }
}

extension Character : Grapheme {
    public typealias Pattern = String
    
    public static func convertToPatterns(s: String) -> String {
        s
    }
    
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
    
    public static func convertToGraphemes(s: String) -> NoOpGraphemes<String> {
        .init(c: s)
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

extension [UInt8] : StringConvertibleCollection {
    
    public init(s: String) {
        self.init(s.utf8)
    }
    
    public subscript(range: some RangeExpression<Index>) -> String {
        String(decoding: self[range], as: UTF8.self)
    }
    
    public func string() -> String {
        String(decoding: self, as: UTF8.self)
    }
}

extension ArraySlice<UInt8> : StringConvertibleCollection {
    
    public init(s: String) {
        fatalError()
    }
    
    public subscript(range: some RangeExpression<Index>) -> String {
        String(decoding: self[range], as: UTF8.self)
    }
    
    public func string() -> String {
        String(decoding: self, as: UTF8.self)
    }
}

extension [UInt8] : GraphemePattern {
    public init(_ s: String) {
        self.init(s.utf8)
    }
    
    public var positiveInteger: Int? {
        Int(String(decoding: self, as: UTF8.self), radix: 10)
    }
}

extension UInt8 : Grapheme {
    public var isLowercase: Bool {
        "a"..."z" ~= self
    }
    
    public var isUppercase: Bool {
        "A"..."Z" ~= self
    }
    
    public var isLetter: Bool {
        isLowercase || isUppercase
    }
    
    public typealias Pattern = [UInt8]
    
    public static func convertToPatterns(s: String) -> [UInt8] {
        Array(s.utf8)
    }
    
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
    
    public static func convertToGraphemes(s: String) -> NoOpGraphemes<[UInt8]> {
        .init(c: Array(s.utf8))
    }
    
    public static func matcher() -> BitBus {
        .init()
    }
}
