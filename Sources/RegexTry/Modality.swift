//
//  File.swift
//  
//
//  Created by Lau Chun Kai on 13/3/2024.
//

/// The domain of modality is [1, ∞)
///
/// The case constructor `.finite(Wrapped)` should never be used directly.
enum Modality {
    typealias Wrapped = UInt
    
    case finite(Wrapped)
    case infinity
}

extension Modality {
    struct Range : Equatable, Sequence, CustomStringConvertible, CustomLoggingStringConvertible, CustomDebugStringConvertible, IteratorProtocol {
        typealias Element = Modality
        
        var lowerBound: Modality
        var upperBound: Modality
        
        init(lowerBound: Modality, upperBound: Modality) {
            assert(!lowerBound.isInfinite)
            assert(lowerBound <= upperBound)
            
            self.lowerBound = lowerBound
            self.upperBound = upperBound
        }
        
        mutating func next() -> Modality? {
            guard lowerBound <= upperBound else {
                return nil
            }
            defer {
                lowerBound += 1
            }
            return lowerBound
        }
        
        func makeIterator() -> some IteratorProtocol<Element> {
            self
        }
        
        func loggingString<ST>() -> ST where ST : LoggingString {
            "[\(lowerBound),\(upperBound)\(upperBound.isInfinite ? ")" : "]")"
        }
        
        static func - (lhs: Self, rhs: Modality) -> Self? {
            if let newUpper = lhs.upperBound - rhs {
                if let newLower = lhs.lowerBound - rhs {
                    newLower...newUpper
                } else {
                    1...newUpper
                }
            } else {
                nil
            }
        }
    }
    
    static func ... (lhs: Self, rhs: Self) -> Range {
        .init(lowerBound: lhs, upperBound: rhs)
    }
    
    static postfix func ...(lhs: Self) -> Range {
        .init(lowerBound: lhs, upperBound: .infinity)
    }
    
    static func + (lhs: Self, rhs: Wrapped) -> Self {
        switch lhs {
        case let .finite(i):
            .finite(i + rhs)
        case .infinity:
            .infinity
        }
    }
    
    static func - (lhs: Self, rhs: Wrapped) -> Self? {
        switch lhs {
        case let .finite(i):
            if i <= rhs {
                nil
            } else {
                .finite(i - rhs)
            }
        case .infinity:
            .infinity
        }
    }
    
    static func - (lhs: Self, rhs: Self) -> Self? {
        assert(!rhs.isInfinite)
        
        return switch (lhs, rhs) {
        case let (.finite(i1), .finite(i2)):
            if i1 <= i2 {
                nil
            } else {
                .finite(i1 - i2)
            }
        case _:
            .infinity
        }
    }
    
    static func += (lhs: inout Self, rhs: Wrapped) {
        lhs = lhs + rhs
    }

    var isInfinite: Bool {
        if case .infinity = self {
            true
        } else {
            false
        }
    }
}

extension Modality : Equatable {
    static func == (lhs: Self, rhs: Wrapped) -> Bool {
        switch lhs {
        case let .finite(i):
            i == rhs
        case .infinity:
            false
        }
    }
}

extension Modality : Comparable {
    static func < (lhs: Self, rhs: Self) -> Bool {
        switch (lhs, rhs) {
        case let (.finite(i1), .finite(i2)):
            i1 < i2
        case (.finite, .infinity):
            true
        case (.infinity, _):
            false
        }
    }
}

extension Modality : ExpressibleByIntegerLiteral {
    init(integerLiteral value: Wrapped) {
        assert(value > 0)
        self = .finite(value)
    }
}

extension Modality : CustomStringConvertible {
    var description: String {
        switch self {
        case .finite(let wrapped):
            "\(wrapped)"
        case .infinity:
            "∞"
        }
    }
}

extension Modality : CustomDebugStringConvertible, CustomDebugStringConvertibleEnum {
    func loggingString<ST>() -> ST where ST : LoggingString {
        switch self {
        case .finite(let wrapped):
            "finite(\(wrapped))"
        case .infinity:
            "infinity"
        }
    }
}
