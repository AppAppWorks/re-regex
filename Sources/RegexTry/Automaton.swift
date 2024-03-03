//
//  Automaton.swift
//  
//
//  Created by Lau Chun Kai on 27/2/2024.
//

struct Automaton<GS : Graphemes, GP : GraphemePattern> where GS.Element == GP.Element {
    typealias G = GS.Element
    
    let groupCount: Int
    let operation: Operation
    
    init(groupCount: Int, _ operations: Operation...) {
        self.init(groupCount: groupCount, operations: operations)
    }
    
    init(groupCount: Int, operations: [Operation]) {
        assert(!operations.isEmpty)
        
        self.groupCount = groupCount
        operation = .group(.init(operations: operations), nil)
    }
    
    func process(text: String) -> [[Range<GS.Index>?]] {
        let graphemes = GS.convertToGraphemes(s: text)
        var stream = graphemes.dropFirst(0)
        var groups = Result.Groups(repeating: nil, count: groupCount + 1)
                
        var matches = [Result.Groups]()
        
        while !stream.isEmpty {
            var operation = operation
            
            switch operation.process(stream: stream, groups: &groups) {
            case let .success(newStream):
                groups[0] = stream.startIndex..<newStream.startIndex
                matches.append(groups)
            case .failed:
                break
            }
            
            (_, stream) = stream.dropFirst()
        }
        
        return matches
    }
}

extension Automaton {
    typealias Stream = GS.SubSequence
    
    enum Result {
        typealias Groups = [Range<GS.Index>?]
        
        case failed
        case success(_ stream: Stream)
        
        var isSuccess: Bool {
            if case .success = self {
                true
            } else {
                false
            }
        }
    }
    
    struct Grouper : Sequencer {
        typealias I = IndexingIterator<[Operation]>
        
        let operations: [Operation]
        
        var isCompleted = false
        
        var timeMachine = TimeMachine()
        
        var canReuse: Bool {
            !isCompleted || !timeMachine.isEmpty
        }
        
        func followers() -> I {
            operations.makeIterator()
        }
        
        mutating func complete() {
            isCompleted = true
        }
    }
    
    struct Repeater : Sequencer {
        typealias I = Iterator
        
        let child: Operation
        let times: Times
        let isGreedy: Bool
        
        struct Iterator : IteratorProtocol, Greediness {
            typealias Element = Operation
            
            var times: Times
            let child: Operation
            
            var isAtomic: Bool {
                switch times {
                case let .exact(_, atomic):
                    atomic
                case let .atLeast(times) where times > 0:
                    true
                case let .range(_, _, atomic):
                    atomic
                case _:
                    false
                }
            }
            
            var hasNext: Bool {
                times.hasNext
            }
            
            mutating func next() -> Element? {
                guard times.hasNext else {
                    return nil
                }
                
                let (next, yielded) = times.matching()
                times = next
                return yielded ? child : .noOp
            }
        }
        
        enum Times : Equatable {
            case none
            case exact(_ times: Int, _ atomic: Bool)
            case atLeast(_ times: Int)
            case atMost(_ times: Int)
            case range(_ lower: Int, _ upper: Int, _ atomic: Bool)
            
            func matching() -> (next: Self, yielded: Bool) {
                switch self {
                case .none:
                    (.none, false)
                case let .exact(times, atomic):
                    (times == 1 ? .none : .exact(times - 1, atomic), true)
                case let .atLeast(times):
                    times == 0 ? (.atLeast(1), false) : (.atLeast(times - 1), true)
                case let .atMost(times):
                    (times == 1 ? .exact(1, false) : .range(1, times, false), false)
                case let .range(lower, upper, atomic):
                    if lower == 1 {
                        (upper == 2 ? .exact(1, false) : .range(1, upper - 1, false), true)
                    } else {
                        (.range(lower - 1, upper - 1, atomic), true)
                    }
                case _:
                    (.none, false)
                }
            }
            
            var hasNext: Bool {
                if case .none = self {
                    false
                } else {
                    true
                }
            }
        }
        
        var isCompleted = false
        
        var timeMachine = TimeMachine()
        
        var canReuse: Bool {
            !isCompleted || !timeMachine.isEmpty
        }
        
        func followers() -> I {
            .init(times: times, child: child)
        }
        
        mutating func complete() {
            isCompleted = true
        }
    }
        
    indirect enum Operation {
        case start
        case end
        case graphemes(_ pattern: GP)
        case match(_ filter: (G) -> Bool)
        case notMatch(_ filter: (G) -> Bool)
        case or(_ branches: [Operation])
        case group(_ grouper: Grouper, _ order: Int?)
        case repeater(Repeater)
        case noOp
        case none
        
        init(string: String) {
            self = .graphemes(GP(string))
        }
        
        var isNone: Bool {
            if case .none = self {
                true
            } else {
                false
            }
        }
        
        mutating func process(stream: Stream, groups: inout Result.Groups) -> Result {
            guard !stream.isEmpty else {
                return .failed
            }
            
            let result: Result
            switch self {
            case .noOp:
                result = .success(stream)
            case .none:
                return .failed
            case .start:
                result = stream.offset == 0 ? .success(stream) : .failed
            case .end:
                result = stream.isEmpty ? .success(stream) : .failed
            case .graphemes(let pattern):
                result = stream.starts(with: pattern) ? .success(stream.dropFirst(pattern.underestimatedCount)) : .failed
            case .match(let filter):
                let (first, subStream) = stream.dropFirst()
                result = filter(first) ? .success(subStream) : .failed
            case .notMatch(let filter):
                let (first, subStream) = stream.dropFirst()
                result = filter(first) ? .failed : .success(subStream)
            case .or(let branches):
                return handleOr(branches: branches, stream: stream, groups: &groups)
            case let .group(grouper, order):
                return handleGroup(grouper: grouper, order: order, stream: stream, groups: &groups)
            case let .repeater(repeater):
                return handleRepeater(repeater: repeater, stream: stream, groups: &groups)
            }
            
            self = .none
            return result
        }
        
        mutating func handleRepeater(repeater: Repeater, stream: Stream, groups: inout Result.Groups) -> Result {
            var repeater = repeater
            defer {
                self = repeater.canReuse ? .repeater(repeater) : .none
            }
            
            let result = repeater.process(stream: stream, groups: &groups)
            self = .repeater(repeater)
            return result
        }
        
        mutating func handleGroup(grouper: Grouper, order: Int?, stream: Stream, groups: inout Result.Groups) -> Result {
            var grouper = grouper
            
            defer {
                self = grouper.canReuse ? .group(grouper, order) : .none
            }
            
            switch grouper.process(stream: stream, groups: &groups) {
            case let .success(newStream):
                if let order {
                    groups[order] = stream.startIndex..<newStream.startIndex
                }
                return .success(newStream)
            case .failed:
                return .failed
            }
        }
        
        mutating func handleOr(branches: [Operation], stream: Stream, groups: inout Result.Groups) -> Result {
            var branches = branches
            var newBranches = branches.dropFirst(0)
            
            while !newBranches.isEmpty {
                var branch = newBranches.removeFirst()
                
                repeat {
                    let result = branch.process(stream: stream, groups: &groups)
                                                            
                    if case .success = result {
                        if case .none = branch {
                            if newBranches.isEmpty {
                                self = .none
                            } else {
                                self = .or(Array(newBranches))
                            }
                        } else {
                            branches[0] = branch
                            self = .or(branches)
                        }
                        
                        return result
                    } else if case .none = branch {
                        break
                    }
                } while true
            }
            
            self = .none
            
            return .failed
        }
    }
}

protocol Sequencer<I> {
    associatedtype GS : Graphemes
    associatedtype GP : GraphemePattern where GP.Element == GS.Element, GP.Element : Grapheme
    typealias G = GP.Element
    
    typealias A = Automaton<GS, GP>
    typealias Operation = A.Operation
    
    associatedtype I : IteratorProtocol<Operation> & Greediness
    typealias Result = A.Result
    typealias Stream = A.Stream
    typealias Groups = Result.Groups
     
    
    typealias TimeMachine = [(multiModal: Operation, followers: I?, progress: Stream, groups: Groups)]
    
    func followers() -> I
    
    var canReuse: Bool { get }
    
    var timeMachine: TimeMachine { get set }
    
    mutating func complete()
}

extension Sequencer {
    mutating func processVariant(_ variant: inout Operation, progress: Stream, groups: inout Groups) -> SequencerVariantProgress<GS> {
        let oldGroups = groups
        
        switch variant.process(stream: progress, groups: &groups) {
        case let .success(newStream):
            if !variant.isNone {
                timeMachine.append((variant, nil, progress, oldGroups))
            }
            
            return .completed(newStream)
        case .failed:
            return .failed
        }
    }
    
    mutating func processVariant(_ variant: inout Operation, progress: Stream, followers: inout I, groups: inout Groups) -> SequencerVariantProgress<GS> {
        let oldGroups = groups
        
        switch variant.process(stream: progress, groups: &groups) {
        case let .success(newStream):
            if !variant.isNone {
                timeMachine.append((variant, followers, progress, oldGroups))
            }
            
            let isAtomic = followers.isAtomic            
            if let next = followers.next() {
                if !isAtomic {
                    timeMachine.insert((.noOp, nil, newStream, groups), at: timeMachine.count - 1)
                }
                
                timeMachine.append((next, followers, newStream, groups))
                return .inProgress
            } else {
                return .completed(newStream)
            }
        case .failed:
            return .failed
        }
    }
    
    mutating func process(operation: inout Operation, progress: inout Stream, followers: inout I, groups: inout Groups) -> SequencerProgress<GS> {
        let oldGroups = groups
        
        switch operation.process(stream: progress, groups: &groups) {
        case let .success(newStream):
            if !followers.isAtomic && followers.hasNext {
                timeMachine.append((.noOp, nil, newStream, groups))
            }
            
            if !operation.isNone {
                timeMachine.append((operation, followers, progress, oldGroups))
                                        
                guard let nextOperation = followers.next() else {
                    return .completed(newStream)
                }
                timeMachine.append((nextOperation, followers, newStream, groups))
                
                return .inProgress(variantMet: true)
            } else {
                progress = newStream
                return .inProgress(variantMet: false)
            }
        case .failed:
            return .failed
        }
    }
    
    mutating func processStack(groups: inout Groups) -> Result {
        assert(!timeMachine.isEmpty)

        while !timeMachine.isEmpty {
            var (operation, followers, progress, oldGroups) = timeMachine.removeLast()
                                    
            if var followers {
                switch processVariant(&operation, progress: progress, followers: &followers, groups: &oldGroups) {
                case let .completed(newStream):
                    groups = oldGroups
                    return .success(newStream)
                case .failed:
                    while !operation.isNone && !progress.isEmpty {
                        switch processVariant(&operation, progress: progress, followers: &followers, groups: &oldGroups) {
                        case let .completed(newStream):
                            groups = oldGroups
                            return .success(newStream)
                        case .failed:
                            continue
                        case .inProgress:
                            break
                        }
                    }
                case .inProgress:
                    continue
                }
            } else {
                switch processVariant(&operation, progress: progress, groups: &oldGroups) {
                case let .completed(newStream):
                    groups = oldGroups
                    return .success(newStream)
                case .failed:
                    while !operation.isNone && !progress.isEmpty {
                        switch processVariant(&operation, progress: progress, groups: &oldGroups) {
                        case let .completed(newStream):
                            groups = oldGroups
                            return .success(newStream)
                        case .failed:
                            continue
                        case .inProgress:
                            break
                        }
                    }
                case .inProgress:
                    continue
                }
            }
        }
                
        return .failed
    }
    
    mutating func process(stream: Stream, groups: inout Groups) -> Result {
        guard canReuse else {
            return .failed
        }
        
        defer {
            complete()
        }
        
        guard timeMachine.isEmpty else {
            return processStack(groups: &groups)
        }
        
        var stream = stream
        var followers = self.followers()
        
    main:
        while var operation = followers.next() {
            switch process(operation: &operation, progress: &stream, followers: &followers, groups: &groups) {
            case let .completed(newStream):
                complete()
                return .success(newStream)
            case .failed:
                if operation.isNone {
                    complete()
                    return .failed
                } else {
                    while !operation.isNone && !stream.isEmpty {
                        switch process(operation: &operation, progress: &stream, followers: &followers, groups: &groups) {
                        case let .completed(newStream):
                            return .success(newStream)
                        case .failed:
                            continue
                        case let .inProgress(variantMet):
                            if variantMet {
                                return processStack(groups: &groups)
                            } else {
                                continue main
                            }
                        }
                    }
                    
                    return .failed
                }
            case let .inProgress(variantMet):
                if variantMet {
                    return processStack(groups: &groups)
                } else {
                    continue
                }
            }
        }
        
        return .success(stream)
    }
}

protocol Greediness {
    var hasNext: Bool { get }
    var isAtomic: Bool { get }
}

enum SequencerVariantProgress<GS : Graphemes> {
    case inProgress
    case completed(GS.SubSequence)
    case failed
}

enum SequencerProgress<GS : Graphemes> {
    case inProgress(variantMet: Bool)
    case completed(GS.SubSequence)
    case failed
}

extension IndexingIterator : Greediness {
    var hasNext: Bool {
        true
    }
    
    var isAtomic: Bool {
        true
    }
}
