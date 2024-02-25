//
//  Automaton.swift
//  
//
//  Created by Lau Chun Kai on 27/2/2024.
//

struct Automaton {
    let groupCount: Int
    let operation: Operation
    
    init(groupCount: Int, operations: Operation...) {
        assert(!operations.isEmpty)
        
        self.groupCount = groupCount
        operation = .group(.init(operations: operations), nil)
    }
    
    func process(text: String) -> [[Range<Int>?]] {
        let utf8s = Array(text.utf8)
        var bytes = utf8s.dropFirst(0)
        var groups = Result.Groups()
        var operation = operation
        
        var matches = [Result.Groups]()
        
        while !bytes.isEmpty {
            switch operation.process(byteStream: bytes, groups: &groups) {
            case let .success(newStream):
                groups[0] = bytes.startIndex..<newStream.startIndex
                matches.append(groups)
            case .failed:
                break
            }
            
            bytes.removeFirst()
        }
        
        return matches.map { match in
            var groups = [Range<Int>?](repeating: nil, count: groupCount + 1)
            for (key, value) in match {
                groups[key] = value
            }
            return groups
        }
    }
}

extension Automaton {
    enum Result {
        typealias Groups = [Int : Range<Int>]
        
        case failed
        case success(_ byteStream: ArraySlice<UInt8>)
        
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
        
        var operationStack = OperationStack()
        
        var canReuse: Bool {
            !isCompleted || !operationStack.isEmpty
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
        
        var operationStack = OperationStack()
        
        var canReuse: Bool {
            !isCompleted || !operationStack.isEmpty
        }
        
        func followers() -> I {
            .init(times: times, child: child)
        }
        
        mutating func complete() {
            isCompleted = true
        }
    }
        
    indirect enum Operation {
        case bytes(_ pattern: [UInt8])
        case match(_ filter: (UInt8) -> Bool)
        case notMatch(_ filter: (UInt8) -> Bool)
        case or(_ branches: [Operation])
        case group(_ grouper: Grouper, _ order: Int?)
        case repeater(Repeater)
        case noOp
        case none
        
        init(string: String) {
            self = .bytes(Array(string.utf8))
        }
        
        var isNone: Bool {
            if case .none = self {
                true
            } else {
                false
            }
        }
        
        mutating func process(byteStream: ArraySlice<UInt8>, groups: inout Result.Groups) -> Result {
            guard !byteStream.isEmpty else {
                return .failed
            }
            
            let result: Result
            switch self {
            case .noOp:
                result = .success(byteStream)
            case .none:
                return .failed
            case .bytes(let pattern):
                result = byteStream.starts(with: pattern) ? .success(byteStream.dropFirst(pattern.count)) : .failed
            case .match(let filter):
                result = filter(byteStream.first!) ? .success(byteStream.dropFirst()) : .failed
            case .notMatch(let filter):
                result = filter(byteStream.first!) ? .failed : .success(byteStream.dropFirst())
            case .or(var branches):
                var newBranches = branches.dropFirst(0)
                
                while !newBranches.isEmpty {
                    var branch = newBranches.removeFirst()
                    
                    repeat {
                        let result = branch.process(byteStream: byteStream, groups: &groups)
                                                                
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
            case .group(var grouper, let order):
                defer {
                    self = grouper.canReuse ? .group(grouper, order) : .none
                }
                
                switch grouper.process(byteStream: byteStream, groups: &groups) {
                case let .success(newStream):
                    if let order {
                        groups[order] = byteStream.startIndex..<newStream.startIndex
                    }
                    return .success(newStream)
                case .failed:
                    return .failed
                }
            case var .repeater(repeater):
                defer {
                    self = repeater.canReuse ? .repeater(repeater) : .none
                }
                
                let result = repeater.process(byteStream: byteStream, groups: &groups)
                self = .repeater(repeater)
                return result
            }
            
            self = .none
            return result
        }
    }
}

protocol Sequencer<I> {
    typealias Operation = Automaton.Operation
    typealias Result = Automaton.Result
    typealias Groups = Result.Groups
     
    associatedtype I : IteratorProtocol<Operation> & Greediness
    
    typealias OperationStack = [(multiModal: Operation, followers: I?, progress: ArraySlice<UInt8>)]
    
    func followers() -> I
    
    var canReuse: Bool { get }
    
    var operationStack: OperationStack { get set }
    
    mutating func complete()
}

extension Sequencer {
    mutating func processVariant(_ variant: inout Operation, progress: ArraySlice<UInt8>, groups: inout Groups) -> SequencerVariantProgress {
        switch variant.process(byteStream: progress, groups: &groups) {
        case let .success(newStream):
            if !variant.isNone {
                operationStack.append((variant, nil, progress))
            }
            
            return .completed(newStream)
        case .failed:
            return .failed
        }
    }
    
    mutating func processVariant(_ variant: inout Operation, progress: ArraySlice<UInt8>, followers: inout I, groups: inout Groups) -> SequencerVariantProgress {
        switch variant.process(byteStream: progress, groups: &groups) {
        case let .success(newStream):
            if !variant.isNone {
                operationStack.append((variant, followers, progress))
            }
            
            let isCompulsory = followers.isAtomic            
            if let next = followers.next() {
                if !isCompulsory {
                    operationStack.insert((.noOp, nil, newStream), at: operationStack.count - 1)
                }
                
                operationStack.append((next, followers, newStream))
                return .inProgress
            } else {
                return .completed(newStream)
            }
        case .failed:
            return .failed
        }
    }
    
    mutating func process(operation: inout Operation, progress: inout ArraySlice<UInt8>, followers: inout I, groups: inout Groups) -> SequencerProgress {
        switch operation.process(byteStream: progress, groups: &groups) {
        case let .success(newStream):
//            print(followers)
            if !followers.isAtomic && followers.hasNext {
                operationStack.append((.noOp, nil, newStream))
            }
            
            if !operation.isNone {
                operationStack.append((operation, followers, progress))
                                        
                guard let nextOperation = followers.next() else {
                    return .completed(newStream)
                }
                operationStack.append((nextOperation, followers, newStream))
                
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
        assert(!operationStack.isEmpty)

        while !operationStack.isEmpty {
            var (operation, followers, progress) = operationStack.removeLast()
                                    
            if var followers {
                switch processVariant(&operation, progress: progress, followers: &followers, groups: &groups) {
                case let .completed(newStream):
                    return .success(newStream)
                case .failed:
                    while !operation.isNone {
                        switch processVariant(&operation, progress: progress, followers: &followers, groups: &groups) {
                        case let .completed(newStream):
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
                switch processVariant(&operation, progress: progress, groups: &groups) {
                case let .completed(newStream):
                    return .success(newStream)
                case .failed:
                    while !operation.isNone {
                        switch processVariant(&operation, progress: progress, groups: &groups) {
                        case let .completed(newStream):
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
    
    mutating func process(byteStream: ArraySlice<UInt8>, groups: inout Groups) -> Result {
        guard canReuse else {
            return .failed
        }
        
        defer {
            complete()
        }
        
        guard operationStack.isEmpty else {
            return processStack(groups: &groups)
        }
        
        var byteStream = byteStream
        var followers = self.followers()
        
        while var operation = followers.next() {
            switch process(operation: &operation, progress: &byteStream, followers: &followers, groups: &groups) {
            case let .completed(newStream):
                complete()
                return .success(newStream)
            case .failed:
                if operation.isNone {
                    complete()
                    return .failed
                } else {
                    while !operation.isNone {
                        switch process(operation: &operation, progress: &byteStream, followers: &followers, groups: &groups) {
                        case let .completed(newStream):
                            return .success(newStream)
                        case .failed:
                            continue
                        case let .inProgress(variantMet):
                            if variantMet {
                                return processStack(groups: &groups)
                            } else {
                                break
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
        
        return .success(byteStream)
    }
}

protocol Greediness {
    var hasNext: Bool { get }
    var isAtomic: Bool { get }
}

enum SequencerVariantProgress {
    case inProgress
    case completed(ArraySlice<UInt8>)
    case failed
}

enum SequencerProgress {
    case inProgress(variantMet: Bool)
    case completed(ArraySlice<UInt8>)
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
