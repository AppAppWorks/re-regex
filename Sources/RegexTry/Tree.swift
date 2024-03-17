//
//  File.swift
//  
//
//  Created by Lau Chun Kai on 11/3/2024.
//

extension Automaton {
    indirect enum Tree {
        typealias Result = (GS.SubSequence, Self?)
        
        enum Pure {
            case start
            case end
            case pattern(_ pattern: GP)
            case match(_ filter: (G) -> Bool)
            case notMatch(_ filter: (G) -> Bool)
            
            init(string: String) {
                self = .pattern(GP(string))
            }
            
            func process(stream: Stream) -> GS.SubSequence? {
                switch self {
                case .start:
                    return stream.offset == 0 ? stream : nil
                case .end:
                    return stream.isEmpty ? stream : nil
                case .pattern(let pattern):
                    return stream.starts(with: pattern) ? stream.dropFirst(pattern.underestimatedCount) : nil
                case .match(let filter):
                    let (first, subStream) = stream.dropFirst()
                    return filter(first) ? subStream : nil
                case .notMatch(let filter):
                    let (first, subStream) = stream.dropFirst()
                    return filter(first) ? subStream : nil
                }
            }
        }
        
        case pure(Pure)
        case finite(members: [Self])
        case mutatedFinite(stack: Stack, members: [Self])
        case or(branches: [Self])
        case triedOptional(GS.SubSequence)
        case optional(Tree, isGreedy: Bool)
        case newRepeat(repeated: Self, range: Modality.Range, isGreedy: Bool)
        case newRetryRepeat(repeated: Self, stack: RetryStack, regurgitation: Regurgitation)
        case newRetryNonGreedyRepeat(repeated: Self, max: (Modality, GS.SubSequence)?, stack: RetryStack, regurgitation: NonGreedyRegurgitation)
    }
}

extension Automaton.Tree.Pure : Equatable {
    static func == (lhs: Self, rhs: Self) -> Bool {
        switch (lhs, rhs) {
        case let (.pattern(g1), .pattern(g2)):
            g1 == g2
        case _:
            false
        }
    }
}

extension Automaton.Tree.Pure : CustomStringConvertible, CustomDebugStringConvertible, CustomDebugStringConvertibleEnum {
    func loggingString<ST>() -> ST where ST : LoggingString {
        switch self {
        case .start:
            "start"
        case .end:
            "end"
        case .pattern(let p):
            #""\#(p)""#
        case .match(let _):
            "match"
        case .notMatch(let _):
            "notMatch"
        }
    }
}

extension Automaton.Tree : Equatable {
    static func == (lhs: Self, rhs: Self) -> Bool {
        switch (lhs, rhs) {
        case let (.pure(p1), .pure(p2)):
            p1 == p2
        case let (.finite(trees1), .finite(trees2)),
            let (.or(trees1), .or(trees2)):
            trees1 == trees2
        case let (.mutatedFinite(stack1, members: members1), .mutatedFinite(stack: stack2, members: members2)):
            stack1 == stack2 && members1 == members2
        case let (.optional(tree1, isGreedy1), .optional(tree2, isGreedy2)):
            tree1 == tree2 && isGreedy1 == isGreedy2
        case let (.triedOptional(gs1), .triedOptional(gs2)):
            gs1.offset == gs2.offset
        case let (.newRepeat(repeated1, range1, isGreedy1), .newRepeat(repeated2, range2, isGreedy2)):
            repeated1 == repeated2 && range1 == range2 && isGreedy1 == isGreedy2
        case let (.newRetryRepeat(repeated1, stack1, regurgitation1), .newRetryRepeat(repeated2, stack2, regurgitation2)):
            repeated1 == repeated2 && stack1 == stack2 && regurgitation1 == regurgitation2
        case let (.newRetryNonGreedyRepeat(repeated1, max1, stack1, regurgitation1), .newRetryNonGreedyRepeat(repeated2, max2, stack2, regurgitation2)):
            if let max1, let max2 {
                repeated1 == repeated2 && max1.0 == max2.0 && max1.1.offset == max2.1.offset && stack1 == stack2 && regurgitation1 == regurgitation2
            } else if max1 == nil, max2 == nil {
                repeated1 == repeated2 && stack1 == stack2 && regurgitation1 == regurgitation2
            } else {
                false
            }
        case _:
            false
        }
    }
}



typealias DFSStack<GS, GP> = [(mutated: Automaton<GS, GP>.Tree, memberIdx: Int, workOn: GS.SubSequence, lastSuccess: GS.SubSequence)] where GS : Graphemes, GP : GraphemePattern, GS.Element == GP.Element

func == <GS, GP>(lhs: DFSStack<GS, GP>, rhs: DFSStack<GS, GP>) -> Bool {
    guard lhs.count == rhs.count else {
        return false
    }
    return zip(lhs, rhs).allSatisfy { e1, e2 in
        e1.mutated == e2.mutated && e1.memberIdx == e2.memberIdx && e1.workOn.offset == e2.workOn.offset && e1.lastSuccess.offset == e2.lastSuccess.offset
    }
}

extension Automaton.Tree {
    static func or(_ ops: Pure...) -> Self {
        .or(branches: ops.map(pure))
    }
    
    static func or(_ branches: Self...) -> Self {
        .or(branches: branches)
    }
    
    typealias Stack = DFSStack<GS, GP>
    
    func traverse(text: String) {
        let graphemes = GS.convertToGraphemes(s: text)
        let gs = graphemes.dropFirst(0)
                
        if let (end, _) = traverse(startingAt: gs) {
            print(graphemes[gs.startIndex..<end.startIndex])
        }
    }
    
    func traverse(startingAt gs: GS.SubSequence) -> Result? {
        switch self {
        case let .optional(tree, isGreedy):
            tree.processOptional(at: gs, isGreedy: isGreedy)
        case let .triedOptional(workedOn):
            (workedOn, nil)
        case let .finite(members):
            members.processAsFinite(startingAt: gs)
        case let .mutatedFinite(stack, members):
            members.processAsMutatedFinite(fromStack: stack)
        case let .pure(op):
            if let progress = op.process(stream: gs) {
                (progress, nil)
            } else {
                nil
            }
        case let .or(branches):
            branches.processAsOr(at: gs)
        case let .newRepeat(repeated, range, isGreedy):
            if isGreedy {
                repeated.repeatGreedy(at: gs, range: range, retryStack: [], regurgitation: []).map { newWorkOn, continuation in
                    if let continuation {
                        (newWorkOn, .newRetryRepeat(repeated: repeated, stack: continuation.0, regurgitation: continuation.1))
                    } else {
                        (newWorkOn, nil)
                    }
                }
            } else {
                repeated.repeatNonGreedy(at: gs, range: range, retryStack: [], regurgitation: []).map { newWorkOn, continuation in
                    if let continuation {
                        (newWorkOn, .newRetryNonGreedyRepeat(repeated: repeated, max: continuation.max.map { ($0, newWorkOn) }, stack: continuation.stack, regurgitation: continuation.regurgitation))
                    } else {
                        (newWorkOn, nil)
                    }
                }
            }
        case let .newRetryRepeat(repeated, stack, regurgitation):
            if regurgitation.isEmpty {
                repeated.retryRepeatGreedyNotYet(retryStack: stack).map { workOn, continuation in
                    if let continuation {
                        if continuation.0.isEmpty && continuation.1.isEmpty {
                            fatalError("impossible")
                        } else {
                            (workOn, .newRetryRepeat(repeated: repeated, stack: continuation.0, regurgitation: continuation.1))
                        }
                    } else {
                        (workOn, nil)
                    }
                }
            } else {
                repeated.retryRepeatGreedyMin(retryStack: stack, regurgitation: regurgitation)
            }
        case let .newRetryNonGreedyRepeat(repeated, max, stack, regurgitation):
            repeated.retryRepeatNonGreedyMin(at: gs, maxAndLast: max, retryStack: stack, regurgitation: regurgitation)
        }
    }
    
    enum EagerRepeat {
        case completed(Automaton.Tree?)
        case progressed(GS.SubSequence, Automaton.Tree?)
        case noProgress
        case notMatched
    }
    
    // for all greedy repeat and non-first non-greedy repeat
    func traverseForEagerRepeat(startingAt gs: GS.SubSequence, previousEnd: GS.SubSequence, allowsEnd: Bool) -> EagerRepeat {
        var mutated = self
        var matched = false
        
        repeat {
            if let (newWorkOn, newMutated) = mutated.traverse(startingAt: gs) {
                if newWorkOn.offset != gs.offset && newWorkOn.offset != previousEnd.offset {
                    if newWorkOn.isEmpty {
                        if allowsEnd {
                            return .completed(newMutated)
                        }
                    } else {
                        return .progressed(newWorkOn, newMutated)
                    }
                }
                
                if let newMutated {
                    matched = true
                    mutated = newMutated
                } else {
                    return .noProgress
                }
            } else {
                return if matched {
                    .noProgress
                } else {
                    .notMatched
                }
            }
        } while true
    }
    
    func processOptional(at gs: GS.SubSequence, isGreedy: Bool) -> Result {
        if isGreedy {
            switch traverseForEagerRepeat(startingAt: gs, previousEnd: gs, allowsEnd: true) {
            case let .completed(mutated):
                if let mutated {
                    (gs.eot, .optional(mutated, isGreedy: true))
                } else {
                    (gs.eot, .triedOptional(gs))
                }
            case let .progressed(newWorkOn, mutated):
                if let mutated {
                    (newWorkOn, .optional(mutated, isGreedy: true))
                } else {
                    (newWorkOn, .triedOptional(gs))
                }
            case .noProgress, .notMatched:
                (gs, nil)
            }
        } else {
            (gs, self)
        }
    }
    
    func mutateTillDifferent(workOn: GS.SubSequence, lastSuccess: GS.SubSequence) -> Result? {
        repeat {
            if let (success, mutated) = traverse(startingAt: workOn) {
                if success.offset != lastSuccess.offset {
                    return (success, mutated)
                } else if mutated != nil {
                    continue
                }
            }
            return nil
        } while true
    }        
}

extension Automaton.Tree : CustomStringConvertible, CustomDebugStringConvertible, CustomLoggingStringConvertibleStruct {
    func loggingString<ST>() -> ST where ST : LoggingString {
        switch self {
        case .pure(let operation):
            "\(operation)"
        case .finite(let members):
            "finite\(members)"
        case .mutatedFinite(let stack, let members):
            "mutatedFinite(stack: \(stack), members: \(members))"
        case .or(let branches):
            "or(\(seq: branches, separator: "|"))"
        case .triedOptional(let subSequence):
            "triedOptional(\(subSequence))"
        case .optional(let tree, let isGreedy):
            "optional\(isGreedy ? "Greedy" : "NonGreedy")(\(tree))"
        case let .newRepeat(repeated, range, isGreedy):
            "repeat \(repeated) \(isGreedy ? "greedily " : "")for \(range)"
        case let .newRetryRepeat(repeated, stack, regurgitation):
            "retry repeat \(repeated) greedily, stack: [\(seq: stack, separator: ", ")], regurgitation: [\(seq: regurgitation, separator: ", ")]"
        case let .newRetryNonGreedyRepeat(repeated, max, stack, regurgitation):
            "retry repeat \(repeated) non-greedily for \(tuple: max), stack: [\(seq: stack, separator: ", ")], regurgitation: [\(seq: regurgitation, separator: ", ")]"
        }
    }
}

extension Automaton.Tree {
    struct Replay : Equatable {
        var alternative: Automaton.Tree
        var startingAt: GS.SubSequence
        
        func replay(max: Modality?) -> Retried<Modality> {
            var alternative = alternative
            repeat {
                if let (success, mutated) = alternative.traverse(startingAt: startingAt) {
                    if !success.isEmpty {
                        return if let mutated {
                            .retry(.init(replay: .init(alternative: mutated, startingAt: startingAt),
                                         progress: .init(context: max, previousEnd: success)))
                        } else {
                            .progress(.init(context: max, previousEnd: success))
                        }
                    } else if let mutated {
                        alternative = mutated
                        continue
                    }
                }
                return .failed
            } while true
        }
        
        static func == (lhs: Self, rhs: Self) -> Bool {
            lhs.alternative == rhs.alternative && lhs.startingAt.offset == rhs.startingAt.offset
        }
    }
    
    struct Progress<Context : Equatable> : Equatable, CustomStringConvertible, CustomDebugStringConvertible, CustomLoggingStringConvertibleStruct {
        func loggingString<ST>() -> ST where ST : LoggingString {
            if let context {
                if let cdsc = context as? any CustomDebugStringConvertible {
                    "\(previousEnd) and the repeat context is \(cdsc)"
                } else {
                    "\(previousEnd) and the repeat context is \(context)"
                }
            } else {
                "\(previousEnd) and no more repeat"
            }
        }
        
        var context: Context?
        var previousEnd: GS.SubSequence
        
        static func == (lhs: Self, rhs: Self) -> Bool {
            lhs.previousEnd.offset == rhs.previousEnd.offset && lhs.context == rhs.context
        }
    }
    
    struct Retry<Context : Equatable> : Equatable, CustomStringConvertible, CustomDebugStringConvertible, CustomLoggingStringConvertibleStruct {
        func loggingString<ST>() -> ST where ST : LoggingString {
            "will try with \(replay.alternative) at \(replay.startingAt) not to progress to \(progress)"
        }
        
        var replay: Replay
        var progress: Progress<Context>
        
        func retry() -> Retried<Context> {
            var alternative = replay.alternative
            repeat {
                if let (success, mutated) = alternative.traverse(startingAt: replay.startingAt) {
                    if success.offset != progress.previousEnd.offset {
                        var progress = progress
                        progress.previousEnd = success
                        
                        return if let mutated {
                            .retry(.init(replay: .init(alternative: mutated, startingAt: replay.startingAt),
                                         progress: progress))
                        } else {
                            .progress(progress)
                        }
                    } else if let mutated {
                        alternative = mutated
                        continue
                    }
                }
                return .failed
            } while true
        }
    }
    
    enum Retried<Context : Equatable> : CustomStringConvertible, CustomDebugStringConvertible, CustomLoggingStringConvertibleStruct {
        case retry(Retry<Context>)
        case progress(Progress<Context>)
        case failed
        
        func loggingString<ST>() -> ST where ST : LoggingString {
            switch self {
            case let .retry(retry):
                "The last retry produced a new retry that \(retry)"
            case let .progress(progress):
                "The last retry was exhausted and produced a new \(progress)"
            case .failed:
                "The last retry failed to produce a different progress"
            }
        }
    }
    
    enum Regur : Equatable, CustomStringConvertible, CustomDebugStringConvertible, CustomLoggingStringConvertibleStruct {
        case preEot(Replay, max: Modality?)
        case retry(Retry<Modality>)
        case progress(GS.SubSequence)
        
        func loggingString<ST>() -> ST where ST : LoggingString {
            switch self {
            case let .preEot(replay, max):
                if let max {
                    "will try with \(replay.alternative) at \(replay.startingAt) until not yielding an eot and the next max is \(max)"
                } else {
                    "will try with \(replay.alternative) at \(replay.startingAt) until not yielding an eot"
                }
            case let .retry(retry):
                "\(retry)"
            case let .progress(progress):
                "will end the pattern at \(progress)"
            }
        }
        
        static func == (lhs: Self, rhs: Self) -> Bool {
            switch (lhs, rhs) {
            case let (.preEot(replay1, max1), .preEot(replay2, max2)):
                replay1 == replay2 && max1 == max2
            case let (.retry(retry1), .retry(retry2)):
                retry1 == retry2
            case let (.progress(gs1), .progress(gs2)):
                gs1.offset == gs2.offset
            case _:
                false
            }
        }
    }
    
    enum NonGreedyRegur : Equatable, CustomStringConvertible, CustomDebugStringConvertible, CustomLoggingStringConvertibleStruct {
        case preEot(Replay, max: Modality?)
        case retry(Retry<Modality>)
        
        func loggingString<ST>() -> ST where ST : LoggingString {
            switch self {
            case let .preEot(replay, max):
                if let max {
                    "will try with \(replay.alternative) at \(replay.startingAt) until not yielding an eot and the next max is \(max)"
                } else {
                    "will try with \(replay.alternative) at \(replay.startingAt) until not yielding an eot"
                }
            case let .retry(retry):
                "\(retry)"
            }
        }
    }
    
    typealias RetryStack = [Retry<Modality.Range>]
    typealias Regurgitation = [Regur]
    typealias NonGreedyRegurgitation = [NonGreedyRegur]
    
    func repeatGreedy(at gs: GS.SubSequence, range: Modality.Range, retryStack: RetryStack, regurgitation: Regurgitation) -> (GS.SubSequence, (RetryStack, Regurgitation)?)? {
        if range.lowerBound > 1 {
            assert(regurgitation.isEmpty)
            
            return if retryStack.isEmpty {
                repeatGreedyFresh(at: gs, range: range)
            } else {
                repeatGreedyNotYet(at: gs, range: range, retryStack: retryStack)
            }
        } else {
            return repeatGreedyMin(at: gs, max: range.upperBound, retryStack: retryStack, regurgitation: regurgitation)
        }
    }
    
    func repeatGreedyFresh(at gs: GS.SubSequence, range: Modality.Range) -> (GS.SubSequence, (RetryStack, Regurgitation)?)? {
        var workOn = gs
        
        for i in 1...range.lowerBound {
            switch traverseForEagerRepeat(startingAt: workOn, previousEnd: workOn, allowsEnd: i == range.lowerBound) {
            case let .completed(mutated):
                if let mutated {
                    let newRange = range - i
                    let end = workOn.eot
                    return (end, ([.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: newRange, previousEnd: end))], []))
                } else {
                    return (workOn.eot, nil)
                }
            case let .progressed(newWorkOn, mutated):
                if let newRange = range - i {
                    if let mutated {
                        return repeatGreedy(at: newWorkOn, range: newRange, retryStack: [.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: newRange, previousEnd: newWorkOn))], regurgitation: [])
                    }
                    workOn = newWorkOn
                } else {
                    return if let mutated {
                        (newWorkOn, ([.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: nil, previousEnd: newWorkOn))], []))
                    } else {
                        (newWorkOn, nil)
                    }
                }
            case .noProgress:
                // short-circuit the repeat, because all future repeated will have no progress too
                return (workOn, nil)
            case .notMatched:
                return nil
            }
        }
        
        fatalError("impossible")
    }
    
    func retryRepeatGreedyNotYet(retryStack: RetryStack) -> (GS.SubSequence, (RetryStack, Regurgitation)?)? {
        var retryStack = retryStack
        
        while !retryStack.isEmpty {
            let retry = retryStack.removeLast()
            
            let newWorkOn: GS.SubSequence
            switch retry.retry() {
            case let .retry(retry):
                retryStack.append(retry)
                newWorkOn = retry.progress.previousEnd
            case let .progress(progress):
                newWorkOn = progress.previousEnd
            case .failed:
                continue
            }
            
            return if let range = retry.progress.context {
                repeatGreedy(at: newWorkOn, range: range, retryStack: retryStack, regurgitation: [])
            } else {
                (newWorkOn, (retryStack, []))
            }
        }
        
        return nil
    }
    
    func repeatGreedyNotYet(at gs: GS.SubSequence, range: Modality.Range, retryStack: RetryStack) -> (GS.SubSequence, (RetryStack, Regurgitation)?)? {
        assert(!retryStack.isEmpty)
        
        var workOn = gs
        var retryStack = retryStack
        
        for i in 1...range.lowerBound {
            switch traverseForEagerRepeat(startingAt: workOn, previousEnd: workOn, allowsEnd: i == range.lowerBound) {
            case let .completed(mutated):
                let end = workOn.eot
                
                if let mutated {
                    let newRange = range - i
                    retryStack.append(.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: newRange, previousEnd: end)))
                }
                
                return (end, (retryStack, []))
            case let .progressed(newWorkOn, mutated):
                if let newRange = range - i {
                    if let mutated {
                        retryStack.append(.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: newRange, previousEnd: newWorkOn)))
                    }
                    if newRange.lowerBound == 1 {
                        return repeatGreedyMin(at: newWorkOn, max: newRange.upperBound, retryStack: retryStack, regurgitation: [])
                    }
                    workOn = newWorkOn
                } else {
                    if let mutated {
                        retryStack.append(.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: nil, previousEnd: newWorkOn)))
                    }
                    return (newWorkOn, (retryStack, []))
                }
            case .noProgress:
                // short-circuit the repeat, because all future repeated will have no progress too
                return (workOn, (retryStack, []))
            case .notMatched:
                // rewind begins
                return retryRepeatGreedyNotYet(retryStack: retryStack)
            }
        }
        
        fatalError("impossible")
    }
    
    func repeatGreedyMin(at gs: GS.SubSequence, max: Modality, retryStack: RetryStack, regurgitation: Regurgitation) -> (GS.SubSequence, (RetryStack, Regurgitation)?)? {
        var workOn = gs
        var regurgitation = regurgitation
        
        for i in 1...max {
            switch traverseForEagerRepeat(startingAt: workOn, previousEnd: workOn, allowsEnd: true) {
            case let .completed(mutated):
                let newMax = max - i
                
                if let mutated {
                    regurgitation.append(.preEot(.init(alternative: mutated, startingAt: workOn), max: newMax))
                }
                                         
                return (workOn.eot, (retryStack, regurgitation))
            case let .progressed(newWorkOn, mutated):
                let newMax = max - i
                
                if let mutated {
                    regurgitation.append(.retry(.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: newMax, previousEnd: newWorkOn))))
                }
                regurgitation.append(.progress(newWorkOn))
                
                workOn = newWorkOn
            case .noProgress:
                // short-circuit the repeat, because all future repeated will have no progress too
                return (workOn, (retryStack, regurgitation))
            case .notMatched:
                // previous success to fail over
                return if !regurgitation.isEmpty {
                    if let (newWorkOn, newRegurgitation) = retryRepeatGreedyMin(regurgitation: regurgitation) {
                        (newWorkOn, (retryStack, newRegurgitation))
                    } else {
                        nil
                    }
                } else { // see if there're previous alternatives before fulfilling the min that can help
                    retryRepeatGreedyNotYet(retryStack: retryStack)
                }
            }
        }
        
        return (workOn, (retryStack, regurgitation))
    }
    
    func retryRepeatGreedyMin(retryStack: RetryStack, regurgitation: Regurgitation) -> (GS.SubSequence, Automaton.Tree)? {
        if let (newWorkOn, newRegurgitation) = retryRepeatGreedyMin(regurgitation: regurgitation) {
            (newWorkOn, .newRetryRepeat(repeated: self, stack: retryStack, regurgitation: newRegurgitation))
        } else {
            nil
        }
    }
    
    func retryRepeatGreedyMin(at workOn: GS.SubSequence, max: Modality, regurgitation: Regurgitation) -> (GS.SubSequence, Regurgitation)? {
        var workOn = workOn
        var regurgitation = regurgitation
        
        for i in 1...max {
            switch traverseForEagerRepeat(startingAt: workOn, previousEnd: workOn, allowsEnd: true) {
            case let .completed(mutated):
                let newMax = max - i
                
                if let mutated {
                    regurgitation.append(.preEot(.init(alternative: mutated, startingAt: workOn), max: newMax))
                }
                
                return (workOn.eot, regurgitation)
            case let .progressed(newWorkOn, mutated):
                let newMax = max - i
                
                if let mutated {
                    regurgitation.append(.retry(.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: newMax, previousEnd: newWorkOn))))
                }
                regurgitation.append(.progress(newWorkOn))
                
                workOn = newWorkOn
            case .noProgress:
                // short-circuit the repeat, because all future repeated will have no progress too
                return (workOn, regurgitation)
            case .notMatched:
                // previous success to fail over
                return if let (newWorkOn, newRegurgitation) = retryRepeatGreedyMin(regurgitation: regurgitation) {
                    (newWorkOn, newRegurgitation)
                } else {
                    nil
                }
            }
        }
        
        return (workOn, regurgitation)
    }
    
    func retryRepeatGreedyMin(regurgitation: Regurgitation) -> (GS.SubSequence, Regurgitation)? {
        assert(!regurgitation.isEmpty)
        var regurgitation = regurgitation
        
        let lastRegur = regurgitation.removeLast()
        
        switch lastRegur {
        case let .preEot(replay, max):
            switch replay.replay(max: max) {
            case let .retry(newRetry):
                regurgitation.append(.retry(newRetry))
                let newWorkOn = newRetry.progress.previousEnd
                
                if let max {
                    regurgitation.append(.progress(newWorkOn))
                    return retryRepeatGreedyMin(at: newWorkOn, max: max, regurgitation: regurgitation)
                } else {
                    return (newWorkOn, regurgitation)
                }
            case let .progress(progress):
                if let max {
                    regurgitation.append(.progress(progress.previousEnd))
                    return retryRepeatGreedyMin(at: progress.previousEnd, max: max, regurgitation: regurgitation)
                } else {
                    return (progress.previousEnd, regurgitation)
                }
            case .failed:
                return if regurgitation.isEmpty {
                    nil
                } else {
                    retryRepeatGreedyMin(regurgitation: regurgitation)
                }
            }
        case .retry(let retry):
            switch retry.retry() {
            case let .retry(newRetry):
                let workOn = newRetry.progress.previousEnd
                let max = newRetry.progress.context
                
                if workOn.isEmpty {
                    regurgitation.append(.preEot(newRetry.replay, max: max))
                    return (workOn, regurgitation)
                }
                
                regurgitation.append(.retry(newRetry))
                
                return if let max {
                    retryRepeatGreedyMin(at: workOn, max: max, regurgitation: regurgitation)
                } else {
                    (workOn, regurgitation)
                }
            case let .progress(progress):
                let workOn = progress.previousEnd
                
                if let max = progress.context, !workOn.isEmpty {
                    regurgitation.append(.progress(workOn))
                    return retryRepeatGreedyMin(at: workOn, max: max, regurgitation: regurgitation)
                } else {
                    return (workOn, regurgitation)
                }
            case .failed:
                return if regurgitation.isEmpty {
                    nil
                } else {
                    retryRepeatGreedyMin(regurgitation: regurgitation)
                }
            }
        case .progress(let progress):
            return (progress, regurgitation)
        }
    }
}

extension Collection {
    typealias Tree<GS, GP> = Automaton<GS, GP>.Tree where GS : Graphemes, GP : GraphemePattern, GS.Element == GP.Element
    
    func processAsOr<GS, GP>(at gs: GS.SubSequence) -> (GS.SubSequence, Element?)? where Element == Tree<GS, GP> {
        for (i, branch) in enumerated() {
            if let (newWorkOn, mutated) = branch.traverse(startingAt: gs) {
                let rest = dropFirst(i + 1)
                
                let next: Element? =
                if let mutated {
                    rest.isEmpty ? mutated : .or(branches: [mutated] + Array(rest))
                } else {
                    rest.isEmpty ? nil : rest.count == 1 ? rest.first! : .or(branches: Array(rest))
                }
                
                return (newWorkOn, next)
            }
        }
        return nil
    }
    
    func processAsFinite<GS, GP>(startingAt gs: GS.SubSequence) -> (GS.SubSequence, Element?)? where Element == Tree<GS, GP>, Index == Int {
        var growingMembers = DFSStack<GS, GP>()
        growingMembers.reserveCapacity(count)
        
        var workOn = gs
        
        for (i, member) in enumerated() {
            if let (newWorkOn, mutated) = member.traverse(startingAt: workOn) {
                if let mutated {
                    growingMembers.append((mutated, i + startIndex, workOn, newWorkOn))
                }
                
                workOn = newWorkOn
            } else if !growingMembers.isEmpty {
                return processAsMutatedFinite(fromStack: growingMembers)
            } else {
                return nil
            }
        }
        
        if !growingMembers.isEmpty {
            return (workOn, .mutatedFinite(stack: growingMembers, members: Array(self)))
        } else {
            return (workOn, nil)
        }
    }
    
    func processAsMutatedFinite<GS, GP>(fromStack dfsStack: DFSStack<GS, GP>) -> (GS.SubSequence, Element?)? where Element == Tree<GS, GP>, Index == Int {
        var dfsStack = dfsStack
        
    outer:
        while !dfsStack.isEmpty {
            let (previousMutated, oldI, previousWorkOn, previousSuccess) = dfsStack.removeLast()
            if var (workOn, mutated) = previousMutated.mutateTillDifferent(workOn: previousWorkOn, lastSuccess: previousSuccess) {
                if let mutated {
                    dfsStack.append((mutated, oldI, previousWorkOn, workOn))
                }
                
                let rest = self[(oldI + 1)..<endIndex]
                for (i, member) in rest.enumerated() {
                    if let (nextWorkOn, mutated) = member.traverse(startingAt: workOn) {
                        if let mutated {
                            dfsStack.append((mutated, i + rest.startIndex, workOn, nextWorkOn))
                        }
                        workOn = nextWorkOn
                    } else {
                        continue outer
                    }
                }
                
                if dfsStack.isEmpty {
                    return (workOn, nil)
                } else {
                    return (workOn, .mutatedFinite(stack: dfsStack, members: Array(self)))
                }
            }
        }
        
        return nil
    }
}


