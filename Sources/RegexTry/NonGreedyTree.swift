//
//  NonNonGreedyTree.swift
//
//
//  Created by Lau Chun Kai on 15/3/2024.
//

extension Automaton.Tree {
    typealias NonGreedyResult = (GS.SubSequence, (max: Modality?, stack: RetryStack, regurgitation: NonGreedyRegurgitation)?)?
    
    typealias NonGreedyIntermediate = (GS.SubSequence, max: Modality?, regurgitation: NonGreedyRegurgitation)?
    
    func repeatNonGreedy(at gs: GS.SubSequence, range: Modality.Range, retryStack: RetryStack, regurgitation: NonGreedyRegurgitation) -> NonGreedyResult {
        if range.lowerBound > 1 {
            assert(regurgitation.isEmpty)
            
            return if retryStack.isEmpty {
                repeatNonGreedyFresh(at: gs, range: range)
            } else {
                repeatNonGreedyNotYet(at: gs, range: range, retryStack: retryStack)
            }
        } else {
            return repeatNonGreedyMin(at: gs, max: range.upperBound, retryStack: retryStack, regurgitation: regurgitation)
        }
    }
    
    func repeatNonGreedyFresh(at gs: GS.SubSequence, range: Modality.Range) -> NonGreedyResult {
        var workOn = gs
        
        for i in 1...range.lowerBound {
            switch traverseForEagerRepeat(startingAt: workOn, previousEnd: workOn, allowsEnd: i == range.lowerBound) {
            case let .completed(mutated):
                if let mutated {
                    let newRange = range - i
                    let end = workOn.eot
                    return (end, (nil, [.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: newRange, previousEnd: end))], []))
                } else {
                    return (workOn.eot, nil)
                }
            case let .progressed(newWorkOn, mutated):
                if let newRange = range - i {
                    let stack: RetryStack =
                    if let mutated {
                        [.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: newRange, previousEnd: newWorkOn))]
                    } else {
                        []
                    }
                    
                    if i == range.lowerBound {
                        return (newWorkOn, (newRange.upperBound, stack, [.retry(.init(replay: .init(alternative: self, startingAt: newWorkOn), progress: .init(context: newRange.upperBound, previousEnd: newWorkOn)))]))
                    } else if !stack.isEmpty {
                        return repeatNonGreedyNotYet(at: newWorkOn, range: newRange, retryStack: stack)
                    }
                    
                    workOn = newWorkOn
                } else {
                    return if let mutated {
                        (newWorkOn, (nil, [.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: nil, previousEnd: newWorkOn))], []))
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
    
    func retryRepeatNonGreedyNotYet(retryStack: RetryStack) -> NonGreedyResult {
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
                repeatNonGreedy(at: newWorkOn, range: range, retryStack: retryStack, regurgitation: [])
            } else {
                (newWorkOn, (nil, retryStack, []))
            }
        }
        
        return nil
    }
    
    func repeatNonGreedyNotYet(at gs: GS.SubSequence, range: Modality.Range, retryStack: RetryStack) -> NonGreedyResult {
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
                
                return (end, (nil, retryStack, []))
            case let .progressed(newWorkOn, mutated):
                if let newRange = range - i {
                    if let mutated {
                        retryStack.append(.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: newRange, previousEnd: newWorkOn)))
                    }
                    
                    if i == range.lowerBound {
                        return (newWorkOn, (newRange.upperBound, retryStack, []))
                    }
                    
                    workOn = newWorkOn
                } else {
                    if let mutated {
                        retryStack.append(.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: nil, previousEnd: newWorkOn)))
                    }
                    
                    return (newWorkOn, (nil, retryStack, []))
                }
            case .noProgress:
                // short-circuit the repeat, because all future repeated will have no progress too
                return (workOn, (nil, retryStack, []))
            case .notMatched:
                // rewind begins
                return retryRepeatNonGreedyNotYet(retryStack: retryStack)
            }
        }
        
        fatalError("impossible")
    }
    
    func repeatNonGreedyMin(at workOn: GS.SubSequence, max: Modality, retryStack: RetryStack, regurgitation: NonGreedyRegurgitation) -> NonGreedyResult {
        var regurgitation = regurgitation
        
        switch traverseForEagerRepeat(startingAt: workOn, previousEnd: workOn, allowsEnd: true) {
        case let .completed(mutated):
            let newMax = max - 1
            if let mutated {
                regurgitation.append(.preEot(.init(alternative: mutated, startingAt: workOn), max: newMax))
            }
                                     
            return (workOn.eot, (nil, retryStack, regurgitation))
        case let .progressed(newWorkOn, mutated):
            let newMax = max - 1
            
            if let mutated {
                regurgitation.append(.retry(.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: newMax, previousEnd: newWorkOn))))
            }
            
            return (newWorkOn, (newMax, retryStack, regurgitation))
        case .noProgress:
            // short-circuit the repeat, because all future repeated will have no progress too
            return (workOn, (nil, retryStack, regurgitation))
        case .notMatched:
            // previous success to fail over
            return if !regurgitation.isEmpty {
                if let (newWorkOn, newMax, newRegurgitation) = retryRepeatNonGreedyMin(regurgitation: regurgitation) {
                    (newWorkOn, (newMax, retryStack, newRegurgitation))
                } else {
                    nil
                }
            } else { // see if there're previous alternatives before fulfilling the min that can help
                retryRepeatNonGreedyNotYet(retryStack: retryStack)
            }
        }
    }
    
    func retryRepeatNonGreedyMin(at workOn: GS.SubSequence, maxAndLast: (Modality, GS.SubSequence)?, retryStack: RetryStack, regurgitation: NonGreedyRegurgitation) -> (GS.SubSequence, Automaton.Tree?)? {
        if let (newWorkOn, newMax, newRegurgitation) = retryRepeatNonGreedyMin(at: workOn, maxAndLast: maxAndLast, regurgitation: regurgitation) {
            (newWorkOn, .newRetryNonGreedyRepeat(repeated: self, max: newMax.map { ($0, newWorkOn) }, stack: retryStack, regurgitation: newRegurgitation))
        } else if let (newWorkOn, tuple) = retryRepeatNonGreedyNotYet(retryStack: retryStack) {
            if let (newMax, newStack, newRegurgitation) = tuple {
                (newWorkOn, .newRetryNonGreedyRepeat(repeated: self, max: newMax.map { ($0, newWorkOn) }, stack: newStack, regurgitation: newRegurgitation))
            } else {
                (newWorkOn, nil)
            }
        } else {
            nil
        }
    }
    
    func retryRepeatNonGreedyMin(at workOn: GS.SubSequence, maxAndLast: (Modality, GS.SubSequence)?, regurgitation: NonGreedyRegurgitation) -> NonGreedyIntermediate {
        guard let maxAndLast else {
            return if regurgitation.isEmpty {
                nil
            } else {
                retryRepeatNonGreedyMin(regurgitation: regurgitation)
            }
        }
        
        var regurgitation = regurgitation
        
        switch traverseForEagerRepeat(startingAt: workOn, previousEnd: maxAndLast.1, allowsEnd: true) {
        case let .completed(mutated):
            let newMax = maxAndLast.0 - 1
            
            if let mutated {
                regurgitation.append(.preEot(.init(alternative: mutated, startingAt: workOn), max: newMax))
            }
            
            return (workOn.eot, nil, regurgitation)
        case let .progressed(newWorkOn, mutated):
            let newMax = maxAndLast.0 - 1
            
            if let mutated {
                regurgitation.append(.retry(.init(replay: .init(alternative: mutated, startingAt: workOn), progress: .init(context: newMax, previousEnd: newWorkOn))))
            }
            
            return (newWorkOn, newMax, regurgitation)
        case .noProgress:
            // short-circuit the repeat, because all future repeated will have no progress too
            return (workOn, nil, regurgitation)
        case .notMatched:
            // previous success to fail over
            return retryRepeatNonGreedyMin(regurgitation: regurgitation)
        }
    }
    
    func retryRepeatNonGreedyMin(regurgitation: NonGreedyRegurgitation) -> NonGreedyIntermediate {
        assert(!regurgitation.isEmpty)
        var regurgitation = regurgitation
        
        let lastRegur = regurgitation.removeLast()
        
        switch lastRegur {
        case let .preEot(replay, max):
            switch replay.replay(max: max) {
            case let .retry(newRetry):
                regurgitation.append(.retry(newRetry))
                return (newRetry.progress.previousEnd, max, regurgitation)
            case let .progress(progress):
                return (progress.previousEnd, max, regurgitation)
            case .failed:
                return if regurgitation.isEmpty {
                    nil
                } else {
                    retryRepeatNonGreedyMin(regurgitation: regurgitation)
                }
            }
        case .retry(let retry):
            switch retry.retry() {
            case let .retry(newRetry):
                let workOn = newRetry.progress.previousEnd
                let max = newRetry.progress.context
                
                if workOn.isEmpty {
                    regurgitation.append(.preEot(newRetry.replay, max: max))
                    return (workOn, nil, regurgitation)
                } else {
                    regurgitation.append(.retry(newRetry))
                    return (workOn, max, regurgitation)
                }
            case let .progress(progress):
                let workOn = progress.previousEnd
                
                return if workOn.isEmpty {
                    (workOn, nil, regurgitation)
                } else {
                    (workOn, progress.context, regurgitation)
                }
            case .failed:
                return if regurgitation.isEmpty {
                    nil
                } else {
                    retryRepeatNonGreedyMin(regurgitation: regurgitation)
                }
            }
        }
    }
}
