//
//  NewTreeTests.swift
//
//
//  Created by Lau Chun Kai on 13/3/2024.
//

import XCTest
@testable import RegexTry

final class NewTreeTests : XCTestCase, TreeTestsBase {
    func testRetry() {
        let source = NoOpGraphemes(c: "abcde")
        let gs1 = source.dropFirst(0)
        let retry1 = Tree.Retry<Modality.Range>(replay: .init(alternative: .or(.init(string: "ab"), .init(string: "b")), startingAt: gs1), progress: .init(context: 2...(.infinity), previousEnd: gs1.dropFirst(1)))
        let retried1 = retry1.retry()
        guard case let .retry(retry2) = retried1 else {
            XCTFail()
            return
        }
        XCTAssertEqual(.pure(.init(string: "b")), retry2.replay.alternative)
        XCTAssertEqual(2, retry2.progress.previousEnd.offset)
        if case .failed = retry2.retry() {
        } else {
            XCTFail()
        }
    }
    
    func testRepeatTree() {
        let source = NoOpGraphemes(c: "abcde")
        let gs1 = source.dropFirst(0)
//        let repeat1 = Tree.newRepeat(repeated: .or(
//            .newRepeat(repeated: .pure(.init(string: "a")), range: 1..., isGreedy: true),
//            .pure(.init(string: "b")),
//            .pure(.init(string: "c")),
//            .pure(.init(string: "d")),
//            .pure(.init(string: "e"))
//        ), range: 3..., isGreedy: false)
        let repeat1 = Tree.newRepeat(repeated: .or(
//            .newRepeat(repeated: .pure(.init(string: "a")), range: 1..., isGreedy: true),
            .newRepeat(repeated: .or(
                .init(string: "a"),
                .init(string: "b"),
                .init(string: "c"),
                .init(string: "d"),
                .init(string: "e")
            ), range: 1..., isGreedy: false),
            .optional(.pure(.init(string: "b")), isGreedy: true),
            .optional(.pure(.init(string: "c")), isGreedy: true),
            .optional(.pure(.init(string: "d")), isGreedy: true),
            .optional(.pure(.init(string: "e")), isGreedy: true)
        ), range: 3..., isGreedy: false)
//        let repeat1 = Tree.newRepeat(repeated: .or(
//            .pure(.init(string: "a")),
//            .pure(.init(string: "b")),
//            .pure(.init(string: "c")),
//            .pure(.init(string: "d")),
//            .pure(.init(string: "e"))
//        ), range: 3...4, isGreedy: false)
        
        var result: Tree.Result?
        result = repeat1.traverse(startingAt: gs1)
        while let (gs12, repeat1) = result, !gs12.isEmpty, let repeat1 {
            if case let .newRetryNonGreedyRepeat(_, max, s, r) = repeat1 {
                devPrint("max: \(tuple: max), stack: [\(seq: s, separator: ",")],  regs: [\(seq: r, separator: ",")]")
            }
            devPrint(gs12)
            result = repeat1.traverse(startingAt: gs12)
        }
        if case let (gs12, .newRetryNonGreedyRepeat(_, max, s, r))? = result {
            devPrint("max: \(tuple: max), stack: [\(seq: s, separator: ",")],  regs: [\(seq: r, separator: ",")]")
        }
//        while let repeated = repeat12
        
//        guard let (gs12, repeat12) = repeat1.traverse(startingAt: gs1), let repeat12 else {
//            XCTFail()
//            return
//        }
//        devPrint(repeat12)
//        XCTAssert(gs12.isEmpty)
//        guard let (gs13, repeat13) = repeat12.traverse(startingAt: gs12), let repeat13 else {
//            XCTFail()
//            return
//        }
//        devPrint(repeat13)
//        XCTAssertEqual(4, gs13.offset)
//        guard let (gs14, repeat14) = repeat13.traverse(startingAt: gs13), let repeat14 else {
//            XCTFail()
//            return
//        }
//        devPrint(repeat14)
//        XCTAssertEqual(3, gs14.offset)
//        guard let (gs15, repeat15) = repeat14.traverse(startingAt: gs14), let repeat15 else {
//            XCTFail()
//            return
//        }
//        devPrint(repeat15)
//        XCTAssertEqual(3, gs15.offset)
    }
}
