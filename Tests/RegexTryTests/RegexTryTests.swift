import XCTest
@testable import RegexTry

final class RegexTryTests: XCTestCase {
    func testExample() throws {
        // XCTest Documentation
        // https://developer.apple.com/documentation/xctest

        // Defining Test Cases and Test Methods
        // https://developer.apple.com/documentation/xctest/defining_test_cases_and_test_methods
    }
    
    func testLexer() throws {
        let ret = try lexAscii(s: "((a*?|)?(?:白癡{1,})?|(??)[^sd^\\w(]$(?=)(<=))")
        print(ret.map(\.0))
        
        let ret2 = try lexUnicode(s: "((a*?|)?(?:白癡{1,})?|(??)[^sd^\\w(]$(?=)(<=))")
        print(ret2.map(\.0))
    }
    
    func testComposite() throws {
        let ret = try lexAscii(s: "a?(([^byte])*(hoho)?)")
        let a = try Parser<NoOpGraphemes<[UInt8]>, [UInt8]>.parse(lexedList: ret)
        print(a)
        
        let groups = a.process(text: "abcdrhoho")
        print(groups)
    }
    
    func testComposite2() throws {
        let ret = try lexUnicode(s: "((?:aa)*|白癡)+")
        print(ret)
        let a = try Parser<NoOpGraphemes<String>, String>.parse(lexedList: ret)
        print(a)
        
        let text = "aaa白癡白癡aaa白癡a白癡a"
        let matches = a.process(text: text)
        
        for (i, match) in matches.enumerated() {
            print("match \(i)")
            
            for (j, group) in match.enumerated() {
                print("(\(j): \(group.map { "'\(text[$0])'" } ?? "<nil>"))")
            }
        }
//        print(groups)
    }
////
////    func testTimes() {
////        var t = Parser.Automaton.Operation.Repeater.Times.atLeast(3)
////        t = t.matching().next
////        XCTAssertEqual(.atLeast(2), t)
////        t = t.matching().next
////        XCTAssertEqual(.atLeast(1), t)
////        t = t.matching().next
////        XCTAssertEqual(.atLeast(0), t)
////        
////        t = .range(2, 4)
////        t = t.matching().next
////        XCTAssertEqual(.range(1, 3), t)
////        t = t.matching().next
////        XCTAssertEqual(.atMost(2), t)
////        t = t.matching().next
////        XCTAssertEqual(.range(1, 2), t)
////        t = t.matching().next
////        XCTAssertEqual(.atMost(1), t)
////    }
//    
//    func testAutomaton() {
//        let repeater = Automaton<UInt8>.Repeater(child:.or([
//                .match({ "c" ~= $0 }),
//                .init(string: "aa")
//            ]), times: .exact(7, true), isGreedy: false
//        )
//        let text = "aacaaccaacaaccd"
//        
//        let automaton = Automaton(groupCount: 0, .repeater(repeater))
//        let result = automaton.process(text: text)
//        print(result)
//    }
//    
//    func testGrouper() {
//        var grouper = Automaton<UInt8>.Grouper(operations: [
//            .group(.init(operations: [
//                .or([
//                    .match({ "a" ~= $0 }),
//                    .init(string: "aa")
//                ]),
//                .or([
//                    .match({ "a" ~= $0 }),
//                    .init(string: "ac")
//                ]),
//            ]), 1),
//            .or([
//                .match({ "a" ~= $0 }),
//                .init(string: "ac")
//            ]),
//            .init(string: "c"),
//        ])
//        
//        let text = "aaacacc"
//        var groups = Automaton<UInt8>.Result.Groups(repeating: nil, count: 2)
//        let textU8 = UInt8.convertToGraphemes(s: text)
//        
//        let result1 = grouper.process(stream: textU8.dropFirst(0), groups: &groups)
//        switch result1 {
//        case let .success(endStream):
//            XCTAssertEqual("aaac", textU8[0..<endStream.startIndex])
//        case .failed:
//            XCTFail("first match")
//        }
//        print(groups)
//        let result2 = grouper.process(stream: textU8.dropFirst(0), groups: &groups)
//        switch result2 {
//        case let .success(endStream):
//            XCTAssertEqual("aaacac", textU8[0..<endStream.startIndex])
//        case .failed:
//            XCTFail("second match")
//        }
//        print(groups)
//        let result3 = grouper.process(stream: textU8.dropFirst(0), groups: &groups)
//        switch result3 {
//        case let .success(endStream):
//            XCTAssertEqual("aaacacc", textU8[0..<endStream.startIndex])
//        case .failed:
//            XCTFail("third match")
//        }
//        print(groups)
//        XCTAssertFalse(grouper.canReuse)
//        
//        let result4 = grouper.process(stream: textU8.dropFirst(0), groups: &groups)
//        switch result4 {
//        case .success:
//            XCTFail("impossible")
//        case .failed:
//            break
//        }
//    }
//    
//    func testRepeater() {
//        var repeater = Automaton<UInt8>.Repeater(child:.or([
//                .match({ "c" ~= $0 }),
//                .init(string: "aa")
//            ]), times: .exact(7, true), isGreedy: false
//        )
//        
//        let text = "aacaaccaacaaccd"
//        var groups = Automaton<UInt8>.Result.Groups()
//        let textU8 = UInt8.convertToGraphemes(s: text)
//        
//        while case let .success(newStream) = repeater.process(stream: textU8.dropFirst(0), groups: &groups) {
//            print(textU8[0..<newStream.startIndex])
//        }
//        
////        let result1 = repeater.process(byteStream: textU8.dropFirst(0), groups: &groups)
////        switch result1 {
////        case let .success(endStream):
////            XCTAssertEqual("aaac", String(bytes: textU8[0..<endStream.startIndex], encoding: .utf8))
////        case .failed:
////            XCTFail("first match")
////        }
////        print(repeater.operationStack)
////        
////        let result2 = repeater.process(byteStream: textU8.dropFirst(0), groups: &groups)
////        switch result2 {
////        case let .success(endStream):
////            XCTAssertEqual("aaacac", String(bytes: textU8[0..<endStream.startIndex], encoding: .utf8))
////        case .failed:
////            XCTFail("second match")
////        }
////        print(groups)
////        
////        let result3 = repeater.process(byteStream: textU8.dropFirst(0), groups: &groups)
////        switch result3 {
////        case let .success(endStream):
////            XCTAssertEqual("aaacacc", String(bytes: textU8[0..<endStream.startIndex], encoding: .utf8))
////        case .failed:
////            XCTFail("third match")
////        }
////        print(groups)
////        
////        XCTAssertFalse(repeater.canReuse)
//    }
}
