import XCTest
@testable import RegexTry

final class RegexTryTests: XCTestCase {
    func testExample() throws {
        // XCTest Documentation
        // https://developer.apple.com/documentation/xctest

        // Defining Test Cases and Test Methods
        // https://developer.apple.com/documentation/xctest/defining_test_cases_and_test_methods
    }
    
//    func testLexer() throws {
//        let ret = try Lexer.lex(s: "((a*?|)?(?:白癡{1,})?|(??)[^sd^\\w(]$(?=)(<=))")
//        print(ret.map(\.0))
//    }
//    
//    func testTimes() {
//        var t = Parser.Automaton.Operation.Repeater.Times.atLeast(3)
//        t = t.matching().next
//        XCTAssertEqual(.atLeast(2), t)
//        t = t.matching().next
//        XCTAssertEqual(.atLeast(1), t)
//        t = t.matching().next
//        XCTAssertEqual(.atLeast(0), t)
//        
//        t = .range(2, 4)
//        t = t.matching().next
//        XCTAssertEqual(.range(1, 3), t)
//        t = t.matching().next
//        XCTAssertEqual(.atMost(2), t)
//        t = t.matching().next
//        XCTAssertEqual(.range(1, 2), t)
//        t = t.matching().next
//        XCTAssertEqual(.atMost(1), t)
//    }
    
    func testGrouper() {
        var grouper = Automaton.Grouper(operations: [
            .group(.init(operations: [
                .or([
                    .match({ "a" ~= $0 }),
                    .init(string: "aa")
                ]),
                .or([
                    .match({ "a" ~= $0 }),
                    .init(string: "ac")
                ]),
            ]), 1),
            .or([
                .match({ "a" ~= $0 }),
                .init(string: "ac")
            ]),
            .init(string: "c"),
        ])
        
        let text = "aaacacc"
        var groups = Automaton.Result.Groups()
        let textU8 = Array(text.utf8)
        
        let result1 = grouper.process(byteStream: textU8.dropFirst(0), groups: &groups)
        switch result1 {
        case let .success(endStream):
            XCTAssertEqual("aaac", String(bytes: textU8[0..<endStream.startIndex], encoding: .utf8))
        case .failed:
            XCTFail("first match")
        }
        print(groups)
        let result2 = grouper.process(byteStream: textU8.dropFirst(0), groups: &groups)
        switch result2 {
        case let .success(endStream):
            XCTAssertEqual("aaacac", String(bytes: textU8[0..<endStream.startIndex], encoding: .utf8))
        case .failed:
            XCTFail("second match")
        }
        print(groups)
        let result3 = grouper.process(byteStream: textU8.dropFirst(0), groups: &groups)
        switch result3 {
        case let .success(endStream):
            XCTAssertEqual("aaacacc", String(bytes: textU8[0..<endStream.startIndex], encoding: .utf8))
        case .failed:
            XCTFail("third match")
        }
        print(groups)
        XCTAssertFalse(grouper.canReuse)
        
        let result4 = grouper.process(byteStream: textU8.dropFirst(0), groups: &groups)
        switch result4 {
        case .success:
            XCTFail("impossible")
        case .failed:
            break
        }
    }
    
    func testRepeater() {
        var repeater = Automaton.Repeater(child:.or([
                .match({ "c" ~= $0 }),
                .init(string: "aa")
            ]), times: .exact(7, true), isGreedy: false
        )
        
        let text = "aacaaccaacaaccd"
        var groups = Automaton.Result.Groups()
        let textU8 = Array(text.utf8)
        let stream = textU8[0...]
        
        while case let .success(newStream) = repeater.process(byteStream: stream, groups: &groups) {
            print(String(bytes: textU8[0..<newStream.startIndex], encoding: .utf8))
        }
        
//        let result1 = repeater.process(byteStream: textU8.dropFirst(0), groups: &groups)
//        switch result1 {
//        case let .success(endStream):
//            XCTAssertEqual("aaac", String(bytes: textU8[0..<endStream.startIndex], encoding: .utf8))
//        case .failed:
//            XCTFail("first match")
//        }
//        print(repeater.operationStack)
//        
//        let result2 = repeater.process(byteStream: textU8.dropFirst(0), groups: &groups)
//        switch result2 {
//        case let .success(endStream):
//            XCTAssertEqual("aaacac", String(bytes: textU8[0..<endStream.startIndex], encoding: .utf8))
//        case .failed:
//            XCTFail("second match")
//        }
//        print(groups)
//        
//        let result3 = repeater.process(byteStream: textU8.dropFirst(0), groups: &groups)
//        switch result3 {
//        case let .success(endStream):
//            XCTAssertEqual("aaacacc", String(bytes: textU8[0..<endStream.startIndex], encoding: .utf8))
//        case .failed:
//            XCTFail("third match")
//        }
//        print(groups)
//        
//        XCTAssertFalse(repeater.canReuse)
    }
}
