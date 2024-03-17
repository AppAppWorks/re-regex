//
//  ModalityTests.swift
//
//
//  Created by Lau Chun Kai on 13/3/2024.
//

import XCTest
@testable import RegexTry

final class ModalityTest : XCTestCase {
    func testFinite() {
        var m1: Modality = 1
        m1 += 1
        XCTAssertEqual(2, m1)
        let m2 = m1 - 2
        XCTAssertNil(m2)
    }
    
    func testInfinite() {
        var m1 = Modality.infinity
        m1 += 1
        XCTAssertEqual(.infinity, m1)
        XCTAssertEqual(.infinity, m1 - 2)
        XCTAssertTrue(m1 > 1)
        let m2 = Modality.infinity
        XCTAssertTrue(m1 == m2)
        XCTAssertFalse(m1 > m2)
        let m3: Modality = 10000_0000
        XCTAssertTrue(m1 > m3)
        XCTAssertTrue(m1 >= m3)
        XCTAssertFalse(m1 < m3)
        XCTAssertFalse(m1 <= m3)
        XCTAssertFalse(m1 == m3)
    }
    
    func testRange() {
        let upper: Modality = .infinity
        for i in 1...upper {
            print(i)
            if i == 20 {
                break
            }
        }
    }
}
