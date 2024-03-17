//
//  TreeTestsBase.swift
//
//
//  Created by Lau Chun Kai on 13/3/2024.
//

@testable import RegexTry

protocol TreeTestsBase {
    typealias A = Automaton<NoOpGraphemes<String>, String>
    typealias Operation = A.Operation
    typealias Tree = A.Tree
}
