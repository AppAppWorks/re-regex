//
//  Parser.swift
//  
//
//  Created by Lau Chun Kai on 26/2/2024.
//

public enum Parser {
    struct ParserError : Error {
        
    }
    
    func parse(lexedList: [Lexer.Lexed]) throws -> Automaton {
        .init(groupCount: 0, operations: .bytes([0]))
    }
}
