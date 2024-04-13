open Test

module Boolean = {
  let isTrue = (message, bool) =>
    assertion(~message=message, ~operator="boolean is true", (a,b) => a == b, true, bool)
  let isFalse = (message, bool) =>
    assertion(~message=message, ~operator="boolean is false", (a,b) => a == b, false, bool)
}


module HashMapIntString = {
  module PairComparator = Belt.Id.MakeComparableU({
        type t = (int, string)
         let cmp = ((a0, a1), (b0, b1)) =>
            switch Pervasives.compare(a0, b0) {
            | 0 => Pervasives.compare(a1, b1)
            | c => c
            }
    })
    
  let iEqualBySet = (a, b) => {
    let arrA = Belt.HashMap.Int.toArray(a)
    let arrB = Belt.HashMap.Int.toArray(b)
    let setA = Belt.Set.fromArray(arrA, ~id=module(PairComparator))
    let setB = Belt.Set.fromArray(arrB, ~id=module(PairComparator))

    (setA == setB)
  }

  let isHashMapEqual = (message, a, b) =>
    assertion(~message=message, ~operator="Assert.HashMapIntString.hasmapEqualBySet", iEqualBySet, a, b)
}

module Parser = {
  let _throwsNoTokens = (f, _) => {
      try {
        f()
        false
      }
      catch {
      | Parser.NoTokens => true
      }
  }

  let _throwsUnexpectedToken = (f, token) => {
      try {
        f()
        false
      }
      catch {
      | Parser.UnexpectedToken(_token) => _token == token
      }
  }
  
  let throwsNoTokens = (message, f) =>
    assertion(~message=message, ~operator="custom expecting exception", _throwsNoTokens, f, None)

  let throwsUnexpectedToken = (message, f, token) =>
    assertion(~message=message, ~operator="custom expecting exception", _throwsUnexpectedToken, f, token)
}

module Ast = {
  let isEqualByName = (message, a, b) =>
    assertion(~message=message, ~operator="Equality.byName", Equality.byName, a, b)
  
  let notEqualByName = (a, b) => !Equality.byName(a, b)
  
  let isNotEqualByName = (message, a, b) =>
    assertion(~message=message, ~operator="Equality.byName", notEqualByName, a, b)

  let isEqualByDeBruinj = (message, a, b) =>
    assertion(~message=message, ~operator="Equality.byDebruinj", (a, b) => Equality.byDebruinj(a, b), a, b)
}

module Lexer = {
  open Lexer
  let isTokenEqual = (a, b) =>
    assertion(~message="Tokens Equal <" ++ toString(a) ++"," ++ toString(b) ++ ">", 
    ~operator="Lexer.isEqualtokenEquals", tokenEquals, a, b)
  let isTokenNotEqual = (a, b) =>
    assertion(~message="Tokens Not Equal <" ++ toString(a) ++"," ++ toString(b) ++ ">", 
    ~operator="Lexer.isEqualtokenEquals", (a, b) => !tokenEquals(a, b), a, b)
}