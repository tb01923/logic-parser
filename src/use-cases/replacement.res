open Ast
open LawApplication

let getAbstractionForSymbol = (abstractionsBySymbol, symbol) => {
    switch (Belt.HashMap.String.get(abstractionsBySymbol, symbol)) {
    | Some(abstractedOperation) => abstractedOperation
    | None => makeVariable(symbol)
    }
}

let rec replaceAbstractionsInLaw = (node, abstractionBySymbol) => switch node {
| BinaryOperation(_, o, l, r) => makeBinaryOperation(
    o,
    replaceAbstractionsInLaw(l, abstractionBySymbol),
    replaceAbstractionsInLaw(r, abstractionBySymbol))
| UnaryOperation(_, o, t) => makeUnaryOperation(
    o,
    replaceAbstractionsInLaw(t, abstractionBySymbol))
| Value(_, v) => makeValue(v)
| Abstraction(_, s, o) => makeAbstraction(s, o)
| Variable(_, symbol) => getAbstractionForSymbol(abstractionBySymbol, symbol)
}

let rec getAbstractedOperations = (agg, node) => switch node {
| BinaryOperation(_, _, l, r) => getAbstractedOperations(agg, l)->getAbstractedOperations(r)
| UnaryOperation(_, _, t) => getAbstractedOperations(agg, t)
| Variable(_) | Value(_) => agg
| Abstraction(_, symbol, abstractedOperation) => {
    Belt.HashMap.String.set(agg, symbol, abstractedOperation)
    agg
  }
}

let getExtractor = matchedSide => switch matchedSide {
| LHS => Ast.getRhs
| RHS => Ast.getLhs
}

let replace = (originalStatement, statementToReplace, lawAst, matchedSide) => {


    let abstractionBySymbol =
    Belt.HashMap.String.make(~hintSize=3)
    ->getAbstractedOperations(originalStatement)

    // the "to" and "from" alphabets for conversion.  The "from" requires the entire binary operations
    //      in order for variables to be in correct relative order
    let statementAlphabet = Debruinj.getDebruinjIndices(statementToReplace)
    let lawAlpha = Debruinj.getDebruinjIndices(lawAst)

    // get the extractor that deterines the correct side of the law to apply
    let extractApplicableSide = getExtractor(matchedSide)

    let clonedLaw =
    lawAst
    ->extractApplicableSide
    ->Clone.clone(~targetAlphabet=statementAlphabet, ~sourceAlphabet=lawAlpha, _)
    ->replaceAbstractionsInLaw(abstractionBySymbol)

    let idToReplace = getId(statementToReplace)
    let rec replace = (node, with) => switch (idToReplace === getId(node), node) {
    | (true, _) => clonedLaw
    | (false, BinaryOperation(_, op, lhs, rhs)) => makeBinaryOperation(op, replace(lhs, with), replace(rhs, with))
    | (false, UnaryOperation(_, op, term)) => makeUnaryOperation(op, replace(term, with))
    | (false, Abstraction(_, s, ab)) => makeAbstraction(s, replace(ab, with))
    | (false, Variable(_)) => node
    | (false, Value(_)) => node
    }

    replace(originalStatement, clonedLaw)

}