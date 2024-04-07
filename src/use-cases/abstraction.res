open Ast
open Clone
open Debruinj
open Heuristic

let rec getOperations = (node, candidates) =>
  switch node {
  | BinaryOperation(_, _, lhs, rhs) => {
    candidates
    -> Belt.Array.concat([node])
    -> Belt.Array.concat(getOperations(lhs, candidates))
    -> Belt.Array.concat(getOperations(rhs, candidates))
    }
  | UnaryOperation(_, _, term) =>
    candidates
    -> Belt.Array.concat([node])
    -> Belt.Array.concat(getOperations(term, candidates))
  | _ => candidates
  }


let abstractOperation = (knownSymbols, operation) => {
    // use the debruinj logic to get a symobl for this abstraction,
    //  and register it in the known indexes
    let nextSymbol = getNextSymbol(knownSymbols)
    addSymbolToIndices(knownSymbols, nextSymbol) -> ignore

    makeAbstraction(nextSymbol, operation)
}

let rec performAbstraction = (statement, abstraction) => {
    let abstractEquals = (rhs) => Equality.byAbstractionResolution(abstraction, rhs)
    switch (statement) {
    | BinaryOperation(_) if abstractEquals(statement) => abstraction
    | BinaryOperation(_, op, lhs, rhs) => makeBinaryOperation(
        op, performAbstraction(lhs, abstraction), performAbstraction(rhs, abstraction))
    | UnaryOperation(_) if abstractEquals(statement) => abstraction
    | UnaryOperation(_, op, term) => makeUnaryOperation(op, performAbstraction(term, abstraction))
    | Variable(_, _) => statement
    | Value(_, _) => statement
    | Abstraction(_) => statement
    };
}

let applyAbstraction = (agg, abstraction) => {
    let thisApplication =
    agg
    // build on the most recently applied abstraction
    -> Belt.Array.getExn(Belt.Array.length(agg)-1)
    -> clone
    // take the next abstraction (passed in function) and apply to the version
    //    of the statement, with all previous abstractions already applied
    -> performAbstraction(abstraction)

    // record this as the most recent (and most abstract) version of the statement
    Belt.Array.push(agg, thisApplication)
    agg
}

let getAbstractions = (statement) => {
  let second = ((_, b)) => b
  let sortByComplexity = ((scoreA, _), (scoreB, _)) => scoreA - scoreB
  let knownSymbols = getDebruinjIndices(statement)

  statement
  -> getOperations([])
  // score each operation for complexity, sort for simplicity first, then drop the scores
  -> Belt.Array.map(node => (variablesRaisedToOperations(node), node))
  -> Belt.SortArray.stableSortBy(sortByComplexity)
  -> Belt.Array.map(second)
  // remove duplicates bny placing into and removing from a set
  -> Belt.Set.fromArray(~id=module(Equality.PropositionCompare))
  -> Belt.Set.toArray
  // box each Operation within an Abstraction
  -> Belt.Array.map(operation => abstractOperation(knownSymbols, operation))
  // Replace Operations with corresponding Abstraction, resulting in an incremental variants of the
  //    original statement.  S0 is most specfic form, S1 has Abstraction1 applied, S2 has abstraction1 & 2 applied...
  -> Belt.Array.reduce([statement], applyAbstraction)
  // should we slice off the original version, which by definition doesn't have any abstractions,
  //    but it is a variation of the equation (most specific form)
  //-> Belt.Array.sliceToEnd(1)
}