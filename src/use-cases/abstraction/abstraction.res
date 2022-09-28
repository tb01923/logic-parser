open Ast
open Clone
open Debruinj

let rec getOperations = (node, candidates) =>
  switch node {
  | BinaryOperation(_, _, lhs, rhs) => {
    candidates
    -> Belt.Array.concat([node])
    -> Belt.Array.concat(getOperations(lhs, candidates))
    -> Belt.Array.concat(getOperations(rhs, candidates))
    }
  | Negation(_, term) =>
    candidates
    -> Belt.Array.concat([node])
    -> Belt.Array.concat(getOperations(term, candidates))
  | _ => candidates
  }

let getOperationComplexity = node => {
    // helpers to count distinct variable names
    let accumulateMapReducer = (acc, _, _) => acc + 1
    let countItems = Belt.HashMap.String.reduce(_, 0, accumulateMapReducer)

    let numVariables = node
    -> getDebruinjIndices
    -> countItems
    -> Belt.Int.toFloat

    let rec countOperations = (acc, node) => switch node {
    | BinaryOperation(_, _, lhs, rhs) => 1 + acc + countOperations(0, lhs) + countOperations(0, rhs)
    | Negation(_, term) => 1 + acc + countOperations(0, term)
    | _ => acc
    }

    let numOperations = node
    -> countOperations(0, _)
    -> Belt.Int.toFloat

    (Js.Math.pow_float(~base=numVariables, ~exp=numOperations), node)
}

let abstractOperation = (knownSymbols, operation) => {
    // use the debruinj logic to get a symobl for this abstraction,
    //  and register it in the known indexes
    let nextSymbol = getNextSymbol(knownSymbols)
    addSymbolToIndices(knownSymbols, nextSymbol) -> ignore

    makeAbstraction(nextSymbol, operation)
}


let rec replace = (statement, abstraction) => {
    let abstractEquals = Equality.byAbstractionResolution(abstraction)
    switch (statement) {
    | BinaryOperation(_, op, lhs, rhs) if abstractEquals(lhs) && abstractEquals(rhs)  =>
        makeBinaryOperation(op, abstraction, abstraction)
    | BinaryOperation(_, op, lhs, rhs) if abstractEquals(lhs) =>
        makeBinaryOperation(op, abstraction, replace(rhs, abstraction))
    | BinaryOperation(_, op, lhs, rhs) if abstractEquals(rhs) =>
        makeBinaryOperation(op, replace(lhs, abstraction), abstraction)
    | BinaryOperation(_, op, lhs, rhs) =>
        makeBinaryOperation(op, replace(lhs, abstraction), replace(rhs, abstraction))
    | Abstraction(_) => statement
    | Negation(_, term) if abstractEquals(term) => abstraction
    | Negation(_, term) => makeNegation(replace(term, abstraction))
    | Variable(_, _) => statement
    | Value(_, _) => statement
    };
}

let applyAbstraction = (agg, abstraction) => {
    let thisApplication = agg
    -> Belt.Array.getExn(0)
    -> clone
    -> replace(abstraction)

    Belt.Array.push(agg, thisApplication)
    agg
}


let getAbstractions = (statement) => {
  let second = ((_, b)) => b
  let sortByComplexity = ((scoreA, _), (scoreB, _)) => Belt.Float.toInt(scoreA -. scoreB)
  let knownSymbols = getDebruinjIndices(statement)

  statement
  -> getOperations([])
  // score each operation for complexity, sort for simplicity first, then drop the scores
  -> Belt.Array.map(getOperationComplexity)
  -> Belt.SortArray.stableSortBy(sortByComplexity)
  -> Belt.Array.map(second)
  // remove duplicates
  -> Belt.Set.fromArray(~id=module(Equality.PropositionCompare))
  -> Belt.Set.toArray
  // build abstractions from each operation
  -> Belt.Array.map(abstractOperation(knownSymbols))
  // apply abstractions onto statement, one at a time building on prior
  -> Belt.Array.reduce([statement], applyAbstraction)
}
