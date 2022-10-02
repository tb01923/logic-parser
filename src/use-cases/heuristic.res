open Ast
open Debruinj

let variablesRaisedToOperations = node => {
    // helpers to count distinct variable names
    let accumulateMapReducer = (acc, _, _) => acc + 1
    let countItems = Belt.HashMap.String.reduce(_, 0, accumulateMapReducer)

    let numVariables = node
    -> getDebruinjIndices
    -> countItems
    -> Belt.Int.toFloat

    let rec countOperations = (acc, node) => switch node {
    | BinaryOperation(_, _, lhs, rhs) => 1 + acc + countOperations(0, lhs) + countOperations(0, rhs)
    | UnaryOperation(_, _, term) => 1 + acc + countOperations(0, term)
    | _ => acc
    }

    let numOperations = node
    -> countOperations(0, _)
    -> Belt.Int.toFloat

    Js.Math.pow_float(~base=numVariables, ~exp=numOperations)
}