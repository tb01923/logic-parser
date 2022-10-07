open Ast
open Debruinj

let accumulateMapReducer = (acc, _, _) => acc + 1
let countItems = Belt.HashMap.String.reduce(_, 0, accumulateMapReducer)

let rec countBinary = (acc, node) => switch node {
| BinaryOperation(_, _, lhs, rhs) => 1 + acc + countBinary(0, lhs) + countBinary(0, rhs)
| _ => acc
}

let rec countUnary = (acc, node) => switch node {
| UnaryOperation(_, _, term) => 1 + acc + countUnary(0, term)
| _ => acc
}

let countVariables = (node) =>
    node
    -> getDebruinjIndices
    -> countItems

let rec countOperations = (acc, node) => switch node {
| BinaryOperation(_, _, lhs, rhs) => 1 + acc + countOperations(0, lhs) + countOperations(0, rhs)
| UnaryOperation(_, _, term) => 1 + acc + countOperations(0, term)
| _ => acc
}

let isOptimalSolution = (node) => {
    let numUnary = countUnary(0, node)
    let numBinary = countBinary(0, node)
    let numVars = countVariables(node)

    switch (numUnary, numBinary, numVars) {
    | (1,0,1) => true
    | (0, 1, 2) => true
    | _ => false
    }
}

let variablesRaisedToOperations = node => {

    //"not(a)" and "a and a" both contain one operation and one vzriable, we should "prefer" the
    //  former.  Since it is more optimal than the latter.
    let binaryOpBump = switch node {
    | BinaryOperation(_) => 1
    | _ => 0
    }

    Js.Math.pow_float(
        ~base=(countVariables(node) + binaryOpBump)->Belt.Int.toFloat,
        ~exp=countOperations(0, node)->Belt.Int.toFloat)
}

let compare = (a, b) => {
    (variablesRaisedToOperations(b) -. variablesRaisedToOperations(a))->Belt.Float.toInt
}