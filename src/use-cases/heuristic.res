open Ast
open Debruinj

let accumulateMapReducer = (acc, _, _) => acc + 1
let countItems = Belt.HashMap.String.reduce(_, 0, accumulateMapReducer)

let rec countBinary = (acc, node) => switch node {
| BinaryOperation(_, _, lhs, rhs) => 1 + acc + countBinary(0, lhs) + countBinary(0, rhs)
| UnaryOperation(_, _, term) => countBinary(0, term)
| _ => acc
}

let rec countUnary = (acc, node) => switch node {
| UnaryOperation(_, _, term) => 1 + acc + countUnary(0, term)
| BinaryOperation(_, _, lhs, rhs) => acc + countUnary(0, lhs) + countUnary(0, rhs)
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
    // one unary operation with a single variable "not(a)"
    | (1,0,1) => true
    // one binary operation with two variables "a and b"
    | (0, 1, 2) => true
    // one more var than binary op, "a and b and c"
    | (0, b, v) if v - b === 1 => true
    // one more var than bin op, with at most one unary
    | (1, b, v) if v - b === 1 => true
    // no operations, no variables "T"
    | (0, 0, 0) => true
    // no operations, one variable "a"
    | (0, 0, 1) => true
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