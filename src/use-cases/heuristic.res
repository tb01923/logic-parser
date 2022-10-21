open Ast
open Debruinj
open VectorRepresentation

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

let countUniqueVariables = (node) =>
    node
    -> getDebruinjIndices
    -> countItems

let rec countOperations = (acc, node) => switch node {
| BinaryOperation(_, _, lhs, rhs) => 1 + acc + countOperations(0, lhs) + countOperations(0, rhs)
| UnaryOperation(_, _, term) => 1 + acc + countOperations(0, term)
| _ => acc
}

let rec countVariables = (acc, node) => switch node {
| BinaryOperation(_, _, lhs, rhs) => acc + countVariables(0, lhs) + countVariables(0, rhs)
| UnaryOperation(_, _, term) => acc + countVariables(0, term)
| Variable(_, _) => acc + 1
| _ => acc
}

let isOptimalSolution = (node) => {
    let numUnary = countUnary(0, node)
    let numBinary = countBinary(0, node)
    let numVars = countUniqueVariables(node)

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
        ~base=(countUniqueVariables(node) + binaryOpBump)->Belt.Int.toFloat,
        ~exp=countOperations(0, node)->Belt.Int.toFloat)->Belt.Float.toInt
}

let complexity2 = (node) => {
    let u = countUniqueVariables(node)
    let o = countOperations(0, node)
    let v = countVariables(0, node)

    v + u  + o
}

let complexity = complexity2


let minimum = (a, b, c) => {
    if a < b && a < c {
        a
    }
    else if b < a && b < c {
        b
    }
    else {
        c
    }
}
/*
lDistance :: Eq a => [a] -> [a] -> Int
lDistance [] t = length t -- If s is empty, the distance is the number of characters in t
lDistance s [] = length s -- If t is empty, the distance is the number of characters in s
lDistance (a : s') (b : t') =
  if a == b
    then lDistance s' t' -- If the first characters are the same, they can be ignored
    else
      1
        + minimum -- Otherwise try all three possible actions and select the best one
          [ lDistance (a : s') t', -- Character is inserted (b inserted)
            lDistance s' (b : t'), -- Character is deleted  (a deleted)
            lDistance s' t' -- Character is replaced (a replaced with b)
          ]
*/

let rec levenshtein = (s, t) =>  switch(s, t) {
| (list{}, t) => Belt.List.length(t)
| (s, list{}) => Belt.List.length(s)
| (list{a, ...ss}, list{b, ...ts}) if a == b => levenshtein(ss, ts)
| (list{_, ...ss}, list{_, ...ts}) => {
    1 + minimum(
        // lDistance (a : s') t'    (in this case algo reforms s by append a to s' (or ss), we can just refer to s)
        levenshtein(s, ts),
        // lDistance s' (b : t')    (in this case algo reforms t by append b to t' (or ts), we can just refer to t)
        levenshtein(ss, t),
        // lDistance s' t'
        levenshtein(ss, ts)
    )
}}

let levenshteinProposition  = (a: proposition, b: proposition) => {
    let va = toVector(a)
    let vb = toVector(b)
    levenshtein(va, vb)
}