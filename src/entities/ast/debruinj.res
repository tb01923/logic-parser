open Ast;
let increment = a => a + 1

let max = (a, b) =>
  switch (a, b) {
  | (Some(a1), Some(b1)) if a1 >= b1 => Some(a1)
  | (Some(_), Some(b1)) => Some(b1)
  | (Some(a1), None) => Some(a1)
  | (None, Some(b1)) => Some(b1)
  | (None, None) => Some(-1)
  }


let getNextDebuinjIndex = knownVariables =>
    knownVariables
    ->Belt.HashMap.String.keysToArray
    ->Js.Array2.map(key => Belt.HashMap.String.get(knownVariables, key))
    ->Js.Array2.reduce(max, Some(-1))
    ->Belt.Option.getWithDefault(-1)
    ->increment


let addSymbolToIndices = (indices, name) => {
    let id = (_, x) => x
    switch Belt.HashMap.String.has(indices, name) {
    | true => indices
    | false =>
        getNextDebuinjIndex(indices)
        -> Belt.HashMap.String.set(indices, name, _)
        -> id(indices)
    }
}

let getNextSymbol = (indices) => {
    let max = (a, b) => switch (a > b) {
    | true => a
    | false => b
    }

    let increment = x => Belt.Float.toInt(x) + 1

    indices
    -> Belt.HashMap.String.keysToArray
    -> Belt.Array.reduce("", max)
    -> Js.String2.charCodeAt(0)
    -> increment
    -> Js.String2.fromCharCode
}

let getDebruinjIndices = node => {

 let rec getDebruinjIndices = (indices, node) =>
    switch node {
    | BinaryOperation(_, _, lhs, rhs) => {
        indices
        ->getDebruinjIndices(lhs)
        ->getDebruinjIndices(rhs)
    }
    | UnaryOperation(_, _, term) => getDebruinjIndices(indices, term)
    | Variable(_, name) => addSymbolToIndices(indices, name)
    | Abstraction(_, symb, prop) =>
        indices
        ->addSymbolToIndices(symb)
        ->getDebruinjIndices(prop)
    | Value(_, _) => indices
    }

  Belt.HashMap.String.make(~hintSize=10)
  ->getDebruinjIndices(node)
}