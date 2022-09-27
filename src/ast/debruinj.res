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

let getDebruinjIndices = node => {
  let getNextDebuinjIndex = knownVariables =>
    knownVariables
    ->Belt.HashMap.String.keysToArray
    ->Js.Array2.map(key => Belt.HashMap.String.get(knownVariables, key))
    ->Js.Array2.reduce(max, Some(-1))
    ->Belt.Option.getWithDefault(-1)
    ->increment

  let id = (_, x) => x
  let getDebruinjIndexValue = (indices, name) => switch Belt.HashMap.String.has(indices, name) {
    | true => indices
    | false =>
        getNextDebuinjIndex(indices)
        -> Belt.HashMap.String.set(indices, name, _)
        -> id(indices)
    }

 let rec getDebruinjIndices = (indices, node) =>
    switch node {
    | BinaryOperation(_, _, lhs, rhs) => {
        indices
            ->getDebruinjIndices(lhs)
            ->getDebruinjIndices(rhs)
    }
    | Negate(_, term) => getDebruinjIndices(indices, term)
    | Variable(_, name) => getDebruinjIndexValue(indices, name)
    | Value(_, _) => indices
    }

  Belt.HashMap.String.make(~hintSize=10)->getDebruinjIndices(node)
}