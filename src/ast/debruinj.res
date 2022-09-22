open Ast;

let increment = a => a + 1;

let max = (a, b) => switch (a, b) {
    | (Some(a1), Some(b1)) if a1 >= b1 => Some(a1)
    | (Some(_), Some(b1)) => Some(b1)
    | (Some(a1), None) => Some(a1)
    | (None, Some(b1)) => Some(b1)
    | (None, None)  => Some(-1)
}

let getNextDebuinjIndex = knownVariables =>
  knownVariables
  ->Belt.HashMap.String.keysToArray
  ->Js.Array2.map(key => Belt.HashMap.String.get(knownVariables, key))
  ->Js.Array2.reduce(max, Some(-1))
  ->Belt.Option.getWithDefault(-1)
  ->increment;

let getDebruinjIndexValue = (indices, name) =>
  if (Belt.HashMap.String.has(indices, name)) {
    indices;
  } else {
    let nextIndex = getNextDebuinjIndex(indices);
    Belt.HashMap.String.set(indices, name, nextIndex);
    indices;
  };

let rec getDebruinjIndices = (indices, node) =>
  switch (node) {
  | And(_, lhs, rhs) => getNextDebuinjIndexBinOp(indices, lhs, rhs)
  | Or(_, lhs, rhs) => getNextDebuinjIndexBinOp(indices, lhs, rhs)
  | Implies(_, lhs, rhs) => getNextDebuinjIndexBinOp(indices, lhs, rhs)
  | BiConditional(_, lhs, rhs) => getNextDebuinjIndexBinOp(indices, lhs, rhs)
  | Not(_, term) => getDebruinjIndices(indices, term)
  | Variable(_, name) => getDebruinjIndexValue(indices, name)
  | Value(_,_) => indices
  }
and getNextDebuinjIndexBinOp = (indices, lhs, rhs) =>
  indices->getDebruinjIndices(lhs)->getDebruinjIndices(rhs);

let getDebruinjIndices = node =>
  Belt.HashMap.String.make(~hintSize=10)->getDebruinjIndices(node);