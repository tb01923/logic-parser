open Test

module Demo = {
  let intEqual = (~message=?, a: int, b: int) =>
    assertion(~message?, ~operator="Int equals", (a, b) => a === b, a, b)
}

module Boolean = {
  let isTrue = (message, bool) =>
    assertion(~message=message, ~operator="boolean is true", (a,b) => a == b, true, bool)
  let isFalse = (message, bool) =>
    assertion(~message=message, ~operator="boolean is false", (a,b) => a == b, false, bool)
}

module Ast = {
   let isEqualByName = (message, a: Ast.proposition, b: Ast.proposition) =>
    assertion(~message=message, ~operator="Equality.byName", Equality.byName, a, b)
}