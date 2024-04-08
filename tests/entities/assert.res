open Test

module Demo = {
  let intEqual = (~message=?, a: int, b: int) =>
    assertion(~message?, ~operator="Int equals", (a, b) => a === b, a, b)
}

module Ast = {
   let isEqualByName = (message, a: Ast.proposition, b: Ast.proposition) =>
    assertion(~message=message, ~operator="Equality.byName", Equality.byName, a, b)
}