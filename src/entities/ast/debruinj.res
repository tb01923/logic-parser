open Ast
let increment = a => a + 1

let maxOptionals = (a, b) =>
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
    ->Js.Array2.reduce(maxOptionals, Some(-1))
    ->Belt.Option.getWithDefault(-1)
    ->increment

let addSymbolToIndices = (indices, name) => {
    let id = (_, x) => x
    switch Belt.HashMap.String.has(indices, name) {
    | true => indices
    | false => getNextDebuinjIndex(indices)->Belt.HashMap.String.set(indices, name, _)->id(indices)
    }
}

let getNextSymbol = indices => {
    let max = (a, b) =>
        switch a > b {
        | true => a
        | false => b
        }

    let increment = x => Belt.Float.toInt(x) + 1

    indices
    ->Belt.HashMap.String.keysToArray
    ->Belt.Array.reduce("", max)
    ->Js.String2.charCodeAt(0)
    ->increment
    ->Js.String2.fromCharCode
}

/**
    getDebruinjIndices: given @node:proposition return a hashmap of { string: int } values for each variables debruinj index
        variables are given indexes as they are introduced left to right.  for example `a and b` will result in the hashmap
        { 'a': 0, 'b': 1 } while the propostion `b or a` result in the hashmap { 'b': 0, 'a': 1 } 
 */
let getDebruinjIndices = node => {

    // walk the Ast.proposition and collect the names of variables within
    let rec getVariableNames = (variableNames, node) => switch node {
    | BinaryOperation(_, _, lhs, rhs) => variableNames
        ->getVariableNames(lhs)
        ->getVariableNames(rhs)
    | UnaryOperation(_, _, term) => getVariableNames(variableNames, term)
    | Variable(_, name) => {
        Belt.Array.push(variableNames, name)
        variableNames
    }
    | Abstraction(_, _, prop) => getVariableNames(variableNames, prop)
    | Value(_, _) => variableNames
    }

    let rec getDebruinjIndices = (aggregate, node) => switch node {
    | BinaryOperation(_, _, lhs, rhs) => aggregate
        ->getDebruinjIndices(lhs)
        ->getDebruinjIndices(rhs)
    | UnaryOperation(_, _, term) => getDebruinjIndices(aggregate, term)
    | Variable(_, name) => {
        let (indexMap, deferredVariableNames) = aggregate
        (addSymbolToIndices(indexMap, name), deferredVariableNames)
      }
    | Abstraction(_, symb, prop) => {
        // with an abstraction we want th debruinj of the abstraction's name, and to defer 
        //      collecting the debruinj index of the variables within the abstraction until the end
        let (indexMap, deferredVariableNames) = aggregate
        (addSymbolToIndices(indexMap, symb), getVariableNames(deferredVariableNames, prop))
      }
    | Value(_, _) => aggregate
    }

    // the initial value we want to build upon is a hashmap of { <variable-name : debruinj-index > } 
    //      and an array for variables defined within abstractions
    let initial = (Belt.HashMap.String.make(~hintSize=10), [])
    // indices will contain a hashmap of  { <variable-name : debruinj-index > } while the 
    //      deferred will contain the remaining variables contained within abstractions
    //      that need to be added to the hashmap 
    let (indices, deferred) = getDebruinjIndices(initial, node)
    // reduce the deferred variables and their debruinj index onto the hashmap
    let allIndices = Belt.Array.reduce(deferred, indices, addSymbolToIndices)
    // return the hashmap of  { <variable-name : debruinj-index > }
    allIndices
}