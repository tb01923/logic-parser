open Ast
exception TupleProblem(string)
let increment = a => a + 1

let pushOnto = (a, o) => {
    Belt.Array.push(a, o)
    a
}

let first = (tuple) => {
    let (fst, _) = tuple
    fst
}

let second = (tuple) => {
    let (_, snd) = tuple
    snd
}

// the max value between two optionals
let maxOptionals = (a, b) =>
    switch (a, b) {
    | (Some(a1), Some(b1)) if a1 >= b1 => Some(a1)
    | (Some(_), Some(b1)) => Some(b1)
    | (Some(a1), None) => Some(a1)
    | (None, Some(b1)) => Some(b1)
    | (None, None) => Some(-1)
    }

// getNextDebuinjIndex:
//      given a hash map of known variables and their index positions, determine what the next index positin 
//      should be. e.g., { 'a': 0, 'b': 1 } should return 1 and  { 'a': 0, 'b': 1, 'c': 2 } should return 3
let getNextDebuinjIndex = knownVariables =>
    knownVariables
    // get the keys
    ->Belt.HashMap.String.keysToArray
    // from the keys get the values (will be an optional(value))
    ->Js.Array2.map(key => Belt.HashMap.String.get(knownVariables, key))
    // find the max optional(value)
    ->Js.Array2.reduce(maxOptionals, Some(-1))
    // return that max if one exists otherwise return -1 (default for 0 based indexes)
    ->Belt.Option.getWithDefault(-1)
    // increment the prior max
    ->increment

let addSymbolToIndices = (indices, name) => {
    let id = (_, x) => x
    switch Belt.HashMap.String.has(indices, name) {
    | true => indices
    | false => getNextDebuinjIndex(indices)->Belt.HashMap.String.set(indices, name, _)->id(indices)
    }
}

/**
getNextSymbol: 
     given a hashmap of variable names to index positions, determine what the next variable name will be
     for instance  { 'a': 0, 'b': 1 } should return 'c' and  { 'a': 0, 'b': 1, 'c': 2 } should return 'd'
     only works for single character variable names
*/
let getNextSymbol = indices => {
    let max = (a, b) =>
        switch a > b {
        | true => a
        | false => b
        }

    let increment = x => Belt.Float.toInt(x) + 1

    indices
    // look at the keys of the hasmap
    ->Belt.HashMap.String.keysToArray
    // find the max key
    ->Belt.Array.reduce("", max)
    // translate to the charcode (e.g., ascii value)
    ->Js.String2.charCodeAt(0)
    // increment the value
    ->increment
    // convert it back to a character
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
    | BinaryOperation(_, _, lhs, rhs) => variableNames->getVariableNames(lhs)->getVariableNames(rhs)
    | UnaryOperation(_, _, term) => getVariableNames(variableNames, term)
    | Variable(_, name) => pushOnto(variableNames, name)
    | Abstraction(_, _, prop) => getVariableNames(variableNames, prop)
    | Value(_, _) => variableNames
    }

    /*
    rec getDebruinjIndices: recursively walk the AST and collect all the variable names in the order they are 
         introduced.  This requires sepcial action on only two of the types (the Operations are just recursive 
         walks, and the Value is a no-op).  The variable type has its variable name collected and index recorded
         (if it is the first time the variable has been seen). The Abstraction is the most complicated in that 
         the Symbol representing the abstraction needs to be added / indexed, but the details of the 
         proposition  being abstracted need to be deferred for Variable name collection / indexing.
    */
    let rec getDebruinjIndices = (aggregate, node) => switch node {
    | BinaryOperation(_, _, lhs, rhs) => getDebruinjIndices(aggregate,lhs)->getDebruinjIndices(rhs)
    | UnaryOperation(_, _, term) => getDebruinjIndices(aggregate, term)
    | Variable(_, name) => (addSymbolToIndices(first(aggregate), name), second(aggregate))
    | Abstraction(_, symb, prop) => (addSymbolToIndices(first(aggregate), symb), getVariableNames(second(aggregate), prop))
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