open Ast;
exception KeyNotFoundInAlphabet(string, Belt.HashMap.String.t<int>)
exception KeyNotFoundInInvertedAlphabet(int, Belt.HashMap.Int.t<string>)

/**
    invertHashMap: convert a hashmap of {<k : v>} to a hashmap of {<v : k>}, behavior with duplicate values 
        in the source have undefined behavior in the output.
 */
let invertHashMap = hashmap => {
    let accumulateInvertedMap = (hashmap, invertedMap, key) => {
        switch Belt.HashMap.String.get(hashmap, key) {
        | Some (value) => Belt.HashMap.Int.set(invertedMap, value, key)
        | None => raise(KeyNotFoundInAlphabet(key, hashmap))
        }
        invertedMap
    }

    let accumulateInvertedMapReducer = (agg, x) => accumulateInvertedMap(hashmap, agg, x)

    hashmap
        // get an array of keys
        ->Belt.HashMap.String.keysToArray
        ->Belt.Array.reduce(
            Belt.HashMap.Int.make(~hintSize=10),
            accumulateInvertedMapReducer)
}

let clone = (~targetAlphabet=?, ~sourceAlphabet=?, equation) => {
    /**
        cloneVariable: given the alphabet of variables used in the source proposition (sourceAlphabet) and 
            an alphabet of variables used in a target proposition (targetAlphabet), where
            each alphabet is a hashmap of variable name (string) to its debruinj index.  Conversion is based 
            on the finding the variable in the target alaphabet with the same debruinj index as the variable 
            in the source index 
    */
    let cloneVariable = (name, sourceAlphabet, targetAlphabet) => {
        // invert the target alaphabet so variable name is keyed by debruinj index
        let invertedTargetAlphabet = invertHashMap(targetAlphabet)

        // find the entry for the variable name in the source alphabet]
        switch Belt.HashMap.String.get(sourceAlphabet, name) {
        | Some(index) => {
            // using the debruinj index in the source alphabet, retrieve the new variable name be the debruinj index of the target alphabet 
            switch (Belt.HashMap.Int.get(invertedTargetAlphabet, index)) {
            | Some (newName) => makeVariable(newName)
            | None => raise(KeyNotFoundInInvertedAlphabet(index, invertedTargetAlphabet))
            }   
        }
        // we should never get here, since the equationAlphabet is derived from the variable names
        | None => raise(KeyNotFoundInAlphabet(name, sourceAlphabet))
        }
    }

    // get the alphabet of the equation
    let sourceAlphabet = Belt.Option.getWithDefault(sourceAlphabet, Debruinj.getDebruinjIndices(equation))

    // use target alphabet provided OR default to current alphabet
    let targetAlphabet = Belt.Option.getWithDefault(targetAlphabet, sourceAlphabet)

    // recursive _clone function that doesn't need to pass: originalIndices, newIndices
    //   to make code more readable
    let rec _clone = (node) =>
      switch (node) {
      | BinaryOperation(_, operator, lhs, rhs) => makeBinaryOperation(operator, _clone(lhs), _clone(rhs))
      | Abstraction(_, symb, prop) => makeAbstraction(symb, _clone(prop))
      | UnaryOperation(_, op, term) => makeUnaryOperation(op, _clone(term))
      | Variable(_, name) => cloneVariable(name, sourceAlphabet, targetAlphabet)
      | Value(_, boolean) => makeValue(boolean)
      };

    // apply the recursive function
    _clone(equation)
}