open Ast;

exception KeyNotFoundInAlphabet(string, Belt.HashMap.String.t<int>)
exception KeyNotFoundInInvertedAlphabet(int, Belt.HashMap.Int.t<string>)

let invertHashMap = hashmap =>
    hashmap
        // get an array of keys
        ->Belt.HashMap.String.keysToArray
        ->Belt.Array.reduce(
            Belt.HashMap.Int.make(~hintSize=10),
            (invertedMap, key) => {
                switch Belt.HashMap.String.get(hashmap, key) {
                    | Some (value) => Belt.HashMap.Int.set(invertedMap, value, key)
                    | None => raise(KeyNotFoundInAlphabet(key, hashmap))
                 }
                 invertedMap
             })

let cloneVariable = (name, equationAlphabet, targetAlphabet) => {
    // invert the hashmap to key by the index, and have value be the variable name
    let invertedTargetAlphabet = invertHashMap(targetAlphabet)
    // look up the variable name and get its index in the equation alphabet
    switch Belt.HashMap.String.get(equationAlphabet, name) {
    // from the (inverted) target alphabet use that index to get the new variable name
    | Some(index) => switch (Belt.HashMap.Int.get(invertedTargetAlphabet, index)) {
        | Some (newName) => makeVariable(newName)
        // if the target index doesn't contain a variable at that index, thats a problem
        | None => raise(KeyNotFoundInInvertedAlphabet(index, invertedTargetAlphabet))
    }
    // we should never get here, since the equationAlphabet is derived from the variable names
    | None => raise(KeyNotFoundInAlphabet(name, equationAlphabet))
   }
}

let clone = (equationAlphabet, targetAlphabet) => {
    // recursive _clone function that doesn't need to pass: originalIndices, newIndices
    //   to make code more readable
    let rec _clone = (node) =>
      switch (node) {
      | And(_, lhs, rhs) => makeAnd(_clone(lhs), _clone(rhs))
      | Or(_, lhs, rhs  ) => makeOr(_clone(lhs), _clone(rhs))
      | Implies(_, lhs, rhs) => makeImplies(_clone(lhs), _clone(rhs))
      | BiConditional(_, lhs, rhs) => makeBiConditional(_clone(lhs), _clone(rhs))
      | Not(_, term) => makeNot(_clone(term))
      | Variable(_, name) => cloneVariable(name, equationAlphabet, targetAlphabet)
      | Value(_, boolean) => makeValue(boolean)
      };

    // return recursive function
    _clone
}

let clone = (~targetAlphabet=?, equation) => {
    // get the alphabet of the equation
    let equationAlphabet = Debruinj.getDebruinjIndices(equation)

    // use target alphabet provided OR default to current alphabet
    let targetAlphabet = Belt.Option.getWithDefault(targetAlphabet, equationAlphabet)

    // invoke
    clone(equationAlphabet, targetAlphabet, equation);
};

let hm = Belt.HashMap.String.make(~hintSize=10)
Belt.HashMap.String.set(hm, "p", 0)
Belt.HashMap.String.set(hm, "q", 1)

let ast = makeAnd( makeVariable("a"), makeVariable("b"))
let ast2 = clone(ast, ~targetAlphabet=hm)
