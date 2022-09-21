open Ast;

exception KeyNotFoundInAlphabet(string, Belt.HashMap.String.t<int>)
exception KeyNotFoundInInvertedAlphabet(int, Belt.HashMap.Int.t<string>)

let invert = (alphabet: Belt.HashMap.String.t<int>) : Belt.HashMap.Int.t<string> => {
  let invertedMap = Belt.HashMap.Int.make(~hintSize=10);

  alphabet
  ->Belt.HashMap.String.keysToArray
  ->Belt.Array.map((key: string) =>
    switch Belt.HashMap.String.get(alphabet, key) {
        | Some (value: int) => Belt.HashMap.Int.set(invertedMap, value, key)
        | None => raise(KeyNotFoundInAlphabet(key, alphabet))
    })
  ->ignore

  invertedMap;
};

let cloneVariable = (name, originalIndices, newIndices) => {
   let invertedNew = invert(newIndices)
   switch Belt.HashMap.String.get(originalIndices, name) {
     | Some(index) => switch (Belt.HashMap.Int.get(invertedNew, index)) {
        | Some (newName) => makeVariable(newName)
        | None => raise(KeyNotFoundInInvertedAlphabet(index, invertedNew))
     }
     | None => raise(KeyNotFoundInAlphabet(name, originalIndices))
   }
}

let rec clone = (node, originalIndices, newIndices) =>
  switch (node) {
  | And(_, lhs, rhs) => makeAnd(clone(lhs, originalIndices, newIndices), clone(rhs, originalIndices, newIndices))
  | Or(_, lhs, rhs  ) => makeOr(clone(lhs, originalIndices, newIndices), clone(rhs, originalIndices, newIndices))
  | Implies(_, lhs, rhs) =>
    makeImplies(clone(lhs, originalIndices, newIndices), clone(rhs, originalIndices, newIndices))
  | Not(_, term) => makeNot(clone(term, originalIndices, newIndices))
  | Variable(_, name) => cloneVariable(name, originalIndices, newIndices)
  };

let f = (a: int, b: int) =>
    let rec r = (c: int) => {
        a + b + c
    }
    r

let someR = f(2,3)




let clone = (~alphabet=?, node) => {
    let originalIndices = Debruinj.getDebruinjIndices(node)
    let newIndices = switch(alphabet) {
    | Some(alpha) => alpha
    | None => originalIndices
    }

  clone(node, originalIndices, newIndices);
};

let ast1 = makeAnd(makeVariable("a"), makeVariable("b"));
let as2 = clone(ast1);
let hm = Belt.HashMap.String.make(~hintSize=10);
Belt.HashMap.String.set(hm, "p", 0);
Belt.HashMap.String.set(hm, "q", 1);
let hm2 = invert(hm)
let p = Belt.HashMap.Int.get(hm2, 0)
let q = Belt.HashMap.Int.get(hm2, 1)
let ast3 = clone(ast1, ~alphabet=hm);