open Ast
exception UnableToResolveVariable(string);

let variableNameResolver = (name, _) => name

let variableDebruinjResolver = (name, context) => {
    switch Belt.HashMap.String.get(context, name) {
        | Some(value) =>  Js.String2.make(value)
        | None => raise(UnableToResolveVariable(name))
    }
}


let getBinOpName = operator => switch operator {
        | Conjunction => "Conjunction"
        | Disjunction => "Disjunction"
        | Conditional => "Conditional"
        | BiConditional => "BiConditional"
        | Equivalence => "Equivalence"
    }


let implicitString = (symbolResolver, context, node) => {

    let getBinOpSymbol = operator => switch operator {
        | Conjunction => "∧"
        | Disjunction => "∨"
        | Conditional => "->"
        | BiConditional => "<=>"
        | Equivalence => "≡"
    }    

    let getUnOpSymbol = operator => switch operator {
        | Negation => "¬"
    }

    let rec implicitString = node =>
      switch (node) {
      | BinaryOperation(_, operator, lhs, rhs) => {
        operator
        ->getBinOpSymbol
        ->printBinary(lhs, rhs)
      }
      | UnaryOperation(_, operator, term) =>
        operator
        ->getUnOpSymbol
        ->printUnary(term)
      | Abstraction(_, symb, prop) => printAbstraction(symb, prop)
      | Variable(_, name) => symbolResolver(name, context)
      | Value(_, true) => "⊤"
      | Value(_, false) => "⊥"
      }
    and printBinary = (op, lhs, rhs) => {
      "(" ++ implicitString(lhs) ++ " " ++ op ++ " " ++ implicitString(rhs) ++ ")";
    }
    and printUnary = (op, term) => {
        // not operator shouldn't use parens
      //op ++ "(" ++ implicitString(term) ++ ")";
      op ++ implicitString(term) ;
    }
    and printAbstraction = (symb, prop) => {
     // this isn't the right way to express the substitution, but it works okay for now
     "[" ++ symbolResolver(symb, context) ++ "/" ++ implicitString(prop) ++ "]"
    };
    implicitString(node)
}

let printImplicit = node => implicitString(variableNameResolver, None, node)
let printImplicitDeBruinj = node => implicitString(variableDebruinjResolver, Debruinj.getDebruinjIndices(node), node)

let hash = node =>
    node
    -> printImplicit
//    -> ReScriptHash.Sha256.make
