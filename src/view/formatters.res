exception UnableToResolveVariable(string);

let variableNameResolver = (name, _) => name

let variableDebruinjResolver = (name, context) => {
    switch Belt.HashMap.String.get(context, name) {
        | Some(value) =>  Js.String2.make(value)
        | None => raise(UnableToResolveVariable(name))
    }
}

let implicitString = (variableResolver, context, node) => {
    open Ast
    let getOperator = operator => switch operator {
        | Conjunction => "∧"
        | Disjunction => "∨"
        | Conditional => "->"
        | BiConditional => "<=>"
    }

    let rec _implicitString = node =>
      switch (node) {
      | BinaryOperation(_, operator, lhs, rhs) => {
        operator
        ->getOperator
        ->printBinary(lhs, rhs)
      }
      | Negate(_, term) => printUnary("¬", term)
      | Variable(_, name) => variableResolver(name, context)
      | Value(_, true) => "⊤"
      | Value(_, false) => "⊥"
      }
    and printBinary = (op, lhs, rhs) => {
      "(" ++ _implicitString(lhs) ++ " " ++ op ++ " " ++ _implicitString(rhs) ++ ")";
    }
    and printUnary = (op, term) => {
      op ++ "(" ++ _implicitString(term) ++ ")";
    };
    _implicitString(node)
}

let printImplicit = node => implicitString(variableNameResolver, None, node)
let printImplicitDeBruinj = node => implicitString(variableDebruinjResolver, Debruinj.getDebruinjIndices(node), node)



//let a = Parser.parse("a and (b or e) and d -> c <=> v");
//Js.Console.log(printImplicitStatement(a));
//Js.Console.log(printImplicitDeBruinj(a));