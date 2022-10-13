exception UnableToResolveVariable(string);

type operator =
   | Conjunction
   | Disjunction
   | Conditional
   | BiConditional
   | Equivalence
   | Negation

type vectorComponent =
    | Variable(int)
    | Constant(bool)
    | Operator(operator)

type vector = Belt.List.t<vectorComponent>

let variableDebruinjResolver = (name, context) => {
    switch Belt.HashMap.String.get(context, name) {
        | Some(value) =>  value
        | None => raise(UnableToResolveVariable(name))
    }
}

let getVector = (symbolResolver, context, node) => {

    let getBinOpRepresentation = op => switch op  {
        | Ast.Conjunction => Operator(Conjunction)
        | Ast.Disjunction => Operator(Disjunction)
        | Ast.Conditional => Operator(Conditional)
        | Ast.BiConditional => Operator(BiConditional)
        | Ast.Equivalence => Operator(Equivalence)
    }

    let getUnOpRepresentation = op => switch op {
        | Ast.Negation => Operator(Negation)
    }

    let rec toVector = (~vect:vector=list{}, node) =>
      switch (node) {
      | Ast.BinaryOperation(_, op, lhs, rhs) => {
        vect
        ->toVector(~vect=_, lhs)
        ->Belt.List.add(getBinOpRepresentation(op))
        ->toVector(~vect=_, rhs)
      }
      | UnaryOperation(_, op, term) =>
        vect
        ->Belt.List.add(getUnOpRepresentation(op))
        ->toVector(~vect=_, term)
      | Abstraction(_, symb, _) => Belt.List.add(vect, Variable(symbolResolver(symb, context)))
      | Variable(_, name) => Belt.List.add(vect, Variable(symbolResolver(name, context)))
      | Value(_, true) => Belt.List.add(vect, Constant(true))
      | Value(_, false) => Belt.List.add(vect, Constant(false))
      }

      toVector(node)
}

let toVector  = node => getVector(variableDebruinjResolver, Debruinj.getDebruinjIndices(node), node)