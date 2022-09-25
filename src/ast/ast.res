type uuid = string

type binaryOperator =
   | Conjunction
   | Disjunction
   | Conditional
   | BiConditional

type rec propositional =
  | BinaryOperation(uuid, binaryOperator, propositional, propositional)
  | Negate(uuid, propositional)
  | Variable(uuid, string)
  | Value(uuid, bool)

exception NotBinaryOperation(propositional)

let makeConjunction = (lhs, rhs) => BinaryOperation(Uuid.V4.make(), Conjunction, lhs, rhs)
let makeDisjunction = (lhs, rhs) => BinaryOperation(Uuid.V4.make(), Disjunction, lhs, rhs)
let makeConditional = (lhs, rhs) => BinaryOperation(Uuid.V4.make(), Conditional, lhs, rhs)
let makeBiConditional = (lhs, rhs) => BinaryOperation(Uuid.V4.make(), BiConditional, lhs, rhs)
let makeBinaryOperation = (operator, lhs, rhs) => switch operator {
    | Conjunction => makeConjunction(lhs, rhs)
    | Disjunction => makeDisjunction(lhs, rhs)
    | Conditional => makeConditional(lhs, rhs)
    | BiConditional => makeBiConditional(lhs, rhs)
}
let makeNegate = (term) => Negate(Uuid.V4.make(), term)
let makeVariable = name => Variable(Uuid.V4.make(), name)
let makeValue = b => Value(Uuid.V4.make(), b)
let getLhs = op => switch op {
   | BinaryOperation(_, _, lhs, _) => lhs
   | _ => raise(NotBinaryOperation(op))
}
let getRhs = op => switch op {
   | BinaryOperation(_, _, _, rhs) => rhs
   | _ => raise(NotBinaryOperation(op))
}