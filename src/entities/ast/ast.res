type uuid = string
type symbol = string


// todo: support all 16 binary operations: https://en.wikipedia.org/wiki/Truth_table
type binaryOperator =
   | Conjunction
   | Disjunction
   | Conditional
   | BiConditional
   // support equivalence operator for laws
   | Equivalence

type unaryOperator =
   | Negation

type rec proposition =
  | BinaryOperation(uuid, binaryOperator, proposition, proposition)
  | UnaryOperation(uuid, unaryOperator, proposition)
  | Abstraction(uuid, symbol, proposition)
  | Variable(uuid, symbol)
  | Value(uuid, bool)

exception NotBinaryOperation(proposition)

let getId = node => switch node {
  | BinaryOperation(uuid, _, _, _) => uuid
  | UnaryOperation(uuid, _, _) => uuid
  | Abstraction(uuid, _, _) => uuid
  | Variable(uuid, _) => uuid
  | Value(uuid, _) => uuid
}

let makeConjunction = (lhs, rhs) => BinaryOperation(Uuid.V4.make(), Conjunction, lhs, rhs)
let makeDisjunction = (lhs, rhs) => BinaryOperation(Uuid.V4.make(), Disjunction, lhs, rhs)
let makeConditional = (lhs, rhs) => BinaryOperation(Uuid.V4.make(), Conditional, lhs, rhs)
let makeBiConditional = (lhs, rhs) => BinaryOperation(Uuid.V4.make(), BiConditional, lhs, rhs)
let makeEquivalence = (lhs, rhs) => BinaryOperation(Uuid.V4.make(), Equivalence, lhs, rhs)
let makeBinaryOperation = (operator, lhs, rhs) => switch operator {
    | Conjunction => makeConjunction(lhs, rhs)
    | Disjunction => makeDisjunction(lhs, rhs)
    | Conditional => makeConditional(lhs, rhs)
    | BiConditional => makeBiConditional(lhs, rhs)
    | Equivalence => makeEquivalence(lhs, rhs)
}
let makeNegation = (term) => UnaryOperation(Uuid.V4.make(), Negation, term)
let makeUnaryOperation = (operator, term) => switch operator {
    | Negation => makeNegation(term)
}
let makeVariable = name => Variable(Uuid.V4.make(), name)
let makeValue = b => Value(Uuid.V4.make(), b)
let makeAbstraction = (symb, prop) => Abstraction(Uuid.V4.make(), symb, prop)
let getLhs = op => switch op {
   | BinaryOperation(_, _, lhs, _) => lhs
   | _ => raise(NotBinaryOperation(op))
}
let getRhs = op => switch op {
   | BinaryOperation(_, _, _, rhs) => rhs
   | _ => raise(NotBinaryOperation(op))
}

let rec hasAbstraction = ast => switch ast {
| BinaryOperation(_, _, lhs, rhs) => hasAbstraction(lhs) || hasAbstraction(rhs)
| UnaryOperation(_, _, t) => hasAbstraction(t)
| Abstraction(_) => true
| _ => false
}
