type uuid = string

type rec propositional =
  | And(uuid, propositional, propositional)
  | Or(uuid, propositional, propositional)
  | Implies(uuid, propositional, propositional)
  | Not(uuid, propositional)
  | Variable(uuid, string)

let makeAnd = (lhs, rhs) => And(Uuid.V4.make(), lhs, rhs)
let makeOr = (lhs, rhs) => Or(Uuid.V4.make(), lhs, rhs)
let makeImplies = (lhs, rhs) => Implies(Uuid.V4.make(), lhs, rhs)
let makeNot = (term) => Not(Uuid.V4.make(), term)
let makeVariable = name => Variable(Uuid.V4.make(), name)