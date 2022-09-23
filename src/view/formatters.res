let rec print = node =>
  switch (node) {
  | Ast.And(_, lhs, rhs) => printBinary("∧", lhs, rhs)
  | Ast.Or(_, lhs, rhs) => printBinary("∨", lhs, rhs)
  | Ast.Implies(_, lhs, rhs) => printBinary("->", lhs, rhs)
  | Ast.BiConditional(_, lhs, rhs) => printBinary("<=>", lhs, rhs)
  | Ast.Not(_, term) => printUnary("¬", term)
  | Ast.Variable(_, name) => name
  | Ast.Value(_, true) => "⊤"
  | Ast.Value(_, false) => "⊥"
  }
and printBinary = (op, lhs, rhs) => {
  "(" ++ print(lhs) ++ " " ++ op ++ " " ++ print(rhs) ++ ")";
}
and printUnary = (op, term) => {
  op ++ "(" ++ print(term) ++ ")";
};

let a = Parser.parse("a and (b or e) and d -> c <=> v");
Js.Console.log(print(a));