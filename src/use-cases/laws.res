/*
TODO: three variable laws not working over statements with only two vars (e.g., "(a and b) and a")
TODO: non biconditional laws: modus ponens, modus tollens
*/

open Ast
exception ExpectingEquivalence(string)

type law = (string, Ast.proposition, bool)

/**
    makeLaw: make a `law` by converting a string that parsed into an `equivalence` `ast`
        @bidirectional indicates that this law can be matched on eithr LHS or RHS of the
        equivalence. Some laws do not make sense to be bidirectional as only one direction
        can possibly simplify the problem: `T and T = T` is an example.  If bidirection 
        was applied the possible next moves in reduction increase exponentially, e.g.,

          `T` can be replaced by `T and T` then
          `T and T` can be replaced by `T and T and T and T` then... 
        
        essentially any law that resolves to a single Boolean or single Variable should 
        _not_ be bidirectional 
*/
let makeLaw = (~bidirectional=true, name, propositionString) => {
  let ast = Parser.parse(propositionString)
  switch ast {
  | BinaryOperation(_, Equivalence, _, _) => (name, ast, bidirectional)
  | _ => raise(ExpectingEquivalence(propositionString))
  }
}

let laws = [
    // should these be elsewhere
    makeLaw("truth-table-and", "T and T = T", ~bidirectional=false),
    makeLaw("truth-table-and", "T and F = F", ~bidirectional=false),
    makeLaw("truth-table-and", "F and T = F", ~bidirectional=false),
    makeLaw("truth-table-and", "F and F = F", ~bidirectional=false),
    makeLaw("truth-table-or", "T or T = T", ~bidirectional=false),
    makeLaw("truth-table-or", "T or F = T", ~bidirectional=false),
    makeLaw("truth-table-or", "F or T = T", ~bidirectional=false),
    makeLaw("truth-table-or", "F or F = F", ~bidirectional=false),
    makeLaw("truth-table-implies", "T -> T = T", ~bidirectional=false),
    makeLaw("truth-table-implies", "T -> F = F", ~bidirectional=false),
    makeLaw("truth-table-implies", "F -> T = T", ~bidirectional=false),
    makeLaw("truth-table-implies", "F -> F = T", ~bidirectional=false),
    makeLaw("truth-table-biconditional", "T <=> T = T", ~bidirectional=false),
    makeLaw("truth-table-biconditional", "T <=> F = F", ~bidirectional=false),
    makeLaw("truth-table-biconditional", "F <=> T = F", ~bidirectional=false),
    makeLaw("truth-table-biconditional", "F <=> F = T", ~bidirectional=false),
    makeLaw("truth-table-negate", "not F = T", ~bidirectional=false),
    makeLaw("truth-table-negate", "not T = F", ~bidirectional=false),

    // this group doesn't make sense to reverse, makes problems more complicated
    makeLaw("Domination<or>", "p or T = T", ~bidirectional=false),
    makeLaw("Domination<and>", "p and F = F", ~bidirectional=false),
    makeLaw("Identity<or>", "p or F = p", ~bidirectional=false),
    makeLaw("Identity<and>", "p or T = p", ~bidirectional=false),
    makeLaw("Idempotence<or>", "p or p = p", ~bidirectional=false),
    makeLaw("Idempotence<and>", "p and p = p", ~bidirectional=false),
    makeLaw("Tautology<bi>", "p <=> p = p", ~bidirectional=false),
    makeLaw("Tautology<imp>", "p -> p = T", ~bidirectional=false),
    makeLaw("Double Negation", "not not p = p", ~bidirectional=false),
    makeLaw("Absorbtion<or>", "p or (p and q) = p", ~bidirectional=false),
    makeLaw("Absorbtion<and>", "p and (p or q) = p", ~bidirectional=false),
    makeLaw("Complement<or>", "p or not(p) = T", ~bidirectional=false),
    makeLaw("Complement<and>", "p and not(p) = F", ~bidirectional=false),

    // this group can be applied in both directions
    makeLaw("Commutative<and>", "p and q = q and p"),
    makeLaw("Commutative<or>", "p or q = q or p"),
    makeLaw("DeMorgan<not(or)>", "not(p or q) = not(p) and not(q)"),
    makeLaw("DeMorgan<not(and)>", "not(p and q) = not(p) or not(q)"),
    makeLaw("Associative<and>", "(p and q) and r = p and (q and r)"),
    makeLaw("Associative<or>", "(p or q) or r = p or (q or r)"),
    makeLaw("Distributive<or(and)>", "p or (q and r) = (p or q) and (p or r)"),
    makeLaw("Distributive<and(or)>", "p and (q or r) = (p and q) or (p and r)"),

    // new
    makeLaw("biconditional equivalence", "(p and q) or (not(p) and not(q)) = p <=> q"),
    makeLaw("implication equivalence", "p or not(q) = p -> q")
]

let hasName = name1 => ((name2, _, _)) => name1 == name2
let getLawByName = name => Js.Array2.find(laws, hasName(name))