open Debruinj
open Clone
open LawApplication
open Formatters

let printLawsFor = state => {
    state
    -> identifyLaws
    -> Belt.Array.forEach(transformation => {
        let (name, ast, _) = transformation.matchedLaw

        Js.Console.log(
            printImplicit(transformation.statementMatched) ++
            " matches " ++ name ++ ": " ++ printImplicit(ast)
        )

        let printSide = transformSide => {
            transformation.statementMatched
            -> getDebruinjIndices
            -> clone(ast, ~targetAlphabet=_)
            -> transformSide
            -> printImplicit
            -> str => Js.Console.log(" suggest: " ++ str)
        }

        switch transformation.matchedSide {
        | LHS => printSide(Ast.getRhs)
        | RHS => printSide(Ast.getLhs)
        }
    })
}


//let x = Parser.parse("a and b or c and not(a or b) and not(a or c) and not(c) and (a or b)")
//let x = Parser.parse("not(a and b) and not(a and b)")
//let x = Parser.parse("a and not(b) or (c and not(b))")
let x = Parser.parse("a and b or not(a and b)")
//let x = Parser.parse("a and (b or c)")

let printSide = side => switch side {
| Laws.LHS => "LHS"
| Laws.RHS => "RHS"
}

let xs = Abstraction.getAbstractions(x)

let print = (l, x) => {
    let (score, _) = Abstraction.getOperationComplexity(x)
    let tabs = switch Js.String2.length(l) {
    | x if x < 15 => "\t\t"
    | _ => "\t"
    }
    Js.Console.log(l ++ ":" ++ tabs ++ Belt.Float.toString(score) ++ "\t" ++  printImplicit(x))
}

print("original", x)
Belt.Array.forEach(xs, (abstraction) => {
    print("abstraction", abstraction)

    let applicableLaws = identifyLaws(abstraction)
    Belt.Array.forEach(applicableLaws, (applicableLaw) => {
        let replacement = Replacement.replace(
            abstraction,
            applicableLaw.statementMatched,
            Laws.getLawAst(applicableLaw.matchedLaw),
            applicableLaw.matchedSide)

        let label = " " ++ Laws.getLawName(applicableLaw.matchedLaw)
        print(label, replacement)
    })
})

original:		16	((a ∧ b) ∨ ¬((a ∧ b)))
abstraction:		16	((a ∧ b) ∨ ¬((a ∧ b)))
 Commutative<and>:	16	((b ∧ a) ∨ ¬((a ∧ b)))
 DeMorgan<not(and)>:	32	((a ∧ b) ∨ (¬(a) ∨ ¬(b)))
 Commutative<and>:	16	((a ∧ b) ∨ ¬((b ∧ a)))
abstraction:		9	([c/(a ∧ b)] ∨ ¬([c/(a ∧ b)]))
 Tautology:		1	⊤
abstraction:		4	([c/(a ∧ b)] ∨ [d/¬((a ∧ b))])
 Commutative<or>:	16	(¬((a ∧ b)) ∨ (a ∧ b))
abstraction:		1	[e/((a ∧ b) ∨ ¬((a ∧ b)))]

//printLawsFor("(T and T)")
//printLawsFor("not(c and d) and not(d or c)")
//printLawsFor("a and b and a")

// a and not(a) <=> F
// c and not(c) <=> F