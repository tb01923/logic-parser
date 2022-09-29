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
//let x = Parser.parse("not(a and b) and not(b or c)")
//let x = Parser.parse("a and not(b) or (c and not(b))")
let x = Parser.parse("a and b or b and c")


let xs = Abstraction.getAbstractions(x)
Belt.Array.forEach(xs, (x) => {
    let str = "Abstraction: " ++ x->printImplicit ++ " is expressed as " ++ x->printImplicitDeBruinj
    Js.Console.log(str)
    printLawsFor(x)
    Js.Console.log("---------------------------")
})



//printLawsFor("(T and T)")
//printLawsFor("not(c and d) and not(d or c)")
//printLawsFor("a and b and a")

// a and not(a) <=> F
// c and not(c) <=> F