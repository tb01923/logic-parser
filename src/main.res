open Debruinj
open Clone
open LawApplication
open Formatters

let printLawsFor = str => {
    str
    -> Parser.parse
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

//printLawsFor("(T and T)")
//printLawsFor("not(c and d) and not(d or c)")
printLawsFor("a and b and a")

// a and not(a) <=> F
// c and not(c) <=> F