let printLawsFor = str => {
    str
    -> Parser.parse
    -> Laws.identifyLaws
    -> Belt.Array.forEach(transformation => {
        let (name, ast, _) = transformation.matchedLaw

        Js.Console.log(
            Formatters.printImplicit(transformation.statementMatched) ++
            " matches " ++ name ++ ": " ++ Formatters.printImplicit(ast)
        )

        let printSide = transformSide => {
            transformation.statementMatched
            -> Debruinj.getDebruinjIndices
            -> Clone.clone(ast, ~targetAlphabet=_)
            -> transformSide
            -> Formatters.printImplicit
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
printLawsFor("a and b and c and a")