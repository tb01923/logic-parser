type solutionStep =
    AbstractionStep
    | ApplicationStep(LawApplication.transformation)
    | TransformationResult
    | Initial
    | Trace (string)

let getLabel = tag => switch tag {
    | AbstractionStep => "abstraction"
    | ApplicationStep({ matchedLaw, matchedSide, statementMatched }) => {
        let (name, ast, bidirectional) = matchedLaw
        name
    }
    | TransformationResult => "transformation"
    | Initial => "initial"
    | Trace (s) => s
}

let print = (step, x) => {
    let score = Heuristic.variablesRaisedToOperations(x)
    let l = getLabel(step)
    let tabs = switch Js.String2.length(l) {
    | x if x < 6 => "\t\t\t"
    | x if x < 15 => "\t\t"
    | _ => "\t"
    }
    Js.Console.log(l ++ ":" ++ tabs ++ Belt.Float.toString(score) ++ "\t" ++  StringRepresentation.printImplicit(x))
}




let visitedStatements : Belt.HashMap.String.t<bool> = Belt.HashMap.String.make(~hintSize=10)

let next = (statement, history) => {
    // get all the abstractions
    Abstraction.getAbstractions(statement)
    ->Belt.Array.flatMap(abstraction => {
        // for each abstraction get all appplicable laws
//        Js.Console.log("Abstraction: " ++ StringRepresentation.printImplicit(abstraction))
        LawApplication.identifyLaws(abstraction)
        ->Belt.Array.keepMap(applicableLaw => {

            // apply law tp current abstraction, providing a form for the next epoch
            let nextStatement = Replacement.replace(
                abstraction,
                applicableLaw.statementMatched,
                LawApplication.getLawAst(applicableLaw.matchedLaw),
                applicableLaw.matchedSide)

            // if we have seen this form prior, ignore it otherwise add the abstraction
            //  step and transformation to the historical view
            let myHash = StringRepresentation.hash(nextStatement)
            switch Belt.HashMap.String.has(visitedStatements, myHash) {
            | true =>  None
            | false => {
                Belt.HashMap.String.set(visitedStatements, myHash, true)

                let thisHistory = Belt.Array.copy(history)

                // only push abstraction if it differs from statement
                switch Equality.byDebruinj(statement, abstraction) {
                | true => ()
                | false => Belt.Array.push(thisHistory, (AbstractionStep, abstraction))
                }

                // todo: perhaps push the matching sub-statement onto this portion of the history
                Belt.Array.push(thisHistory, (
                    ApplicationStep(applicableLaw),
                    LawApplication.getLawAst(applicableLaw.matchedLaw)))
                // todo consider adding this
                Belt.Array.push(thisHistory, (TransformationResult, nextStatement))

                Some(nextStatement, thisHistory)
            }}
        })
    })
}

let solve = statement => {

    let rec iterate  = (i, prior) => {
//        Js.Console.log("ITERATING("++ Js.String2.make(i) ++ ")")
        switch (prior, i) {
        | (None, 0) => Some([(statement, [(Initial, statement)])])
        | (Some(_), 0) => prior
        | (None, _) => {
            let it = next(statement, [(Initial, statement)])

//            it
//            ->Belt.Array.map(((s, _)) => s)
//            ->Belt.Array.map(StringRepresentation.printImplicit)
//            ->Js.Console.log

            iterate(i - 1, Some(it))
        }
        | (Some(priorIt), _) => {
            let it = priorIt
            ->Belt.Array.flatMap(((statement, history)) => next(statement, history))

            // if there is nothing next, sort circuit iterations by moving i to 0
            let n = switch Belt.Array.length(it) {
            | 0 => 0
            | _ => i -1
            }

            let all = Belt.Array.concat(it, priorIt)

//            all
//            ->Belt.Array.map(((s, _)) => s)
//            ->Belt.Array.map(StringRepresentation.printImplicit)
//            ->Js.Console.log

            iterate(n, Some(all))
        }
    }}

    let res = iterate(6, None)

    res
    ->Belt.Option.getExn
    ->Belt.Array.keepMap(((statement, history)) => switch Ast.hasAbstraction(statement) {
    | true => None
    | false => Some((statement, history))
    })
}


"not(a) and a"
//"not(a and b) and not(a and b)"
//"a and b or not(a and b)"
//"a and a and a and a and a"
//"p or q and p"
->Parser.parse
->(ast => {
    StringRepresentation.printImplicit(ast)->Js.Console.log
    Js.Console.log("_________")
    ast
})
->solve
->Belt.Array.map(((statement, history)) => (Heuristic.variablesRaisedToOperations(statement), statement, history))
->Belt.SortArray.stableSortBy(((scA, _, _), (scB, _, _)) => Belt.Float.toInt(scA -. scB))
->(arr => arr[0])
->((_, _, history)) => history
->Belt.Array.map(((step, statement)) => print(step, statement) )
->ignore