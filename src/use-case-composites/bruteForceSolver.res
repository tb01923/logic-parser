type solutionStep =
    AbstractionStep
    | ApplicationStep(LawApplication.transformation)
    | TransformationResult
    | Initial
    | Trace (string)

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

let initializeHistory = statement =>[(Initial, statement)]

let solve = statement => {

    let chainIntoNext = ((statement, history)) => next(statement, history)

    let rec invokeCallAndIterate = (i, previousIteratorResultsOpt) => {
        let previousIteratorResults = Belt.Option.getWithDefault(previousIteratorResultsOpt, [])

        let nextResults = switch previousIteratorResultsOpt {
        | None => next(statement, initializeHistory(statement))
        | Some(_) => Belt.Array.flatMap(previousIteratorResults, chainIntoNext)
        }

        let n = switch Belt.Array.length(nextResults) {
        | 0 => 0
        | _ => i - 1
        }

        let aggregateResults = Belt.Array.concat(nextResults, previousIteratorResults)

        iterate(n, Some(aggregateResults))
    }
    and iterate = (i, previousIteratorResultsOpt) => {
        switch (previousIteratorResultsOpt, i)  {
        | (None, 0) => Some([(statement, initializeHistory(statement))])
        | (Some(_), 0) => previousIteratorResultsOpt
        | (None, _) => invokeCallAndIterate(i, previousIteratorResultsOpt)
        | (Some(_), _) => invokeCallAndIterate(i, previousIteratorResultsOpt)
        }
    }
    and iterateOld  = (i, prior) => {
        switch (prior, i) {
        | (None, 0) => Some([(statement, initializeHistory(statement))])
        | (Some(_), 0) => prior
        | (None, _) => {
            let it = next(statement, initializeHistory(statement))
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
            iterate(n, Some(all))
        }
    }}

    let res = iterate(1, None)

    res
    ->Belt.Option.getExn
    ->Belt.Array.keepMap(((statement, history)) => switch Ast.hasAbstraction(statement) {
    | true => None
    | false => Some((statement, history))
    })
}
