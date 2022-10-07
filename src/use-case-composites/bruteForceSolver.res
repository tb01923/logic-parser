type solutionStep =
    AbstractionStep
    | ApplicationStep(LawApplication.transformation)
    | TransformationResult
    | Initial
    | Trace (string)

let numberOfIterations = 4

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

let shouldShortCircuit = (nextResults) => {
    // if there aere
    let containsOptimal = nextResults
    ->Belt.Array.map(((a, _)) => a)
    ->Belt.Array.keep(Heuristic.isOptimalSolution)
    ->Belt.Array.length
    ->(l => l > 0)

    let shortCircuit =  (Belt.Array.length(nextResults) === 0) || containsOptimal
    shortCircuit
}

let solve = statement => {

    let chainIntoNext = arr => Belt.Array.flatMap(arr, ((statement, history)) => next(statement, history))

    let rec invokeNextAndIterate = (i, previousIteratorResultsOpt) => {
        let previousIteratorResults = Belt.Option.getWithDefault(previousIteratorResultsOpt, [])

        let nextResults = switch previousIteratorResultsOpt {
        | None => next(statement, initializeHistory(statement))
        | Some(_) => chainIntoNext(previousIteratorResults)
        }

        let n =  shouldShortCircuit(nextResults) ? 0 : i - 1

        let aggregateResults =
        nextResults
        //->Belt.SortArray.stableSortBy(((a, _), (b, _)) => Heuristic.compare(a, b))
        ->Belt.Array.concat(previousIteratorResults)


        iterate(n, Some(aggregateResults))
    }
    and iterate = (i, previousIteratorResultsOpt) => {
        switch (previousIteratorResultsOpt, i)  {
        | (None, 0) => Some([(statement, initializeHistory(statement))])
        | (Some(_), 0) => previousIteratorResultsOpt
        | (None | Some(_), _) => invokeNextAndIterate(i, previousIteratorResultsOpt)
        }
    }

    iterate(numberOfIterations, None)
    ->Belt.Option.getExn
    ->Belt.Array.keepMap(((statement, history)) => switch Ast.hasAbstraction(statement) {
    | true => None
    | false => Some((statement, history))
    })
}
