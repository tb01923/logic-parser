type solutionStep =
    AbstractionStep
    | ApplicationStep(LawApplication.transformation)
    | TransformationResult
    | Initial
    | Trace (string)

type solutionHistory = Belt.Array.t<(solutionStep, Ast.proposition)>
type solutionWithHistory = (Ast.proposition, solutionHistory)
type solutionWithHistoryArr = Belt.Array.t<solutionWithHistory>

let numberOfIterations = 4



let visitedStatements : Belt.HashMap.String.t<bool> = Belt.HashMap.String.make(~hintSize=10)
let isNew = ((applicableLaw, nextStatement)) => {
    let myHash = StringRepresentation.hash(nextStatement)
    switch Belt.HashMap.String.has(visitedStatements, myHash) {
    | true =>  None
    | false => {
        Belt.HashMap.String.set(visitedStatements, myHash, true)
        Some((applicableLaw, nextStatement))
    }}
}

let recordHistory = (statement, abstraction, applicableLaw, nextStatement, history) : solutionHistory => {

    let thisHistory : solutionHistory = Belt.Array.copy(history)

    // only push abstraction if it differs from statement
    switch Equality.byDebruinj(statement, abstraction) {
    | true => ()
    | false => Belt.Array.push(thisHistory, (AbstractionStep, abstraction))
    }

    // todo: perhaps push the matching sub-statement onto this portion of the history
    Belt.Array.push(thisHistory, (
        ApplicationStep(applicableLaw),
        LawApplication.getLawAst(applicableLaw.matchedLaw)))

    // todo consider adding this... ooops we did :)
    Belt.Array.push(thisHistory, (TransformationResult, nextStatement))

    thisHistory
}

let applyLawsToAbstraction = (abstraction, statement, history: solutionHistory) => {
    abstraction
    ->LawApplication.identifyLaws
    ->Belt.Array.map(applicableLaw => {

        let law  =applicableLaw.matchedLaw


        // apply law tp current abstraction, providing a form for the next epoch
        (applicableLaw, Replacement.replace(
            abstraction,
            applicableLaw.statementMatched,
            LawApplication.getLawAst(applicableLaw.matchedLaw),
            applicableLaw.matchedSide))
    })
    ->Belt.Array.keepMap(isNew)
    ->Belt.Array.map(((applicableLaw, nextStatement)) => {
        let thisHistory = recordHistory(statement, abstraction, applicableLaw, nextStatement, history)
        (nextStatement, thisHistory)
    })
}

let next = (statement: Ast.proposition, history: solutionHistory) : solutionWithHistoryArr  => {
    let abstractions = Abstraction.getAbstractions(statement)
    let laws = abstractions->Belt.Array.flatMap(applyLawsToAbstraction(_, statement, history))



    laws
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

let solve = (statement: Ast.proposition) : solutionWithHistory => {

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
    // eliminate any solutions that are still abstractions
    ->Belt.Array.keepMap(((statement, history)) => switch Ast.hasAbstraction(statement) {
    | true => None
    | false => Some((statement, history))
    })
    ->Belt.Array.map(((statement, history)) => (Heuristic.variablesRaisedToOperations(statement), statement, history))
    ->Belt.SortArray.stableSortBy(((scA, _, _), (scB, _, _)) => Belt.Float.toInt(scA -. scB))
    ->(arr => arr[0])
    ->((_, solution, history)) => (solution, history)
}
