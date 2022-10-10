
exception UninitializedSearch(Ast.proposition)
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

let containsSolution = neighbors => {
    // if there aere
    let containsOptimal = neighbors
    ->Belt.Array.map(((statement, _)) => statement)
    ->Belt.Array.keep(Heuristic.isOptimalSolution)
    ->Belt.Array.length
    ->(l => l > 0)

    let shortCircuit =  (Belt.Array.length(neighbors) === 0) || containsOptimal
    shortCircuit
}

let makeInitialSolution = statement => PropositionSearchDomain.makeSolutionArray(statement)

let solve = (statement) : PropositionSearchDomain.solutionWithSteps => {

    let rec iterate = (i, solutions) => {
        switch i  {
        | 0 => solutions
        | _ => {
            let neighbors = PropositionSearchDomain.neighborsForMany(solutions)
            let n = (containsSolution(neighbors)) ? 0 : i - 1
            iterate(n, neighbors)
        }}
    }

    iterate(numberOfIterations, makeInitialSolution(statement))
    // score each neighbor
    ->Belt.Array.map(((statement, history)) => (Heuristic.variablesRaisedToOperations(statement), (statement, history)))
    // sort by score
    ->Belt.SortArray.stableSortBy(((scA, _), (scB, _)) => Belt.Float.toInt(scA -. scB))
    // pick best
    ->(arr => arr[0])
    // disca
    ->((_, solution)) => solution
}
