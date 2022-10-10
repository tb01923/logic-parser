
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


let solve = (statement: Ast.proposition) : PropositionSearchDomain.solutionWithSteps => {


    let rec invokeNextAndIterate = (i, solutions) => {

        let neighbors = switch solutions {
        | None => PropositionSearchDomain.neighbors(statement)
        | Some(arr) => PropositionSearchDomain.neighborsForMany(arr)
        }

        let n = (containsSolution(neighbors)) ? 0 : i - 1

        iterate(n, Some(neighbors))
    }
    and iterate = (i, solutions) => {
        switch (i, solutions)  {
        | (0, None) => PropositionSearchDomain.makeSolutionArray(statement)->Some
        | (0, Some(_)) => solutions
        | (_, None | Some(_)) => invokeNextAndIterate(i, solutions)
        }
    }

    iterate(numberOfIterations, PropositionSearchDomain.makeSolutionArray(statement)->Some)
    ->Belt.Option.getExn
    // eliminate any solutions that are still abstractions
//    ->Belt.Array.keepMap(((statement, history)) => switch Ast.hasAbstraction(statement) {
//    | true => None
//    | false => Some((statement, history))
//    })
    // score each last remaining neighbor, pick the best, drop the scoring
    ->Belt.Array.map(((statement, history)) => (Heuristic.variablesRaisedToOperations(statement), (statement, history)))
    ->Belt.SortArray.stableSortBy(((scA, _), (scB, _)) => Belt.Float.toInt(scA -. scB))
    ->(arr => arr[0])
    ->((_, solution)) => solution
}
