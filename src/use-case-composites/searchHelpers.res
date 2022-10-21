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

let takeBest = (neighbors, n) => {
    let numNeighbors = Belt.Array.length(neighbors)
    neighbors
    // score each neighbor
    ->Belt.Array.map(((statement, history)) => (Heuristic.complexity(statement), (statement, history)))
    // sort by score
    //->Belt.SortArray.stableSortBy(((scA, _), (scB, _)) => Belt.Float.toInt(scA -. scB))
    ->Belt.SortArray.stableSortBy(((scA, _), (scB, _)) => scA - scB)
    // pick best
    ->arr => Belt.Array.slice(arr, ~offset=0, ~len=(n < numNeighbors ? n : numNeighbors))
    // disca
    ->Belt.Array.map(((_, solution)) => solution)
}

let takeClosest = (neighbors, original, n) => {
    let numNeighbors = Belt.Array.length(neighbors)
    neighbors
    // score each neighbor
    ->Belt.Array.map(((statement, history)) => (Heuristic.levenshteinProposition(original, statement), (statement, history)))
    // sort by score
    ->Belt.SortArray.stableSortBy(((scA, _), (scB, _)) => scA - scB)
    // pick best
    ->arr => Belt.Array.slice(arr, ~offset=0, ~len=(n < numNeighbors ? n : numNeighbors))
    // disca
    ->Belt.Array.map(((_, solution)) => solution)
}

let removeAbstractions = arr =>
    arr->
    Belt.Array.keepMap(((solution, history)) => Ast.hasAbstraction(solution) ? None : Some((solution, history)) )
