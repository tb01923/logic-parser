open SearchHelpers

let solve = (statement) : PropositionSearchDomain.solutionWithSteps => {

    let rec iterate = (~previousSet=Belt.Set.String.empty, solutions) => {
        let neighbors =
            solutions
            ->PropositionSearchDomain.neighborsForMany
            ->removeAbstractions
            ->bestN(5)

        let currentSet =
            neighbors
            ->Belt.Array.map(((solution, _)) => StringRepresentation.printImplicitDeBruinj(solution))
            ->Belt.Set.String.fromArray

        let doesContainSolution = containsSolution(neighbors)

        switch (doesContainSolution, Belt.Set.String.eq(currentSet, previousSet))  {
        | (true, _) => neighbors
        | (_, true) => neighbors
        | (false, false) => {
            iterate(neighbors, ~previousSet=currentSet)
        }}
    }

    PropositionSearchDomain.makeSolutionArray(statement)
    ->iterate
    ->bestN(1)
    ->(arr => arr[0])
}
