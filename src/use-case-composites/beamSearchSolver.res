open SearchHelpers

let solve = (statement) : PropositionSearchDomain.solutionWithSteps => {

    let rec iterate = (~previousSet=Belt.Set.String.empty, solutions) => {
        let neighbors =
            solutions
            ->PropositionSearchDomain.neighborsForMany
            ->removeAbstractions
            ->takeBest(5)

//        Js.Console.log(".........")
//        Js.Console.log(Belt.Array.map(solutions, ((s, _)) => StringRepresentation.printImplicit(s)))
//        Js.Console.log(Belt.Array.map(neighbors, ((s, _)) =>
//            Heuristic.complexity(s)->Belt.Int.toString ++ " :" ++
//            StringRepresentation.printImplicit(s)))

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
    ->takeBest(1)
    ->(arr => arr[0])
}
