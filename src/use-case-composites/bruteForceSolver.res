open SearchHelpers
let numberOfIterations = 1000

let solve = (statement) : PropositionSearchDomain.solutionWithSteps => {

    let rec iterate = (i, solutions) => {
        switch i  {
        | 0 => solutions
        | _ => {
            let neighbors =
                solutions
                ->PropositionSearchDomain.neighborsForMany

            let n = (containsSolution(neighbors)) ? 0 : i - 1
            iterate(n, neighbors)
        }}
    }

    PropositionSearchDomain.makeSolutionArray(statement)
    ->iterate(numberOfIterations, _)
    ->takeBest(1)
    ->(arr => arr[0])
}
