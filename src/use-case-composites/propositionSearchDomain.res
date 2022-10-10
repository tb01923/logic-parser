open LawApplication
open Replacement


type solutionStep =
    AbstractionStep(Ast.proposition)
    | ApplicationStep(LawApplication.transformation)
    | TransformationResult(Ast.proposition)
    | Initial(Ast.proposition)
    | Trace(string, Ast.proposition)

type solutionSteps = Belt.Array.t<solutionStep>
type solutionWithSteps = (Ast.proposition, solutionSteps)
type solutionArray = Belt.Array.t<solutionWithSteps>

let makeSolutionArray = (statement) : solutionArray => [(statement, [Initial(statement)])]

let visitedStatements : Belt.HashMap.String.t<bool> = Belt.HashMap.String.make(~hintSize=10)
let isUnseenNeighbor = (solution) => {
    let (statement, _) = solution

    let myHash = StringRepresentation.hash(statement)
    switch Belt.HashMap.String.has(visitedStatements, myHash) {
    | true =>  None
    | false => {
        Belt.HashMap.String.set(visitedStatements, myHash, true)
        Some(solution)
    }}
}


let applyLawsToAbstraction = (abstraction, stepsFromOrigin:solutionSteps) => {
    abstraction
    ->LawApplication.identifyLaws
    ->Belt.Array.map(applicableLaw => {

        let {matchedLaw, statementMatched, matchedSide}  = applicableLaw

        // apply law tp current abstraction, providing a form for the next epoch
        let neighbor = replace(
            abstraction,
            statementMatched,
            getLawAst(matchedLaw),
            matchedSide)

        let stepsToNeighbor = [AbstractionStep(abstraction), ApplicationStep(applicableLaw), TransformationResult(neighbor)]
        let stepsFromOriginToNeighbor = Belt.Array.concat(stepsFromOrigin, stepsToNeighbor)
        (neighbor, stepsFromOriginToNeighbor)
    })
}

let neighbors = (~steps=[], statement: Ast.proposition) : solutionArray  => {
    statement
    ->Abstraction.getAbstractions
    ->Belt.Array.flatMap(applyLawsToAbstraction(_, steps))
    ->Belt.Array.keepMap(isUnseenNeighbor)
}


let nextNeighbors = (solution: solutionWithSteps) => {
    let (statement, stepsFromOrigin) = solution
    neighbors(statement, ~steps=stepsFromOrigin)
}

let neighborsForMany = (arr: solutionArray) => Belt.Array.flatMap(arr, nextNeighbors)