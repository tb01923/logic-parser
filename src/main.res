

let extractLabelAndProposition = tag => switch tag {
    | PropositionSearchDomain.AbstractionStep(x) => ("abstraction", x)
    | PropositionSearchDomain.ApplicationStep({ matchedLaw, _, _ }) => {
        let (name, x, _) = matchedLaw
        (name, x)
    }
    | PropositionSearchDomain.TransformationResult(x) => ("transformation", x)
    | PropositionSearchDomain.Initial(x) => ("initial", x)
    | PropositionSearchDomain.Trace(str, x) => (str, x)
}

let printStep = (step) => {
    let (l, statement) = extractLabelAndProposition(step)
    let score = Heuristic.variablesRaisedToOperations(statement)

    let tabs = switch Js.String2.length(l) {
    | x if x < 6 => "\t\t\t\t"
    | x if x < 15 => "\t\t\t"
    | x if x < 18 => "\t\t"
    | _ => "\t"
    }
    Js.Console.log(l ++ ":" ++ tabs ++ Belt.Float.toString(score) ++ "\t" ++  StringRepresentation.printImplicit(statement))
}

let solve = ast =>
    ast
    ->BruteForceSolver.solve
    ->((_, history)) => history
    ->Belt.Array.map(printStep)
    ->ignore

let abstract = ast =>
    ast
    ->Abstraction.getAbstractions
    ->Belt.Array.getExn(4)
    ->abstraction => {
        Js.Console.log("")
        Js.Console.log("Abstraction")
        Js.Console.log("")
        abstraction
        ->StringRepresentation.printImplicit
        ->Js.Console.log

        Js.Console.log("")
        Js.Console.log("Laws")
        Js.Console.log("")

        abstraction
    }
    ->LawApplication.identifyLaws
    ->Belt.Array.map((trans) => {
        let {matchedLaw} = trans

        LawApplication.getLawAst(matchedLaw)
        ->StringRepresentation.printImplicit
        ->Js.Console.log

        trans
    })
    ->Belt.Array.getExn(0)
    ->(trans: LawApplication.transformation ) => {

        let {matchedLaw, statementMatched, matchedSide} = trans
        let nextStatement = Replacement.replace(
            abstraction,
            statementMatched,
            LawApplication.getLawAst(matchedLaw),
            matchedSide)

        nextStatement
        ->StringRepresentation.printImplicit
        ->Js.Console.log
    }
//    ->Belt.Array.map(StringRepresentation.printImplicit)
//    ->Belt.Array.map(Js.Console.log)
    ->ignore


"a and b"
//"not(a) and a"
//"not(q) or p"
//"(not(a and b) and not(a and b) or (a and b)) or F"
//"(not(a and b) or not(a and b) or (a and b))"
//"a and b or not(a and b)"
//"a and a and a and a and a"
//"p or q and p"
->Parser.parse
->solve


/*
     (a and b) and not(a and b)

     c and not c
     c and d
     e
*/
