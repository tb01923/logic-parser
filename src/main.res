

let getLabel = tag => switch tag {
    | BruteForceSolver.AbstractionStep => "abstraction"
    | BruteForceSolver.ApplicationStep({ matchedLaw, _, _ }) => {
        let (name, _, _) = matchedLaw
        name
    }
    | TransformationResult => "transformation"
    | Initial => "initial"
    | Trace (s) => s
}

let print = (step, x) => {
    let score = Heuristic.variablesRaisedToOperations(x)
    let l = getLabel(step)
    let tabs = switch Js.String2.length(l) {
    | x if x < 6 => "\t\t\t\t"
    | x if x < 15 => "\t\t\t"
    | x if x < 16 => "\t\t"
    | _ => "\t"
    }
    Js.Console.log(l ++ ":" ++ tabs ++ Belt.Float.toString(score) ++ "\t" ++  StringRepresentation.printImplicit(x))
}

let solve = ast =>
    ast
    ->(ast => {
        StringRepresentation.printImplicit(ast)->Js.Console.log
        Js.Console.log("_________")
        ast
    })
    ->BruteForceSolver.solve
    ->((_, history)) => history
    ->Belt.Array.map(((step, statement)) => print(step, statement))
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


//"not(a) and a"
"not(q) or p"
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
