

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
    let score = Heuristic.complexity(statement)

    let tabs = switch Js.String2.length(l) {
    | x if x < 6 => "\t\t\t\t"
    | x if x < 15 => "\t\t\t"
    | x if x < 18 => "\t\t"
    | _ => "\t"
    }
    Js.Console.log(l ++ ":" ++ tabs ++ Belt.Int.toString(score) ++ "\t" ++  StringRepresentation.printImplicit(statement))
}

let solve = ast =>
    ast
   ->BruteForceSolver.solve
    // ->BeamSearchSolver.solve
    ->((_, history)) => history
    ->Belt.Array.map(printStep)
    ->ignore

open LawApplication
let abstract = ast =>

        ast
        ->Abstraction.getAbstractions
        ->Belt.Array.map(abstraction => {
            abstraction
            ->(a => "A " ++ StringRepresentation.printImplicit(a))
            ->Js.Console.log

            abstraction
        })
        ->Belt.Array.flatMap(LawApplication.identifyLaws)
        ->Belt.Array.map((trans: LawApplication.transformation) => {
            let {matchedLaw} = trans

            LawApplication.getLawAst(matchedLaw)
            ->(l => "L " ++ StringRepresentation.printImplicit(l))
            ->Js.Console.log

            trans
        })
        ->Belt.Array.map((trans: LawApplication.transformation ) => {

            let {matchedLaw, statementMatched, matchedSide} = trans
            let nextStatement = Replacement.replace(
                ast,
                statementMatched,
                LawApplication.getLawAst(matchedLaw),
                matchedSide)

            nextStatement
            ->(n => "N " ++ StringRepresentation.printImplicit(n))
            ->Js.Console.log
        })

//    ->Belt.Array.map(StringRepresentation.printImplicit)
//    ->Belt.Array.map(Js.Console.log)
    ->ignore


let printAbstractions = () => {
    // "a and b and c or not(a and b and c)"
    "a and b and c and d"
    ->Parser.parse
    ->Abstraction.getAbstractions
    ->Belt.Array.map(StringRepresentation.printImplicit)
    ->Belt.Array.map(Js.Console.log)
    ->ignore
}

let solve = () => 
    // "a and b and c"
    // "not(a) and a"
    //"not(q) or p"
    //"(not(a and b) and not(a and b) or (a and b)) or F"
    //"(not(a and b) or not(a and b) or (a and b))"
    // "a and b and c or not(a and b and c)"
    //"a and a and a and a and a"
    //"p or q and p"
    "(a ∧ [f/(b ∧ (c ∧ d))])"
    ->Parser.parse
    ->solve
    ->ignore

let printLaws = () => {
    // Laws.laws
    //     ->Belt.Array.map(LawApplication.getLawAst)
    //     ->Belt.Array.map(StringRepresentation.printImplicit)
    //     ->Belt.Array.map(Js.Console.log)
    //     ->ignore

    let getName = (law:Laws.law) : string => {
        let (name, _,  _) = law
        name
    } 

    Laws.laws
        ->Belt.Array.map(law  => (getName(law), LawApplication.getLawAst(law)))
        ->Belt.Array.map(((name, ast)) => (name, StringRepresentation.printImplicit(ast)))
        ->Belt.Array.map(((name, law)) => Js.Console.log(name ++ ": " ++ law))
        ->ignore
}

// printAbstractions()
solve()