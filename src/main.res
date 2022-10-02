let print = (l, x) => {
    let score = Heuristic.variablesRaisedToOperations(x)
    let tabs = switch Js.String2.length(l) {
    | x if x < 6 => "\t\t\t"
    | x if x < 15 => "\t\t"
    | _ => "\t"
    }
    Js.Console.log(l ++ ":" ++ tabs ++ Belt.Float.toString(score) ++ "\t" ++  StringRepresentation.printImplicit(x))
}

let visitedStatements : Belt.HashMap.String.t<bool> = Belt.HashMap.String.make(~hintSize=10)

let next = (statement, history) => {
    Abstraction.getAbstractions(statement)
    ->Belt.Array.flatMap(abstraction => {
        LawApplication.identifyLaws(abstraction)
        ->Belt.Array.keepMap(applicableLaw => {

            let nextStatement = Replacement.replace(
                abstraction,
                applicableLaw.statementMatched,
                LawApplication.getLawAst(applicableLaw.matchedLaw),
                applicableLaw.matchedSide)

            let myHash = StringRepresentation.hash(nextStatement)
            switch Belt.HashMap.String.has(visitedStatements, myHash) {
            | true =>  None
            | false => {
                Belt.HashMap.String.set(visitedStatements, myHash, true)

                let thisHistory = Belt.Array.copy(history)
                Belt.Array.push(thisHistory, ("abstraction", abstraction))
                Belt.Array.push(thisHistory, (
                    LawApplication.getLawName(applicableLaw.matchedLaw),
                    LawApplication.getLawAst(applicableLaw.matchedLaw)))

                Some(nextStatement, thisHistory)
            }}
        })
    })
}

//let x = Parser.parse("a and b or c and not(a or b) and not(a or c) and not(c) and (a or b)")
//let x = Parser.parse("not(a and b) and not(a and b)")
//let x = Parser.parse("a and not(b) or (c and not(b))")
//let x = Parser.parse("a and b or not(a and b)")
//let x = Parser.parse("a and (b or c)")
let x = Parser.parse("a and b and c and d")

print("original", x)
next(x, [("start", x)])
->Belt.Array.forEach(
    ((statement, history)) => {
        Js.Console.log("")
        history
        ->Belt.Array.forEach(((label, s)) => print(label, s))
        print("final", statement)

    })
