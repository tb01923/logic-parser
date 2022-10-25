
module ComparableByComplexity = Belt.Id.MakeComparable({
  type t = Ast.proposition
  let cmp = Heuristic.compare
})



let solve = (statement)  => {
    statement
}





//let openSet = Belt.Set.make(~id=module(ComparableByComplexity))
//let s1 = Belt.Set.add(openSet, Parser.parse("a and b"))
//let s2 = Belt.Set.add(s1, Parser.parse("a or b"))
//Js.Console.log(s2)


// h(n) = size of tree (n)
