open Test

// todo: break this into testing isVectorEqual with hardcoded vectors
//       and test toVector which is a more casual testing for AST -> Vector
test("/entities/ast/vectorRepresentation.res/toVector", () => {

    //debruinj index right to left
    let aAndB = list{
        VectorRepresentation.Variable(1), 
        VectorRepresentation.Operator(VectorRepresentation.Conjunction), 
        VectorRepresentation.Variable(0) }

    let aAndB' = VectorRepresentation.toVector(Parser.parse("a and b"))
    Assert.VectorRepresentation.isVectorEqual(aAndB, aAndB')

    let aOrC  = VectorRepresentation.toVector(Parser.parse("a or c"))
    Assert.VectorRepresentation.isVectorNotEqual(aAndB, aOrC)

    let aAndBAndB  = VectorRepresentation.toVector(Parser.parse("a and b and b"))
    Assert.VectorRepresentation.isVectorNotEqual(aAndB, aAndBAndB)
})

test("/entities/ast/vectorRepresentation.res/toVector", () => {
    ignore()
})