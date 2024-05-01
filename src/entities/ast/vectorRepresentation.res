exception UnableToResolveVariable(string);

type operator =
   | Conjunction
   | Disjunction
   | Conditional
   | BiConditional
   | Equivalence
   | Negation

type vectorComponent =
    | Variable(int)
    | Constant(bool)
    | Operator(operator)

type vector = Belt.List.t<vectorComponent>

// VectorComponent.Variable is equality based on debruinj 
let isVectorComponentEqual = (a,b) => switch (a, b) {
    | (Variable(x), Variable(y)) => x == y
    | (Constant(x), Constant(y)) => x == y
    | (Operator(x), Operator(y)) => x == y
    | (_,_) => false   
}

let isVectorsEqual = (a,b) => {
    let aggregateVectorComponentEquality = (agg, (a, b)) => { agg && isVectorComponentEqual(a, b) }
    let listLengthEqual = Belt.List.length(a) == Belt.List.length(b)  
    Belt.List.zip(a, b)
        ->Belt.List.reduce(listLengthEqual, aggregateVectorComponentEquality)
}


let variableDebruinjResolver = (name, context) => {
    switch Belt.HashMap.String.get(context, name) {
        | Some(value) =>  value
        | None => raise(UnableToResolveVariable(name))
    }
}

/**
    getVector: convert @node AST to a vector representation passing
        @symbolResolver a way convert a variable name to an integer representation and
        @context the debruinj mapping from Variable name to the index position 
 */
let getVector = (symbolResolver, context, node) => {

    // convert AST binary operatpr to the vector representaiton of the operator
    let getBinOpRepresentation = op => switch op  {
        | Ast.Conjunction => Operator(Conjunction)
        | Ast.Disjunction => Operator(Disjunction)
        | Ast.Conditional => Operator(Conditional)
        | Ast.BiConditional => Operator(BiConditional)
        | Ast.Equivalence => Operator(Equivalence)
    }

     // convert AST unary operatpr to the vector representaiton of the operator
    let getUnOpRepresentation = op => switch op {
        | Ast.Negation => Operator(Negation)
    }

    // recursively parse tree convertin nodes
    let rec toVector = (~vect:vector=list{}, node) =>
      switch (node) {
      | Ast.BinaryOperation(_, op, lhs, rhs) => {
        vect
        ->toVector(~vect=_, lhs)
        ->Belt.List.add(getBinOpRepresentation(op))
        ->toVector(~vect=_, rhs)
      }
      | UnaryOperation(_, op, term) =>
        vect
        ->Belt.List.add(getUnOpRepresentation(op))
        ->toVector(~vect=_, term)
      | Abstraction(_, symb, _) => Belt.List.add(vect, Variable(symbolResolver(symb, context)))
      | Variable(_, name) => Belt.List.add(vect, Variable(symbolResolver(name, context)))
      | Value(_, true) => Belt.List.add(vect, Constant(true))
      | Value(_, false) => Belt.List.add(vect, Constant(false))
      }

      toVector(node)
}

/**
    toVector: convert an AST representation into a vector representation. this 
        reduces the tree structure using left to right depth based approach.  AST nodes 
        are converted to vector node representations, Variable names are drpopped and 
        leverage debruinj indexing instead. Vector representation is necessary for some 
        alogrithms like Levenshtien distance

        e.g. this AST 

               and            
            /      \
          a(0)      or
                  /   \
                b(1)  c(2)

        Converts to

            Variable(0), Operator(conJunction), variable(1), Operator(Disjunction), Varialble(2)
 */
let toVector  = node => getVector(
    variableDebruinjResolver, 
    Debruinj.getDebruinjIndices(node), 
    node)