type position = {
  line: int,
  startCol: int,
  endCol: int,
};

type token =
  | LParen
  | RParen
  | LBracket
  | RBracket
  | Slash
  | Not
  | And
  | Or
  | ThinRightArrow
  | FatDoubleArrow
  | Equal
  | Variable(string)
  | Truth
  | Falsity;

let toString = (token) => switch (token) {
  | LParen => "LParen"
  | RParen => "RParen"
  | LBracket => "LBracket"
  | RBracket => "RBracket"
  | Slash => "Slash"
  | Not => "Not"
  | And => "And"
  | Or => "Or"
  | ThinRightArrow => "ThinRightArrow"
  | FatDoubleArrow => "FatDoubleArrow"
  | Equal => "Equal"
  | Variable(a) => "Variable(" ++ a ++ ")"
  | Truth => "Truth" 
  | Falsity => "Falsity"
}

let arrayToString = tokens => {
  "[" ++ Belt.Array.reduce(
    tokens, "", 
    (str, t) => str ++ "," ++ toString(t)) ++ "]"
}

let tokenEquals = (tokenA, tokenB) => switch (tokenA, tokenB) {
    | (Variable(a), Variable(b)) if a === b => true
    | (Variable(a), Variable(b)) if a !== b => false
    | (Variable(_), _) => false
    | (_, Variable(_)) => false
    | (Or, Or) => true
    | (Or, _) => false
    | (_, Or) => false
    | (And, And) => true
    | (And, _) => false
    | (_, And) => false
    | (ThinRightArrow, ThinRightArrow) => true
    | (ThinRightArrow, _) => false
    | (_, ThinRightArrow) => false
    | (FatDoubleArrow, FatDoubleArrow) => true
    | (FatDoubleArrow, _) => false
    | (_, FatDoubleArrow) => false
    | (Not, Not) => true
    | (Not, _) => false
    | (_, Not) => false
    | (Truth, Truth) => true
    | (Truth, _) => false
    | (_, Truth) => false
    | (Falsity, Falsity) => true
    | (Falsity, _) => false
    | (_, Falsity) => false
    | (LParen, LParen) => true
    | (LParen, _) => false
    | (_, LParen) => false
    | (RParen, RParen) => true
    | (RParen, _) => false
    | (_, RParen) => false
    | (Equal, Equal) => true
    | (Equal, _) => false
    | (_, Equal) => false
    | (LBracket, LBracket) => true
    | (LBracket, _) => false
    | (_, LBracket) => false
    | (RBracket, RBracket) => true
    | (RBracket, _) => false
    | (_, RBracket) => false
    | (Slash, Slash) => true
    // | (Slash, _) => false
    // | (_, Slash) => false
}


exception UnknownWord(string)

let splitIntoWords = Js.String.splitByRe(%re("/([\s\(\)\[\]\/])/g"))
let isSpace = word =>  switch Js.Re.exec_(%re("/\s+/"),word) {
    | Some(_) => true
    | None => false
}

let addWordToTokens = (tokens, word) => {
    let token = switch word {
        | "(" => LParen
        | ")" => RParen
        | "[" => LBracket
        | "]" => RBracket
        | "/" => Slash
        | "~" => Not
        | "¬" => Not
        | "!" => Not
        | "not" => Not
        | "&" => And
        | "∧" => And
        | "and" => And
        | "|" => Or
        | "∨" => Or
        | "or" => Or
        | "->" => ThinRightArrow
        | "<=>" => FatDoubleArrow
        | "⊤" => Truth
        | "T" => Truth
        | "1" => Truth
        | "⊥" => Falsity
        | "F" => Falsity
        | "0" => Falsity
        | "=" => Equal
        | "≡" => Equal
        | name if Js.String.length(name) === 1 => Variable(name)
        | word => raise(UnknownWord(word))
    }
    
    Belt.Array.push(tokens, token)
    tokens
}

let isWord = potentialWord => switch potentialWord {
    | "" => None
    | word if isSpace(word) => None
    | word => Some(word)
}

let getOptionValue = opt => Belt.Option.getWithDefault(opt, "")

let getTokens = line => {
    line
        -> splitIntoWords
        -> Belt.Array.map(getOptionValue)
        -> Belt.Array.keepMap(isWord)
        -> Belt.Array.reduce([], addWordToTokens)
};
