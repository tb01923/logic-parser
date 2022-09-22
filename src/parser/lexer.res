type position = {
  line: int,
  startCol: int,
  endCol: int,
};

type token =
  | LParen
  | RParen
  | Not
  | And
  | Or
  | ThinRightArrow
  | FatDoubleArrow
  | Variable(string)
  | Truth
  | Falsity;

exception UnknownWord(string)

let splitIntoWords = Js.String.splitByRe(%re("/([\s\(\)])/g"))
let isSpace = word =>  switch Js.Re.exec_(%re("/\s+/"),word) {
    | Some(_) => true
    | None => false
}

let addWordToTokens = (tokens, word) => {
    let token = switch word {
        | "(" => LParen
        | ")" => RParen
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

let myTokens = getTokens("not a and not(b -> a) <=> d")