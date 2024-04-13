open Test

test("/entities/ast/lexer.res", () => {
  let string = "(a and b) or not c -> a <=> b T F ="
  let tokens = Lexer.getTokens(string)

  // test raw lexization (string token array)
  Assert.Boolean.isTrue("Token 0 `" ++ string ++ "` is LParen", tokens[0] == Lexer.LParen)
  Assert.Boolean.isTrue("Token 1 `" ++ string ++ "` is Variable('a')", tokens[1] == Lexer.Variable("a"))
  Assert.Boolean.isTrue("Token 2 `" ++ string ++ "` is And", tokens[2] == Lexer.And)
  Assert.Boolean.isTrue("Token 3 `" ++ string ++ "` is Variable('b')", tokens[3] == Lexer.Variable("b"))
  Assert.Boolean.isTrue("Token 4 `" ++ string ++ "` is RParen", tokens[4] == Lexer.RParen)
  Assert.Boolean.isTrue("Token 5 `" ++ string ++ "` is Or", tokens[5] == Lexer.Or)
  Assert.Boolean.isTrue("Token 6 `" ++ string ++ "` is Not", tokens[6] == Lexer.Not)
  Assert.Boolean.isTrue("Token 7 `" ++ string ++ "` is Variable('c')", tokens[7] == Lexer.Variable("c"))
  Assert.Boolean.isTrue("Token 8 `" ++ string ++ "` is ThinRightArrow", tokens[8] == Lexer.ThinRightArrow)
  Assert.Boolean.isTrue("Token 9 `" ++ string ++ "` is Vairable('a')", tokens[9] == Lexer.Variable("a"))
  Assert.Boolean.isTrue("Token 10 `" ++ string ++ "` is FatDoubleArrow", tokens[10] == Lexer.FatDoubleArrow)
  Assert.Boolean.isTrue("Token 11 `" ++ string ++ "` is Variable('b')", tokens[11] == Lexer.Variable("b"))
  Assert.Boolean.isTrue("Token 12 `" ++ string ++ "` is Truth", tokens[12] == Lexer.Truth)
  Assert.Boolean.isTrue("Token 13 `" ++ string ++ "` is Falsity", tokens[13] == Lexer.Falsity)
  Assert.Boolean.isTrue("Token 14 `" ++ string ++ "` is Equal", tokens[14] == Lexer.Equal)

  // Some testing of Lexer.tokenEquals with one not equals case
  //  todo: add more negative tests (loop through tokens and test negative cases)
  Assert.Lexer.isTokenEqual(Lexer.LParen, Lexer.LParen)
  Assert.Lexer.isTokenNotEqual(Lexer.LParen, Lexer.And)
  Assert.Lexer.isTokenEqual(Lexer.RParen, Lexer.RParen)
  Assert.Lexer.isTokenNotEqual(Lexer.RParen, Lexer.And)
  Assert.Lexer.isTokenEqual(Lexer.Not, Lexer.Not)
  Assert.Lexer.isTokenNotEqual(Lexer.Not, Lexer.And)
  Assert.Lexer.isTokenEqual(Lexer.And, Lexer.And)
  Assert.Lexer.isTokenNotEqual(Lexer.And, Lexer.Or)
  Assert.Lexer.isTokenEqual(Lexer.Or, Lexer.Or)
  Assert.Lexer.isTokenNotEqual(Lexer.Or, Lexer.And)
  Assert.Lexer.isTokenEqual(Lexer.ThinRightArrow, Lexer.ThinRightArrow)
  Assert.Lexer.isTokenNotEqual(Lexer.ThinRightArrow, Lexer.And)
  Assert.Lexer.isTokenEqual(Lexer.FatDoubleArrow, Lexer.FatDoubleArrow)
  Assert.Lexer.isTokenNotEqual(Lexer.FatDoubleArrow, Lexer.And)
  Assert.Lexer.isTokenEqual(Lexer.Equal, Lexer.Equal)
  Assert.Lexer.isTokenNotEqual(Lexer.Equal, Lexer.And)
  Assert.Lexer.isTokenEqual(Lexer.Truth, Lexer.Truth)
  Assert.Lexer.isTokenNotEqual(Lexer.Truth, Lexer.And)
  Assert.Lexer.isTokenEqual(Lexer.Falsity, Lexer.Falsity)
  Assert.Lexer.isTokenNotEqual(Lexer.Falsity, Lexer.And)
  Assert.Lexer.isTokenEqual(Lexer.Variable("a"), Lexer.Variable("a"))
  Assert.Lexer.isTokenNotEqual(Lexer.Variable("a"), Lexer.Variable("b"))
})