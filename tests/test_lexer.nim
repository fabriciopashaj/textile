import unittest, streams, math
import ../src/textile/lexer
import yarolin/results

suite "lexer":
  setup:
    var lexer: Lexer
  test "string literals":
    lexer.open(newStringStream("\"foo\n bar\" \"fr\"\n\"\\x0c\\n\""))
    discard lexer.next.getVal() # first call for initialization
    var token = lexer.next().getVal()
    check token.kind == StrLit
    check token.str == "foo\n bar"
    check token.span.line == 1..2
    check token.span.col == 0..4
    check token.span.offset == 0..9
    token = lexer.next().getVal()
    check token.kind == StrLit
    check token.str == "fr"
    check token.span.line == 2..2
    check token.span.col == 6..9
    check token.span.offset == 11..14
    token = lexer.next().getVal()
    check token.kind == StrLit
    check token.str == "\x0c\n"
    check token.span.line == 3..3
    check token.span.col == 0..7
    check token.span.offset == 16..23
  test "integers":
    lexer.open(newStringStream("123"))
    discard lexer.next.getVal() # first call for initialization
    var token = lexer.next().getVal()
    check token.kind == IntLit
    check token.integer == 123
    check token.span.line == 1..1
    check token.span.col == 0..3
    check token.span.offset == 0..3
  test "floats":
    lexer.open(newStringStream("456.75"))
    discard lexer.next.getVal() # first call for initialization
    var token = lexer.next().getVal()
    check token.kind == FloatLit
    check token.real.almostEqual(456.75)
    check token.span.line == 1..1
    check token.span.col == 0..6
    check token.span.offset == 0..6
  test "identifiers":
    lexer.open(newStringStream("fooBar_baz a0323bcs"))
    discard lexer.next.getVal() # first call for initialization
    var token = lexer.next().getVal()
    check token.kind == Ident
    check token.str == "fooBar_baz"
    check token.span.line == 1..1
    check token.span.col == 0..10
    check token.span.offset == 0..10
    token = lexer.next().getVal()
    check token.kind == Ident
    check token.str == "a0323bcs"
    check token.span.line == 1..1
    check token.span.col == 11..19
    check token.span.offset == 11..19
