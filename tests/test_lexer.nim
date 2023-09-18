import unittest, streams
import ../src/textile/lexer
import yarolin/results

suite "lexer":
  setup:
    var lexer: Lexer
  test "string literals":
    lexer.open(newStringStream("\"foo\n bar\" \"fr\"\n\"\\x0a\\n\""))
    var token = lexer.next().getVal()
    check token.kind == StrLit
    check token.str == "foo\n bar"
    check token.span.line == 1..2
    check token.span.col == 1..5
    check token.span.offset == 0..9
    token = lexer.next().getVal()
    check token.kind == StrLit
    check token.str == "fr"
    check token.span.line == 2..2
    check token.span.col == 7..10
    check token.span.offset == 11..14
    token = lexer.next().getVal()
    check token.kind == StrLit
    check token.str == "\x0a\n"
    check token.span.line == 3..3
    check token.span.col == 1..8
    check token.span.offset == 16..23
