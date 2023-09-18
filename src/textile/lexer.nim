import lexbase, unicode, strutils, strformat
import yarolin/results
import ./util

export lexbase

template `!!`[V](valType: typedesc[V]): untyped = LexError!V

type
  Pos* = tuple[line, col, offset: int]
  Span* = tuple[line, col, offset: Slice[int]]

  TokenKind* {.pure.} = enum
    Eof
    StrLit = "$strlit"
  Token* = object
    span*: Span
    case kind*: TokenKind
    of Eof:
      discard
    of StrLit:
      str*: string

  LexError* {.pure.} = enum
    NotHexDigit
    UntermiatedString
  Lexer* {.byRef.} = object of BaseLexer
    inited: bool = false
    rune: Rune
    token: Token

func `..`(start, finish: Pos): Span =
  result.line = start.line..finish.line
  result.col = start.col..finish.col
  result.offset = start.offset..finish.offset

using
  L: Lexer
  lexer: var Lexer

func position*(L; offset = 0): Pos {.inline.} =
  (L.lineNumber, L.getColNumber(L.bufpos + offset), L.offsetBase + L.bufpos - 1)

proc nextChar(lexer): Rune =
  fastRuneAt(lexer.buf, lexer.bufpos, lexer.rune, false)
  if lexer.rune == Rune'\n':
    lexer.bufpos = lexer.handleLF(lexer.bufpos)
  elif lexer.rune == Rune'\c':
    lexer.bufpos = lexer.handleCR(lexer.bufpos)
  else:
    inc lexer.bufpos, size(lexer.rune)
  result = lexer.rune

proc skip(lexer; charset: set[char]) =
  while true:
    if size(lexer.rune) != 1 or char(lexer.rune) notin charset:
      break
    discard lexer.nextChar()

func hexValue(hexDigit: char): !!int {.inline.} =
  if   hexDigit in '0'..'9': result =!+ ord(hexDigit) - ord('0')
  elif hexDigit in 'a'..'z': result =!+ ord(hexDigit) - ord('a') + 0xa
  elif hexDigit in 'A'..'Z': result =!+ ord(hexDigit) - ord('A') + 0xa
  else                     : result =!- NotHexDigit

proc lexEscapeSeq(lexer): LexError!bool =
  case lexer.nextChar()
  of Rune'x':
    let
      digit1 = hexValue(char lexer.nextChar()).try
      digit2 = hexValue(char lexer.nextChar()).try
    lexer.rune = Rune((digit1 shl 8) or digit2)
  of Rune'n':
    lexer.rune = Rune'\l'
  of Rune'r':
    lexer.rune = Rune'\c'
  of Rune'\\':
    lexer.rune = Rune'\\'
  else:
    todo("other escape sequences")
  result =!+ false

proc lexStrLit(lexer): !!string =
  var acc = newStringOfCap(16)
  while true:
    case lexer.nextChar()
    of Rune'\\':
      if lexer.lexEscapeSeq().try:
        continue
    of Rune'"':
      break
    else: discard
    acc.add(lexer.rune)
  result =!+ acc

proc next*(lexer): !!Token =
  if lexer.inited:
    result =!+ lexer.token
  else:
    result =!+ Token(kind: Eof)
    discard lexer.nextChar()
  lexer.skip(Whitespace)
  let start = lexer.position()
  case lexer.rune
  of Rune'"':
    returnVal Token(
      kind: StrLit,
      str: lexer.lexStrLit().try,
      span: start..lexer.position())
  else:
    todo(fmt"Other kinds of tokens: {lexer.rune=}")
