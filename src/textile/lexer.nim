import lexbase, unicode, strutils, strformat, sugar
import yarolin/results
import ./util

export lexbase

template `!!`[V](valType: typedesc[V]): untyped = LexError!V

type
  Pos* = tuple[line, col, offset: int]
  Span* = tuple[line, col, offset: Slice[int]]

  TokenKind* {.pure.} = enum
    Eof
    StrLit    = "$strlit"
    IntLit    = "$int"
    FloatLit  = "$float"
    Ident     = "$ident"
    LParen    = "'('"
    RParen    = "')'"
    LBracket  = "'['"
    RBracket  = "']'"
    LBrace    = "'{'"
    RBrace    = "'}'"
  Token* = object
    span*: Span
    case kind*: TokenKind
    of Eof, LParen, RParen, LBracket, RBracket, LBrace, RBrace:
      discard
    of StrLit, Ident:
      str*: string
    of IntLit:
      integer*: BiggestInt
    of FloatLit:
      real*: BiggestFloat

  LexError* {.pure.} = enum
    NotHexDigit
    UntermiatedString
  Lexer* {.byRef.} = object of BaseLexer
    rune: Rune
    token: Token

func `..`(start, finish: Pos): Span {.inline.} =
  result.line = start.line..finish.line
  result.col = start.col..finish.col
  result.offset = start.offset..finish.offset

using
  L: Lexer
  lexer: var Lexer

func `<=`(a, b: Rune): bool {.inline.} = int32(a) <= int32(b)
func `>=`(a, b: Rune): bool {.inline.} = int32(a) >= int32(b)

func position*(L; offset = 0): Pos {.inline.} =
  (L.lineNumber,
   L.getColNumber(L.bufpos + offset),
   L.offsetBase + L.bufpos + offset)

proc nextRune(lexer): Rune =
  var pos = lexer.bufpos
  fastRuneAt(lexer.buf, pos, lexer.rune)
  if lexer.rune == Rune'\n':
    lexer.bufpos = lexer.handleLF(lexer.bufpos)
  elif lexer.rune == Rune'\c':
    lexer.bufpos = lexer.handleCR(lexer.bufpos)
  else:
    lexer.bufpos = pos # inc lexer.bufpos, size(lexer.rune)
  result = lexer.rune

proc skip(lexer; charset: set[char]) =
  while true:
    if size(lexer.rune) != 1 or char(lexer.rune) notin charset:
      break
    discard lexer.nextRune()

func hexValue(hexDigit: char): !!int {.inline.} =
  if   hexDigit in '0'..'9': result =!+ ord(hexDigit) - ord('0')
  elif hexDigit in 'a'..'z': result =!+ ord(hexDigit) - ord('a') + 0xa
  elif hexDigit in 'A'..'Z': result =!+ ord(hexDigit) - ord('A') + 0xa
  else                     : result =!- NotHexDigit

proc lexEscapeSeq(lexer): !!bool =
  case lexer.nextRune()
  of Rune'x':
    let
      digit1 = hexValue(char lexer.nextRune()).try
      digit2 = hexValue(char lexer.nextRune()).try
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

proc lexStrLit(lexer): !!(string, Pos) =
  # NOTE: We have to do some goofy stuff in here, like storig the position
  #       before we skip a rune, so that we don't lose track of the end of the
  #       token on newlines. Maybe a better way would be to have smth like a 1
  #       rune lookahead.
  discard lexer.nextRune() # skip '"'
  var
    acc = ""
    pos = lexer.position()
  while true:
    case lexer.rune
    of Rune'\\':
      if lexer.lexEscapeSeq().try:
        continue
    of Rune'"':
      discard lexer.nextRune()
      break
    of Rune(EndOfFile):
      returnErr UntermiatedString
    else: discard
    acc.add(lexer.rune)
    pos = lexer.position()
    discard lexer.nextRune()
  result =!+ (acc, pos)

proc lexNumber(lexer; start: Pos): !!Token =
  var
    token = Token(kind: IntLit, span: start..start)
    intAcc = 0.BiggestInt
    pos: Pos
  while lexer.rune in Rune('0')..Rune('9'):
    pos = lexer.position()
    intAcc *= 10
    inc intAcc, ord(lexer.rune) - ord('0')
    discard lexer.nextRune()
  if lexer.rune != Rune'.':
    token.span.line.b = pos.line
    token.span.col.b = pos.col
    token.span.offset.b = pos.offset
    token.integer = intAcc
    returnVal token
  # TODO: Add scientific notation format and other bases.
  # XXX: Doesnt't check if there's a dot here, assumes
  discard lexer.nextRune()
  var
    n = 0
    decimal = 0
  while lexer.rune in Rune('0')..Rune('9'):
    pos = lexer.position()
    decimal *= 10
    inc decimal, ord(lexer.rune) - ord('0')
    n *= 10
    discard lexer.nextRune()
  token.span.line.b = pos.line
  token.span.col.b = pos.col
  token.span.offset.b = pos.offset
  returnVal Token(
    kind: FloatLit,
    span: token.span,
    real: BiggestFloat(intAcc) + BiggestFloat(decimal) / BiggestFloat(n))

proc lexIdent(lexer): (string, Pos) =
  var
    acc = ""
    pos: Pos
  while size(lexer.rune) > 1 or char(lexer.rune) in IdentChars:
    pos = lexer.position()
    acc.add(lexer.rune)
    discard lexer.nextRune()
  result = (acc, pos)

proc next*(lexer): !!Token =
  if lexer.offsetBase + lexer.bufpos == 0:
    result =!+ Token(kind: Eof)
    discard lexer.nextRune()
  elif lexer.token.kind == Eof:
    returnVal lexer.token
  result =!+ lexer.token
  lexer.skip(Whitespace)
  let start = lexer.position(-1)
  if size(lexer.rune) > 1: # NOTE: just assumes it is an identifier for now
    let (str, endPos) = lexer.lexIdent()
    lexer.token = Token(kind: Ident, str: str, span: start..endPos)
    return
  case char(lexer.rune)
  of '"':
    let (str, endPos) = lexer.lexStrLit().try
    lexer.token = Token(kind: StrLit, str: str, span: start..endPos)
  of '0'..'9':
    lexer.token = lexer.lexNumber(start).try
  of 'a'..'z', 'A'..'Z':
    let (str, endPos) = lexer.lexIdent()
    lexer.token = Token(kind: Ident, str: str, span: start..endPos)
  of '(':
    lexer.token = Token(kind: LParen, span: start..start)
    discard lexer.nextRune()
  of ')':
    lexer.token = Token(kind: RParen, span: start..start)
    discard lexer.nextRune()
  of '[':
    lexer.token = Token(kind: LBracket, span: start..start)
    discard lexer.nextRune()
  of ']':
    lexer.token = Token(kind: RBracket, span: start..start)
    discard lexer.nextRune()
  of '{':
    lexer.token = Token(kind: LBrace, span: start..start)
    discard lexer.nextRune()
  of '}':
    lexer.token = Token(kind: RBrace, span: start..start)
    discard lexer.nextRune()
  of EndOfFile:
    lexer.token = Token(kind: Eof, span: lexer.position()..lexer.position())
  else:
    todo(fmt"Other kinds of tokens: {lexer.rune=}")
