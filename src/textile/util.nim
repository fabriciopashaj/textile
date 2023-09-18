import terminal, macros

template todo*(msg: string = ""): untyped =
  let (file, line, _) = instantiationInfo()
  stderr.styledWriteLine(
    fgCyan, "Code path not yet implemented (", msg, ") ",
    fgWhite, "[",
    fgGreen, file,
    fgWhite, ":",
    fgGreen, $line,
    fgWhite, "]")
  quit(QuitFailure)
