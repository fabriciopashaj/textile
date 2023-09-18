# Package

version       = "0.0.2"
author        = "Fabricio Pashaj"
description   = "A small text processing language."
license       = "GPL-3.0-or-later"
srcDir        = "src"
bin           = @["textile"]


# Dependencies

requires "nim >= 1.9"
requires "https://github.com/fabriciopashaj/yarolin"
