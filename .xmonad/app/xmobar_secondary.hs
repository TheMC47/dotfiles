Config {
  font = "xft:Source Code Pro:size=11:regular:antialias=true,FontAwesome:pixelsize=13"
  , overrideRedirect = False
  , position = TopSize L 100 30
  , iconRoot = "/home/yecinem/.xmonad/xpm"
  , commands = [Run UnsafeStdinReader
               ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "<icon=haskell_20.xpm/> %UnsafeStdinReader%}{"
  }
