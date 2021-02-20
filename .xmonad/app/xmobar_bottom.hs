Config {
  font = "xft:Source Code Pro:size=11:regular:antialias=true"
  , additionalFonts = [ "xft:FontAwesome:pixelsize=13" ]
  , position = BottomSize L 100 30
  , commands = [
      -- Run UnsafeXPropertyLog "_XMONAD_LOG_BOTTOM"
      Run StdinReader
  ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "<action=`qdbus org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout -1 -1 -1` button=1><fn=1></fn></action>} %StdinReader%{"
  , overrideRedirect = False
  }
