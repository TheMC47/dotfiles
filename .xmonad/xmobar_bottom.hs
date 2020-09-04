Config {
  font = "xft:SFN display:size=10,FontAwesome:size=10"
  , position = BottomSize L 100 30
  , commands = [
                Run UnsafeStdinReader
               ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "<action=`qdbus org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout -1 -1 -1` button=1>   </action>}%UnsafeStdinReader%{"
  }
