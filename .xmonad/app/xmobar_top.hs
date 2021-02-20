Config {
  -- font = "xft:SFN display:size=10"
  font = "xft:Source Code Pro:size=11:regular:antialias=true,FontAwesome:pixelsize=13"
  , overrideRedirect = False
  , position = TopSize L 100 30
  , iconRoot = "/home/yecinem/.xmonad/xpm"
  , commands = [Run Cpu [ "--template" , "<ipat><total>%"
                 , "--Low"      , "55"      -- units: %
                 , "--High"     , "77"      -- units: %
                 -- , "--low"      , "#26734D"
                 -- , "--normal"   , "#CC5200"
                 , "--high"     , "#CD3C66"
                 , "--ppad"              , "3"
                 , "--width"             , "3"
                 , "--maxtwidth"         , "4"
                 , "--"
                 , "--load-icon-pattern" , "<icon=cpu/cpu_%%.xpm/>"
                 ] 10
               , Run Memory [ "--template" , "<usedipat><usedratio>%"
                    , "--Low"      , "55"      -- units: %
                    , "--High"     , "77"      -- units: %
                    -- , "--low"      , "#26734D"
                    -- , "--normal"   , "#CC5200"
                    , "--high"     , "#BF616A"
                    , "--ppad"      , "3"
                    , "--width"     , "3"
                    , "--maxtwidth" , "4"
                    , "--"
                    , "--used-icon-pattern" , "<icon=ram/ram_%%.xpm/>"
                    ] 10
               , Run Date "%a %b %_d | %H:%M" "date" 10
               , Run Battery [ "--template"  , "<leftipat> <left>% <timeleft>"
                     , "--Low"       , "20"      -- units: %
                     , "--High"      , "90"      -- units: %
                     , "--low"       , "#BF616A"
                     , "--normal"    , "#D8DEE9"
                     , "--high"      , "#A3BE8C"
                     , "--maxtwidth" , "10"
                     , "--"
                     , "--on-icon-pattern"   , "<icon=battery/on/battery_on_%%.xpm/>"
                     , "--off-icon-pattern"  , "<icon=battery/off/battery_off_%%.xpm/>"
                     , "--idle-icon-pattern" , "<icon=battery/idle/battery_idle_%%.xpm/>"
                     , "-A" , "15"
                     , "-a", "notify-send -u critical 'Battery running out!'"
                     ] 50
               , Run UnsafeStdinReader
               -- , Run UnsafeXPropertyLog "_XMONAD_LOG_TOP"
               ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "<icon=haskell_20.xpm/> %UnsafeStdinReader%}{ %date% | %cpu% | %memory% | %battery% "
  }

