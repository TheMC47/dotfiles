import           Xmobar

import           System.Environment


baseConfig :: Config
baseConfig = defaultConfig
  { font             =
    "xft:Source Code Pro:size=11:regular:antialias=true,FontAwesome:pixelsize=13"
  , overrideRedirect = False
  , lowerOnStart     = True
  , iconRoot         = "/home/yecinem/xmobarrc/xpm"
  , bgColor          = "#00192A"
  , fgColor          = "#D8DEE9"
  , sepChar          = "%"
  , alignSep         = "}{"
  }

topBar :: Config
topBar = baseConfig
  { commands = myCommands
  , position = OnScreen 0 (TopSize L 100 30)
  , template =
    " "
    <> styleUp (haskellIcon <> " %UnsafeXMonadLog%")
    <> "}{"
    <> concatMap styleUp
                 ["%date%", "%cpu%", "%memory%", "%battery%", "%trayerpad%"]
    <> " "
  }



secondary :: Int -> Config
secondary i = baseConfig
  { template =
    " <icon=circle_left.xpm/><icon=haskell.xpm/><fc=#D8DEE9,#2E3440:0> %"
    <> customProp
    <> "%</fc><icon=circle_right.xpm/>}{"
  , position = OnScreen i (TopSize L 100 30)
  , commands = [Run $ UnsafeXPropertyLog customProp]
  }
  where customProp = "_XMONAD_LOG__Secondary_" <> show i
styleUp :: String -> String
styleUp =
  wrap (" " <> leftIcon) (rightIcon <> " ") . xmobarColor "#D8DEE9" "#2E3440:0"


rightIcon, leftIcon, haskellIcon :: String
[rightIcon, leftIcon, haskellIcon] = map icon ["right", "left", "haskell"]


icon :: String -> String
icon = wrap "<icon=" ".xpm/>"

myCommands :: [Runnable]
myCommands =
  [ Run $ Cpu
    [ "--template"
    , "<ipat><total>%"
    , "--Low"
    , "55"      -- units: %
    , "--High"
    , "77"      -- units: %
                 -- , "--low"      , "#26734D"
                 -- , "--normal"   , "#CC5200"
    , "--high"
    , foreground "#CD3C66"
    , "--ppad"
    , "3"
    , "--width"
    , "3"
    , "--maxtwidth"
    , "4"
    , "--"
    , "--load-icon-pattern"
    , "<icon=cpu_%%.xpm/>"
    ]
    10
  , Run $ Memory
    [ "--template"
    , "<usedipat><usedratio>%"
    , "--Low"
    , "55"      -- units: %
    , "--High"
    , "77"      -- units: %
    , "--high"
    , foreground "#BF616A"
    , "--ppad"
    , "3"
    , "--width"
    , "3"
    , "--maxtwidth"
    , "4"
    , "--"
    , "--used-icon-pattern"
    , "<icon=ram_%%.xpm/>"
    ]
    10
  , Run $ Date "%a %b %_d | %H:%M" "date" 10
  , Run $ Battery
    [ "--template"
    , "<fc=#D8DEE9,#2E3440:0><leftipat><left>%<timeleft></fc>"
    , "--Low"
    , "20"      -- units: %
    , "--High"
    , "90"      -- units: %
    , "--low"
    , foreground "#BF616A"
    , "--normal"
    , foreground "#D8DEE9"
    , "--high"
    , foreground "#A3BE8C"
    , "--maxtwidth"
    , "10"
    , "--"
    , "--on-icon-pattern"
    , "<icon=battery_on_%%.xpm/>"
    , "--off-icon-pattern"
    , "<icon=battery_off_%%.xpm/>"
    , "--idle-icon-pattern"
    , "<icon=battery_idle_%%.xpm/>"
    , "-A"
    , "15"
    , "-a"
    , "notify-send -u critical 'Battery running out!'"
    ]
    50
  , Run UnsafeXMonadLog
  , Run $ Com "/home/yecinem/xmobarrc/trayer-padding-icon.sh" [] "trayerpad" 10
  ]


foreground :: String -> String
foreground =  (<> ",#2E3440:0")

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    ["secondary", n] -> xmobar $ secondary (read n)
    _                -> xmobar topBar

-- Stolen from XMonad

-- | Use xmobar escape codes to output a string with given foreground
--   and background colors.
xmobarColor
  :: String  -- ^ foreground color: a color name, or #rrggbb format
  -> String  -- ^ background color
  -> String  -- ^ output string
  -> String
xmobarColor fg bg = wrap t "</fc>"
  where t = concat ["<fc=", fg, if null bg then "" else "," ++ bg, ">"]

-- | Wrap a string in delimiters, unless it is empty.
wrap
  :: String  -- ^ left delimiter
  -> String  -- ^ right delimiter
  -> String  -- ^ output string
  -> String
wrap _ _ "" = ""
wrap l r m  = l ++ m ++ r
