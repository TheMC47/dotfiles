{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-top-binds  #-}

import           Xmobar
import           System.Environment


baseConfig :: Config
baseConfig = defaultConfig
  { font             = mainFont
  , additionalFonts  = [iconFont, workspaceIconFont]
  , overrideRedirect = False
  , lowerOnStart     = True
  , iconRoot         = "/home/yecinem/xmobarrc/xpm"
  , bgColor          = colorBase
  , fgColor          = colorText
  , sepChar          = "%"
  , alignSep         = "}{"
  }

topBar :: Config
topBar = baseConfig
  { commands = myCommands
  , position = OnScreen 0 (TopSize L 100 25)
  , template = green " >>=" <> " %UnsafeXMonadLog%}{%cpu% %memory% %battery% %time% %date% %_XMONAD_TRAYPAD%"
  }



secondary :: Int -> Config
secondary i = baseConfig
  { template =
    "%"
    <> customProp
    <> "%}{"
  , position = OnScreen i (TopSize L 100 30)
  , commands = [Run $ UnsafeXPropertyLog customProp]
  }
  where customProp = "_XMONAD_LOG__Secondary_" <> show i

myCommands :: [Runnable]
myCommands =
  [ Run $ Cpu
    [ "--template", sky (cpuIcon <> "<total>")
    , "--High"    , "77"
    , "--high"    , colorRed
    , "--suffix"  , "Yes"
    , "--ppad"    , "2"
    ]
    10
  , Run $ Memory
    [ "--template", teal (memoryIcon <> "<usedratio>")
    , "--High"    , "77" -- %
    , "--high"    , colorRed
    , "--ppad"    , "2"
    , "--suffix"  , "Yes"
    ]
    10
  , Run $ Date (yellow (clockIcon <> " %H:%M")) "time" 10
  , Run $ Date (peach (calendarIcon <> " %a %b %_d")) "date" 10
  , Run $ Battery
  [ "--template", "<acstatus>"
  , "--Low"     , "15"       -- Low  threshold for colours (in %)
  , "--High"    , "70"       -- High threshold for colours (in %)
  , "--low"     , colorRed
  , "--normal"  , colorGreen
  , "--high"    , colorGreen
  , "--suffix"  , "True"     -- Display '%' after '<left>'.
  , "--"                     -- battery specific options start here.
  , "--off"     , "<left> (<timeleft>)"                                    -- Not plugged
  , "--on"      , green (batteryChargingIcon <> "<left> (<timeleft>)")  -- Plugged.
  , "--idle"    , green (batteryFullIcon <> "<left>")                      -- Fully charged.
    -- Charge strings.  These go _in front_ of the @AC off@ string,
    -- while the @AC on@ and @idle@ strings ignore them.
  , "--lowt"    , "15"       -- Low  threshold for charge strings (in %).
  , "--hight"   , "70"       -- High threshold for charge strings (in %).
  , "--lows"    , batteryLowIcon
  , "--mediums" , batteryMediumIcon
  , "--highs"   , batteryFullIcon
  ] 10
  , Run UnsafeXMonadLog
  , Run $ UnsafeXPropertyLog "_XMONAD_TRAYPAD"
  , Run $ UnsafeXPropertyLog "_XMONAD_LOG__MAIN_WINDOW_NAME"
  ]

mainFont = "Fira Code 12"
iconFont = "Font Awesome 6 Free 10"
workspaceIconFont = "Font Awesome 6 Free 9"

textIcon = xmobarFont 1
workspaceIcon = xmobarFont 2

memoryIcon = textIcon "\xf538" -- 
cpuIcon = textIcon "\xf2db" -- 
clockIcon = textIcon "\xf017"
calendarIcon = textIcon "\xf133"
batteryFullIcon = textIcon "\xf240"
batteryMediumIcon = textIcon "\f242"
batteryLowIcon = textIcon "\xf243"
batteryChargingIcon = textIcon "\xf1e6 \xf240"

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

-- | Use xmobar escape codes to output a string with the font at the given index
xmobarFont :: Int     -- ^ index: index of the font to use (0: standard font)
           -> String  -- ^ output string
           -> String
xmobarFont index = wrap ("<fn=" ++ show index ++ ">") "</fn>"



colorRosewater = "#f2d5cf"
rosewater = xmobarColor colorRosewater ""
colorFlamingo = "#eebebe"
flamingo = xmobarColor colorFlamingo ""
colorPink = "#f4b8e4"
pink = xmobarColor colorPink ""
colorMauve = "#ca9ee6"
mauve = xmobarColor colorMauve ""
colorRed = "#e78284"
red = xmobarColor colorRed ""
colorMaroon = "#ea999c"
maroon = xmobarColor colorMaroon ""
colorPeach = "#ef9f76"
peach = xmobarColor colorPeach ""
colorYellow = "#e5c890"
yellow = xmobarColor colorYellow ""
colorGreen = "#a6d189"
green = xmobarColor colorGreen ""
colorTeal = "#81c8be"
teal = xmobarColor colorTeal ""
colorSky = "#99d1db"
sky = xmobarColor colorSky ""
colorSapphire = "#85c1dc"
sapphire = xmobarColor colorSapphire ""
colorBlue = "#8caaee"
blue = xmobarColor colorBlue ""
colorLavender = "#babbf1"
lavender = xmobarColor colorLavender ""
colorText = "#c6d0f5"
text = xmobarColor colorText ""
colorSubtext1 = "#b5bfe2"
subtext1 = xmobarColor colorSubtext1 ""
colorSubtext0 = "#a5adce"
subtext0 = xmobarColor colorSubtext0 ""
colorOverlay2 = "#949cbb"
overlay2 = xmobarColor colorOverlay2 ""
colorOverlay1 = "#838ba7"
overlay1 = xmobarColor colorOverlay1 ""
colorOverlay0 = "#737994"
overlay0 = xmobarColor colorOverlay0 ""
colorSurface2 = "#626880"
surface2 = xmobarColor colorSurface2 ""
colorSurface1 = "#51576d"
surface1 = xmobarColor colorSurface1 ""
colorSurface0 = "#414559"
surface0 = xmobarColor colorSurface0 ""
colorBase = "#303446"
base = xmobarColor colorBase ""
colorMantle = "#292c3c"
mantle = xmobarColor colorMantle ""
colorCrust = "#232634"
crust = xmobarColor colorCrust ""
