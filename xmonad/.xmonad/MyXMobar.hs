{-# LANGUAGE TypeApplications #-}
module MyXMobar where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.IORef
import           XMonad                  hiding ( defaultConfig )
import           XMonad.Actions.CopyWindow
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Prelude
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.Loggers
import           XMonad.Util.NamedScratchpad
import           Xmobar


barSpawner :: TQueue Int -> ScreenId -> IO StatusBarConfig
barSpawner trayerTQ 0       = xmobarSB (topBar trayerTQ) topPP <> pure trayerSB
barSpawner _        s@(S n) = xmobarSB (secondary n) (secondaryPP s)


foreground :: String -> String
foreground = (<> ",#2E3440:0")


baseConfig :: Config
baseConfig = defaultConfig
  { font             =
    "xft:Source Code Pro:size=11:regular:antialias=true,FontAwesome:pixelsize=13"
  , overrideRedirect = False
  , lowerOnStart     = True
  , iconRoot         = "/home/yecinem/.xmonad/xpm"
  , bgColor          = "#00192A"
  , fgColor          = "#D8DEE9"
  , sepChar          = "%"
  , alignSep         = "}{"
  }


topBar :: TQueue Int -> Config -- State, Trayer
topBar trayerQ = baseConfig
  { commands =
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
    , Run $ QueueReader trayerQ (wrap "<hspace=" "/>" . show) "trayer"
    ]
  , position = OnScreen 0 (TopSize L 100 30)
  , template =
    " "
    <> styleUp (haskellIcon <> " %xmonad%")
    <> "}{"
    <> concatMap styleUp
                 ["%date%", "%cpu%", "%memory%", "%battery%", "%trayer%"]
    <> " "
  }



secondary :: Int -> Config
secondary i = baseConfig
  { template =
    " <icon=circle_left.xpm/><icon=haskell.xpm/><fc=#D8DEE9,#2E3440:0> %xmonad% </fc><icon=circle_right.xpm/>}{"
  , position = OnScreen i (TopSize L 100 30)
  }

styleUp :: String -> String
styleUp =
  wrap (" " <> leftIcon) (rightIcon <> " ") . xmobarColor "#D8DEE9" "#2E3440:0"


rightIcon, leftIcon, haskellIcon :: String
[rightIcon, leftIcon, haskellIcon] = map icon ["right", "left", "haskell"]


icon :: String -> String
icon = wrap "<icon=" ".xpm/>"


xmobarSB :: Config -> X PP -> IO StatusBarConfig
xmobarSB c pp = do
  tid <- newIORef Nothing
  q   <- atomically $ newTQueue @String
  let c' = c { commands = Run (QueueReader q id "xmonad") : commands c }
  return $ mempty
    { sbStartupHook = io (writeIORef tid . Just =<< forkIO (xmobar c'))
    , sbCleanupHook = io
                      $   readIORef tid
                      >>= (`whenJust` killThread)
                      >>  writeIORef tid Nothing
    , sbLogHook = io . atomically . writeTQueue q =<< dynamicLogString =<< pp
    }

circleSep :: String
circleSep =
  "<icon=circle_right.xpm/></fc>  <fc=#D8DEE9,#2E3440:0><icon=circle_left.xpm/>"



myFgColor, myBgColor :: String
myBgColor = "#2E3440"
myFgColor = "#D8DEE9"

topPP :: X PP
topPP =
  copiesPP (xmobarColor "green" "")
    <=< clickablePP
    .   filterOutWsPP [scratchpadWorkspaceTag]
    $   def { ppCurrent = xmobarBorder "Bottom" myFgColor 4
            , ppUrgent  = xmobarBorder "Bottom" "#CD3C66" 4
            , ppVisible = xmobarBorder "Bottom" "#98a0b3" 1
            , ppSep     = circleSep
            , ppExtras = [logLayoutOnScreen 0, shortenL 50 (logTitleOnScreen 0)]
            , ppOrder   = \(ws : _ : _ : extras) -> ws : extras
            }

secondaryPP :: ScreenId -> X PP
secondaryPP s = pure $ def
  { ppOrder  = \(_ : _ : _ : extras) -> extras
  , ppSep    = circleSep
  , ppExtras = [ logCurrentOnScreen s
               , logLayoutOnScreen s
               , shortenL 50 $ logTitleOnScreen s
               , logWhenActive s (logConst "*")
               ]
  }

trayerSB :: StatusBarConfig
trayerSB = statusBarGeneric
  (unwords
    [ "trayer"
    , "--edge top"
    , "--align right"
    , "--widthtype request"
    , "--expand true"
    , "--monitor primary"
    , "--transparent true"
    , "--alpha 0"
    , "-l"
    , "--tint 0x2E3440"
    , "--height 30"
    , "--margin 27"
    ]
  )
  mempty
