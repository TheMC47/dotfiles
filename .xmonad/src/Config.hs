{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Config where

import           ColorScheme

import qualified Data.Map                      as M
import           XMonad.Hooks.UrgencyHook

import           XMonad
-- Hooks
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
-- Status bar
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
-- Layouts
import           XMonad.Layout.Gaps
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet               as W
import           XMonad.Util.ClickableWorkspaces
-- Utils
import           XMonad.Util.EZConfig
-- For chromium
import           XMonad.Util.Hacks
import           XMonad.Util.Loggers


-- contrib:
--   * diff from https://github.com/xmonad/xmonad-contrib/pull/471 for fixing encoding
--   * stack at https://github.com/xmonad/xmonad-contrib/pull/463 for dynamic status bars.
--
---------------------
-- The config
---------------------


privateConfig =
  withEmacs
    .                 withMediaKeys
    .                 javaHack
    .                 withUrgencyHook NoUrgencyHook
    .                 dynamicSBs barSpawner
    -- . ewmh' def { fullscreen = True, workspaceListSort = pure reverse }
    .                 ewmh
    .                 docks
    $                 def { manageHook         = manageDocks <> myManageHook
                          , modMask            = myModMask
                          , workspaces         = myWorkspaces
                          , keys               = myKeys <> keys def
                          , layoutHook         = myLayout
                          , handleEventHook    = windowedFullscreenFixEventHook
                          , focusedBorderColor = fgColor
                          , normalBorderColor  = bgColor
                          , terminal           = myTerminal
                          , startupHook        = spawn "pkill xembedsniproxy"
                          }
    `additionalKeysP` [ ("M-<Return>", spawn myTerminal)
                      , ("M-d"       , rofi)
                      , ("M-S-q"     , kill)
                      , ("M-b"       , toggleCollapse)
                      , ("M-f"       , toggleFullScreen)
                      , ("M-S-x"     , logout)
                      , ("M-q"       , xmonadRecompile)
                      ]

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig {..} =
  M.fromList
    $
      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
       [ ((m .|. modMask, k), windows $ f i)
       | (i, k) <- zip workspaces ([xK_1 .. xK_9] ++ [xK_0])
       , (f, m) <-
         [(W.view, 0), (W.shift, shiftMask), (W.greedyView, controlMask)]
       ]
    ++
      --
      -- mod-{i,o}, Switch to physical/Xinerama screens 2, 1
      -- mod-shift-{i,o}, Move client to screen 2, 1
      --
       [ ( (m .|. modMask, key)
         , screenWorkspace sc >>= flip whenJust (windows . f)
         )
       | (key, sc) <- zip [xK_o, xK_i] [0 ..]
       , (f  , m ) <- [(W.view, 0), (W.shift, shiftMask)]
       ]
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "termite"

myWorkspaces :: [String]
myWorkspaces = zipWith -- euum. yeah. I know. overengineered
  (<>)
  (map ((<> ":") . show) [(1 :: Int) .. 10])
  [ "\xf02d"
  , -- 
    "\xf120"
  , -- 
    "\xf268"
  , -- 
    "\xf108"
  , -- 
    "\xf108"
  , -- 
    "\xf108"
  , -- 
    "\xf108"
  , -- 
    "\xf095"
  , -- 
    "\xf2dc"
  , -- 
    "\xf11b \xf236" --  
  ]

---------
--- Actions
---------
logout :: X ()
logout =
  spawn
    "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1"

rofi :: X ()
rofi = spawn "rofi -m -4 -modi run,drun -show drun"

xmonadRecompile :: X ()
xmonadRecompile = spawn "xmonad --recompile; xmonad --restart"


toggleFullScreen :: X ()
toggleFullScreen = do
  sendMessage $ Toggle NBFULL
  toggleCollapse


toggleCollapse :: X ()
toggleCollapse = do
  sendMessage ToggleStruts
  toggleWindowSpacingEnabled
  sendMessage ToggleGaps

workspaceAt :: Int -> String
workspaceAt 0 = last myWorkspaces
workspaceAt n = ((myWorkspaces !!) . pred) n

---------------------
-- Media keys
---------------------

volumeCommand :: String -> X ()
volumeCommand = spawn . ("pactl set-sink-volume @DEFAULT_SINK@ " <>)

lowerVolume, raiseVolume :: Int -> X ()
raiseVolume n = volumeCommand $ "+" <> show n <> "%"
lowerVolume n = volumeCommand $ "-" <> show n <> "%"

mute :: X ()
mute = spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"

withMediaKeys :: XConfig l -> XConfig l
withMediaKeys = flip
  additionalKeysP
  [ ("<XF86AudioRaiseVolume>", raiseVolume 10)
  , ("<XF86AudioLowerVolume>", lowerVolume 10)
  , ("<XF86AudioMute>"       , mute)
  ]


---------------------
-- Emacs
---------------------

-- TODO debug emacs-everywhere
withEmacs :: XConfig l -> XConfig l
withEmacs = flip
  additionalKeysP
  [ ("M-e"  , spawn "emacsclient -create-frame --no-wait")
  , ("M-S-e", spawn "/home/yecinem/.emacs.d/bin/doom everywhere")
  ]
---------------------
-- Layouts
---------------------
myLayout =
  renamed [CutWordsLeft 1]
    .   avoidStruts
    .   spacingRaw False (Border 0 0 0 0) True (Border 10 10 10 10) True -- between windows
    .   gaps [(U, 15), (R, 15), (L, 15), (D, 15)] -- along the screen, excluding docks
    .   mkToggle (single NBFULL) -- toggle full screen
    .   smartBorders
    $   tiled
    ||| mtiled
    ||| full
 where
  tiled  = renamed [Replace "T "] $ ResizableTall 1 (3 / 100) (5 / 8) []
  mtiled = renamed [Replace "MT"] $ Mirror tiled
  full   = renamed [Replace "F "] Full

---------------------
-- ManageHook
---------------------

myManageHook :: ManageHook
myManageHook = composeAll manageHooks
 where
  manageHooks = generalRules ++ concat windowRules
  generalRules =
    [ className =? "discord" --> doShift (workspaceAt 8)
    , not <$> isDialog --> doF avoidMaster
    , checkDock --> doLower
    ]
  windowRules =
    [ [ className =? c --> doFloat | c <- floatsClasses ]
    , [ title =? t --> doFloat | t <- floatsTitles ]
    ]
  floatsClasses =
    ["MPlayer", "Gimp", "yakuake", "Plasma-desktop", "ksmserver", "R_x11"]
  floatsTitles = ["alsamixer"]

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
  W.Stack t [] (r : rs) -> W.Stack t [r] rs
  _                     -> c


---------------------
-- Status Bars
---------------------
-- TODO figure out how to make trayer dynamic

circleSep :: String
circleSep =
  "<icon=circle_right.xpm/></fc>  <fc=#D8DEE9,#2E3440:0><icon=circle_left.xpm/>"

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 =
  statusBarPipe
      "xmobar top"
      (clickablePP def
        { ppCurrent         = xmobarBorder "Bottom" fgColor 4
        , ppUrgent          = xmobarBorder "Bottom" "#CD3C66" 4
        , ppHiddenNoWindows = xmobarColor "#98a0b3" "#2E3440:0"
        , ppVisible         = xmobarBorder "Bottom" "#98a0b3" 1
        , ppSep             = circleSep
        , ppExtras = [logLayoutOnScreen 0, shortenL 50 (logTitleOnScreen 0)]
        , ppOrder           = \(ws : _ : _ : extras) -> ws : extras
        }
      )
    <> trayerSB
barSpawner s@(S n) = statusBarPipe
  ("xmobar secondary " <> show n)
  (pure $ def
    { ppOrder  = \(_ : _ : _ : extras) -> extras
    , ppSep    = circleSep
    , ppExtras = [ logCurrentOnScreen s
                 , logLayoutOnScreen s
                 , shortenL 50 $ logTitleOnScreen s
                 , logWhenActive s (logConst "*")
                 ]
    }
  )

trayerSB :: IO StatusBarConfig
trayerSB = do
  sb <- statusBarProp trayerCommand def
  return $ sb { sbLogHook = mempty }

trayerCommand :: String
trayerCommand = unwords
  [ "trayer"
  , "--edge top"
  , "--align right"
  , "--widthtype request"
  , "--SetDockType true"
  , "--SetPartialStrut true"
  , "--expand true"
  , "--monitor 1"
  , "--transparent true"
  , "--alpha 0"
  , "--tint 0x2E3440"
  , "--height 30"
  , "--margin 18"
  ]
