{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Config where

import           ColorScheme
import qualified Data.Map                      as M
import           Graphics.X11.ExtraTypes.XF86

import           System.Posix.Env               ( putEnv )
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           Data.Function
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.DynamicStatusBarConfigs
import           XMonad.Util.EZConfig
import           XMonad.Hooks.ManageHelpers
-- Layouts
import           XMonad.Layout.Gaps
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Renamed
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile

import           XMonad.Layout.Spacing
import qualified XMonad.StackSet               as W
import           XMonad.Util.Loggers
-- For chromium
import           XMonad.Util.Hacks              ( windowedFullscreenFixEventHook
                                                )

import           XMonad.Util.ClickableWorkspaces

-- contrib:
--   * diff from https://github.com/xmonad/xmonad-contrib/pull/471 for fixing encoding
--   * javaHack: added manually, it's merged though
--   * FIXME stack at https://github.com/xmonad/xmonad-contrib/pull/463 for dynamic status bars.
--

javaHack :: XConfig l -> XConfig l
javaHack conf = conf
  { startupHook = startupHook conf
                    *> io (putEnv "_JAVA_AWT_WM_NONREPARENTING=1")
  }

---------------------
-- The config
---------------------

privateConfig =
  withEmacs . javaHack . dynamicSBs barSpawner . ewmh . docks $ def
    { manageHook         = manageDocks <> myManageHook
    , modMask            = mod4Mask
    , workspaces         = myWorkspaces
    , mouseBindings      = myMouseBindings
    , keys               = myKeys <> keys def
    , layoutHook         = myLayout
    , handleEventHook    = windowedFullscreenFixEventHook
    , focusedBorderColor = fgColor
    , normalBorderColor  = bgColor
    , terminal           = "termite"
    }

myModMask :: KeyMask
myModMask = mod4Mask

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

workspaceAt :: Int -> String
workspaceAt 0 = last myWorkspaces
workspaceAt n = ((myWorkspaces !!) . pred) n

-- explicit is better than implicit: don't use string representations of modifiers directly
modToString :: KeyMask -> String
modToString key | key == myModMask = "super"
                | key == mod1Mask  = "alt"
                | otherwise        = undefined



soundCommand :: String -> X ()
soundCommand = spawn . ("pactl set-sink-volume @DEFAULT_SINK@ " <>)

lowerVolume, raiseVolume :: Int -> X ()
raiseVolume n = soundCommand $ "+" <> show n <> "%"
lowerVolume n = soundCommand $ "-" <> show n <> "%"

mute :: X ()
mute = soundCommand "toggle"

xF86XK_keyMap = [ ((0, keySym), action) | (keySym, action) <- bindings ]
 where
  bindings =
    [ (xF86XK_AudioRaiseVolume, raiseVolume 10)
    , (xF86XK_AudioLowerVolume, lowerVolume 10)
    , (xF86XK_AudioMute       , mute)
    ]

myKeys XConfig {..} =
  M.fromList
    $
    -- launch a terminal
       [ ((modMask, xK_Return), spawn terminal)
       ,
      -- launch rofi
         ((modMask, xK_d), spawn "rofi -modi run,drun -show drun")
       ,
      -- close focused window
         ((modMask .|. shiftMask, xK_q), kill)
       ,
      -- Rotate through the available layout algorithms
         ((modMask, xK_space), sendMessage NextLayout)
       ,
      --  Reset the layouts on the current workspace to default
         ((modMask .|. shiftMask, xK_space), setLayout layoutHook)
       ,
      -- Resize viewed windows to the correct size
         ((modMask, xK_n), refresh)
       ,
      -- Move focus to the next window
         ((modMask, xK_Tab), windows W.focusDown)
       , ((modMask, xK_a), sendMessage MirrorShrink)
       , ((modMask, xK_z), sendMessage MirrorExpand)
       ,
      -- Move focus to the previous window
         ((modMask, xK_k), windows W.focusUp)
       ,
      -- Move focus to the master window
         ((modMask, xK_m), windows W.focusMaster)
       ,
      -- Swap the focused window and the master window
      -- , ((modMask,               xK_Return), windows W.swapMaster)

      -- Swap the focused window with the next window
         ((modMask .|. shiftMask, xK_j), windows W.swapDown)
       ,
      -- Swap the focused window with the previous window
         ((modMask .|. shiftMask, xK_k), windows W.swapUp)
       ,
      -- Shrink the master area
         ((modMask, xK_h), sendMessage Shrink)
       ,
      -- Expand the master area
         ((modMask, xK_l), sendMessage Expand)
       ,
      -- Push window back into tiling
         ((modMask, xK_t), withFocused $ windows . W.sink)
       ,
      -- Increment the number of windows in the master area
         ((modMask, xK_comma), sendMessage (IncMasterN 1))
       ,
      -- Deincrement the number of windows in the master area
         ((modMask, xK_period), sendMessage (IncMasterN (-1)))
       ,
      -- Toggle the status bar gap
      -- Use this binding with avoidStruts from Hooks.ManageDocks.
      -- See also the statusBar function from Hooks.DynamicLog.
      --
         ((modMask, xK_b), doCollapse)
       ,
      -- Quit xmonad
         ( (modMask .|. shiftMask, xK_x)
         , spawn
           "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1"
         )
       ,
      -- Toggle full screen
         ((modMask, xK_f), doFullScreen)
       ,
      -- Restart xmonad
         ((modMask, xK_q), spawn "xmonad --recompile; xmonad --restart")
       ]
    ++
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
    ++ xF86XK_keyMap

myMouseBindings XConfig { XMonad.modMask = modMask } = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
  [ ( (modMask, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )
  ,
      -- mod-button2, Raise the window to the top of the stack
    ((modMask, button2), \w -> focus w >> windows W.shiftMaster)
  ,
      -- mod-button3, Set the window to floating mode and resize by dragging
    ( (modMask, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )
  ]

-- Emacs
-- TODO debug emacs-everywhere
withEmacs :: XConfig l -> XConfig l
withEmacs = flip
  additionalKeysP
  [ ("M-e"  , spawn "emacsclient -create-frame --no-wait")
  , ("M-S-e", spawn "/home/yecinem/.emacs.d/bin/doom everywhere")
  ]

doFullScreen :: X ()
doFullScreen = do
  sendMessage $ Toggle NBFULL
  doCollapse

doCollapse :: X ()
doCollapse = do
  sendMessage ToggleStruts
  toggleWindowSpacingEnabled
  sendMessage ToggleGaps

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
    , fmap not isDialog --> doF avoidMaster
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
-- TODO : cleanup

-- Some loggers
greeter :: Logger
greeter = logConst "Hey you, you're finally awake..."

logTitleOrGreet = shortenL 50 (logTitleOnScreen 0) .| greeter

-- Add the background and forground colors, the location and the
-- screen to the xmobar command.
getXMobarCommand :: String -> ScreenId -> String
getXMobarCommand location (S screen) = prefix ++ location ++ ".hs"
 where
  prefix =
    "xmobar -x "
      ++ show screen
      ++ " -B "
      ++ wrap "\"" "\"" bgColor
      ++ " -F "
      ++ wrap "\"" "\"" fgColor
      ++ " $HOME/.xmonad/app/xmobar_"



-- | A helper datatype for XMobar
data XMobar = XMobar {location :: String, screen :: ScreenId, pp :: PP}

onWorkspaces :: (String -> String) -> PP -> PP
onWorkspaces f pp@(PP {..}) = pp { ppCurrent         = ppCurrent . f
                                 , ppUrgent          = ppUrgent . f
                                 , ppHiddenNoWindows = ppHiddenNoWindows . f
                                 , ppVisible         = ppVisible . f
                                 , ppHidden          = ppHidden . f
                                 }

-- Main Screen
-- bottomMainPP,
topMainPP :: PP

topMainPP = def
  { ppCurrent         = xmobarBorder "Bottom" fgColor 4
  , ppUrgent          = xmobarBorder "Bottom" "#CD3C66" 4
  , ppHiddenNoWindows = xmobarColor "#98a0b3" ""
  , ppVisible         = xmobarBorder "Bottom" "#98a0b3" 1
  , ppSep             = "|"
  , ppExtras          = [logLayoutOnScreen 0, shortenL 50 $ logTitleOrGreet]
  , ppOrder           = \(ws : _ : _ : extras) -> ws : extras
  }
-- bottomMainPP = def { ppOrder  = \(_ : _ : _ : extras) -> extras
--                    , ppExtras = [logTitleOrGreet]
--                    }

-- Secondary screens
secondaryBar :: ScreenId -> XMobar
secondaryBar n = XMobar "secondary" n $ def
  { ppOrder  = \(_ : _ : _ : extras) -> extras
  , ppSep    = " | "
  , ppExtras = [ logCurrentOnScreen n
               , logLayoutOnScreen n
               , logWhenActive n (logConst "*") .| logConst " "
               , shortenL 50 $ logTitleOnScreen n
               ]
  }

-- Transform XMobar to IO StatusBarConfig
mySB :: XMobar -> IO StatusBarConfig
mySB XMobar {..} = statusBarHandleConfig (getXMobarCommand location screen) pp

topXMobar =
  statusBarHandleConfig' (getXMobarCommand "top" 0) (clickablePP topMainPP)
-- bottomXMobar = mySB $ XMobar "bottom" 0 bottomMainPP

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = topXMobar -- <> bottomXMobar
barSpawner n = mySB $ secondaryBar n
