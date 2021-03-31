{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import qualified Data.Map                      as M
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Decoration
import           XMonad.Layout.FixedAspectRatio
import           XMonad.Layout.Gaps
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet               as W
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.EZConfig
import           XMonad.Util.Hacks
import           XMonad.Util.Loggers

bgColor = "#2E3440"
fgColor = "#D8DEE9"

main :: IO ()
main =
    xmonad
        .                 withEmacs
        .                 withMediaKeys
        .                 withStatusBars
    -- library combiators
        .                 javaHack
        .                 withUrgencyHook NoUrgencyHook
    -- . ewmh' def { fullscreen = True, workspaceListSort = pure reverse }
    -- .                 addDescrKeys ((myModMask, xK_F1), xMessage) myKeys'
        .                 ewmh
        $                 def { manageHook         = myManageHook
                              , modMask            = myModMask
                              , workspaces         = myWorkspaces
                              , keys               = myKeys <> keys def
                              , layoutHook         = myLayout
                              , handleEventHook = windowedFullscreenFixEventHook
                              , focusedBorderColor = fgColor
                              , normalBorderColor  = bgColor
                              , terminal           = myTerminal
                              , startupHook = spawn "pkill xembedsniproxy"
                              }
        `additionalKeysP` [ ("M-<Return>", spawn myTerminal)
                          , ("M-d"       , rofi)
                          , ("M-b"       , toggleCollapse)
                          , ("M-f"       , toggleFullScreen)
                          , ("M-S-q"     , kill)
                          , ("M-S-x"     , logout)
                          , ("M-q"       , xmonadRecompile)
                          , ("M-z +"     , incWindowSpacing 10)
                          , ("M-c"       , windows copyToAll)
                          , ("M-v"       , killAllOtherCopies)
                          , ( "M-a 6"
                            , withFocused $ sendMessage . FixRatio (16 / 9)
                            )
                          , ( "M-a 4"
                            , withFocused $ sendMessage . FixRatio (4 / 3)
                            )
                          , ("M-a r", withFocused $ sendMessage . ResetRatio)
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
    renamed [CutWordsLeft 2]
        .   fixedAspectRatio (0.5, 0.5)
        .   layoutHintsWithPlacement (0.5, 0.5)
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
myManageHook = mconcat manageHooks
  where
    manageHooks = generalRules ++ concat windowRules
    generalRules =
        [ className =? "discord" --> doShift (workspaceAt 8)
        , not <$> isDialog --> doF avoidMaster
        , title =? "Netflix" <||> className =? "vlc" --> doFixAspect (16 / 9)
        ]
    windowRules =
        [ [ className =? c --> doFloat | c <- floatsClasses ]
        , [ title =? t --> doFloat | t <- floatsTitles ]
        , [ className =? c --> doIgnore | c <- ignoreClasses ]
        ]
    floatsClasses =
        ["MPlayer", "Gimp", "yakuake", "Plasma-desktop", "ksmserver", "R_x11"]
    floatsTitles  = ["alsamixer"]
    ignoreClasses = ["krunner"]


avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r : rs) -> W.Stack t [r] rs
    _                     -> c


---------------------
-- Status Bars
---------------------

withStatusBars
    :: LayoutClass l Window
    => XConfig l
    -> XConfig (ModifiedLayout AvoidStruts l)
withStatusBars c = docks . dynamicSBs barSpawner $ c
    { handleEventHook = handleEventHook c -- <> trayerAboveXmobarEventHook
    , layoutHook      = avoidStruts (layoutHook c)
    }


circleSep :: String
circleSep =
    "<icon=circle_right.xpm/></fc>  <fc=#D8DEE9,#2E3440:0><icon=circle_left.xpm/>"

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 =
    statusBarProp
            "xmobar top"
            (clickablePP def
                { ppCurrent         = xmobarBorder "Bottom" fgColor 4
                , ppUrgent          = xmobarBorder "Bottom" "#CD3C66" 4
                , ppHiddenNoWindows = xmobarColor "#98a0b3" "#2E3440:0"
                , ppVisible         = xmobarBorder "Bottom" "#98a0b3" 1
                , ppSep             = circleSep
                , ppExtras          = [ logLayoutOnScreen 0
                                      , shortenL 50 (logTitleOnScreen 0)
                                      ]
                , ppOrder           = \(ws : _ : _ : extras) -> ws : extras
                }
            )
        <> trayerSB
barSpawner s@(S n) = statusBarPropTo
    customProp
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
    where customProp = "_XMONAD_LOG__Secondary_" <> show n

staticStatusBar cmd = pure $ def { sbStartupHook = spawnStatusBar cmd
                                 , sbCleanupHook = killStatusBar cmd
                                 }

trayerSB :: IO StatusBarConfig
trayerSB = staticStatusBar
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
