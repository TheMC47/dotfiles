{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import           Data.Bifunctor
import qualified Data.Map                      as M
import           XMonad
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.FixedAspectRatio
import           XMonad.Layout.Gaps
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.OrgMode
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet               as W
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.EZConfig
import           XMonad.Util.Hacks
import           XMonad.Util.Loggers
import           XMonad.Util.NamedScratchpad

myBgColor = "#2E3440"

myFgColor = "#D8DEE9"

main :: IO ()
main =
  xmonad
    .                 docks
    .                 dynamicSBs barSpawner
    .                 javaHack
    .                 withUrgencyHook NoUrgencyHook
    -- . ewmh' def {workspaceListSort = pure reverse }
    .                 ewmh
    $ def { manageHook = myManageHook <> namedScratchpadManageHook scratchpads
          , modMask            = myModMask
          , workspaces         = myWorkspaces
          , keys               = myKeys <> keys def
          , layoutHook         = myLayout
          , handleEventHook    = windowedFullscreenFixEventHook
          , logHook            = updatePointer (0.5, 0.5) (0, 0)
          , focusedBorderColor = myFgColor
          , normalBorderColor  = myBgColor
          , terminal           = myTerminal
          , startupHook        = spawn "pkill xembedsniproxy"
          }
    `additionalKeysP` (  [ ("M-<Return>", spawn myTerminal)
                         , ( "M-S-<Return>"
                           , namedScratchpadAction scratchpads "dropdown-term"
                           )
                         , ("M-d"  , rofi)
                         , ("M-b"  , toggleCollapse)
                         , ("M-f"  , toggleFullScreen)
                         , ("M-S-q", kill)
                         , ("M-S-x", logout)
                         , ("M-q"  , xmonadRecompile)
                         , ( "M-a 6"
                           , withFocused (broadcastMessage . FixRatio (16 / 9))
                             >> refresh
                           )
                         , ( "M-a r"
                           , withFocused (broadcastMessage . ResetRatio)
                             >> refresh
                           )
                         ,
                            -- Prompts
                           ("M-x", xmonadPrompt myXPConfig)
                         ]
                      ++ screenKeys
                      ++ emacsKeys
                      )
 where
  screenKeys =
    [ ("M-" <> m <> show k, screenWorkspace s >>= flip whenJust (windows . f))
    | (k, s) <- zip "op" [0 ..]
    , (m, f) <- zip ["", "S-"] [W.view, W.shift]
    ]
  emacsKeys = makeSubmap
    "e"
    (spawn emacs)
    [ ("t"  , namedScratchpadAction scratchpads "todos")
    , ("o"  , orgPrompt myXPConfig "TODO" "org/todos.org")
    , ("S-o", orgPromptPrimary myXPConfig "TODO" "org/todos.org")
    ]

-- | A small helper function to make submaps. For the given key k,
-- it binds @M-k@ to the given action, and prefixes the keys in the
-- list with @M-C-k@
makeSubmap :: String -> X () -> [(String, X ())] -> [(String, X ())]
makeSubmap k action ks =
  ("M-" <> k, action) : map (first (("M-S-" <> k <> " ") <>)) ks

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

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = zipWith -- euum. yeah. I know. overengineered
  (<>)
  (map ((<> ":") . show) [(1 :: Int) .. 10])
  [ "\xf02d"
  ,
      -- 
    "\xf120"
  ,
      -- 
    "\xf268"
  ,
      -- 
    "\xf108"
  ,
      -- 
    "\xf108"
  ,
      -- 
    "\xf108"
  ,
      -- 
    "\xf108"
  ,
      -- 
    "\xf095"
  ,
      -- 
    "\xf2dc"
  ,
      -- 
    "\xf11b \xf236" --  
  ]

--------
--- Scratchpads
--------

scratchpads =
  [ NS "todos"
       todoCommand
       (title =? todoTitle)
       (customFloating $ W.RationalRect (1 / 6) 0 (2 / 3) (1 / 3))
  , NS "dropdown-term"
       (withTerminal . withTitleT dropDownTitle $ "")
       (title =? dropDownTitle)
       (customFloating $ W.RationalRect (1 / 6) 0 (2 / 3) (2 / 3))
  ]
 where
  todoTitle     = "TODOs"
  todoCommand   = withEmacs . withTitle todoTitle $ "org/todos.org"
  dropDownTitle = "Dropdown"

--------
--- Emacs
--------

myTerminal :: String
myTerminal = "termite "

withTerminal :: ShowS
withTerminal = (myTerminal <>)

withTitleT :: String -> ShowS
withTitleT c = (("--title=" <> c <> " ") <>)

emacs :: String
emacs = "emacsclient -a '' -create-frame --no-wait "

withTitle :: String -> ShowS
withTitle t = (("-F '(quote (name . \"" <> t <> "\"))' ") <>)

withEmacs :: ShowS
withEmacs = (emacs <>)

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
-- Layouts
---------------------

myLayout =
  renamed [KeepWordsRight 1]
    .   avoidStruts
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
    ]
  floatsClasses =
    ["MPlayer", "Gimp", "yakuake", "Plasma-desktop", "ksmserver", "R_x11"]
  floatsTitles = ["alsamixer"]

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
  W.Stack t [] (r : rs) -> W.Stack t [r] rs
  _                     -> c

---------------------
-- EWMH
---------------------

-- myActivateHook :: ManageHook
-- myActivateHook  = composeOne [
--   className =? "Google-chrome" <||> className =? "google-chrome" -?> doAskUrgent,
--   pure True -?> doFocus]

---------------------
-- Status Bars
---------------------

circleSep :: String
circleSep =
  "<icon=circle_right.xpm/></fc>  <fc=#D8DEE9,#2E3440:0><icon=circle_left.xpm/>"

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 =
  pure
      (statusBarProp
        "xmobar top"
        (clickablePP . filterOutWsPP [scratchpadWorkspaceTag] $ def
          { ppCurrent         = xmobarBorder "Bottom" myFgColor 4
          , ppUrgent          = xmobarBorder "Bottom" "#CD3C66" 4
          , ppHiddenNoWindows = xmobarColor "#98a0b3" "#2E3440:0"
          , ppVisible         = xmobarBorder "Bottom" "#98a0b3" 1
          , ppSep             = circleSep
          , ppExtras = [logLayoutOnScreen 0, shortenL 50 (logTitleOnScreen 0)]
          , ppOrder           = \(ws : _ : _ : extras) -> ws : extras
          }
        )
      )
    <> trayerSB
barSpawner s@(S n) = pure $ statusBarPropTo
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

---------------------
-- Prompt
---------------------
myXPConfig :: XPConfig
myXPConfig = def { font = "xft:Source Code Pro:size=14:regular:antialias=true"
                 , position        = CenteredAt (1 / 4) (2 / 3)
                 , bgColor         = myBgColor
                 , fgColor         = myFgColor
                 , bgHLight        = myFgColor
                 , fgHLight        = myBgColor
                 , borderColor     = myFgColor
                 , searchPredicate = fuzzyMatch
                 , sorter          = fuzzySort
                 , height          = 30
                 }
