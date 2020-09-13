{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Data.List
import           Data.List.Split

import qualified Data.Map                      as M

-- import           ColorScheme
-- import           XFKeys
import           XMonad.Layout.Spiral

import           XMonad
import           XMonad.Config.Kde
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Gaps
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Util.Run



import qualified XMonad.StackSet               as W

import           System.IO

bgColor = "#B4D7F5"
fgColor = "#262d40"

xF86XK_AudioLowerVolume :: KeySym
xF86XK_AudioLowerVolume = 0x1008FF11

xF86XK_AudioMute :: KeySym
xF86XK_AudioMute = 0x1008FF12

xF86XK_AudioRaiseVolume :: KeySym
xF86XK_AudioRaiseVolume = 0x1008FF13

-- xF86XK_AudioPlay :: KeySym
-- xF86XK_AudioPlay = 0x1008FF14

-- xF86XK_AudioStop :: KeySym
-- xF86XK_AudioStop = 0x1008FF15

-- xF86XK_AudioPrev :: KeySym
-- xF86XK_AudioPrev = 0x1008FF16

-- xF86XK_AudioNext :: KeySym
-- xF86XK_AudioNext = 0x1008FF17

-- xF86XK_Search :: KeySym
-- xF86XK_Search = 0x1008FF1B

-- xF86XK_RFKill :: KeySym
-- xF86XK_RFKill = 0x1008FFB5

-- The needed funcionality

raiseVolume :: Int -> X ()
raiseVolume n =
  spawn $ "pactl set-sink-volume @DEFAULT_SINK@ +" ++ show n ++ "% "


lowerVolume :: Int -> X ()
lowerVolume n =
  spawn $ "pactl set-sink-volume @DEFAULT_SINK@ -" ++ show n ++ "% "

mute :: X ()
mute = spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"

-- They keymap

xF86XK_keyMap :: [((KeyMask, KeySym), X ())]
xF86XK_keyMap = [ ((0, keySym), action) | (keySym, action) <- bindings ]
 where
  bindings =
    [ (xF86XK_AudioRaiseVolume, raiseVolume 10)
    , (xF86XK_AudioLowerVolume, lowerVolume 10)
    , (xF86XK_AudioMute       , mute)
    ]

myModMask :: KeyMask
myModMask = mod4Mask

-- preprocess strings before sending to xmobar

-- Inspired from https://arch-ed.dk/xmobar-clickable-workspaces
-- I mainly didn't like the code, so I tried to make it more maintainable and more readable

myWorkspaces :: [String]
myWorkspaces = zipWith wsToActionAt keyBindings wss
 where
  keyBindings = [1 .. 9] ++ [0 :: Integer]
  -- Icons from Font Awesome. They need to be in unicode.
  wss         = zipWith (++) (map ((++ ":") . show) [(1 :: Int) .. 10]) $ map
    (wrap "<fn=1>" "</fn>")
    [ "\xf02d" -- 
    , "\xf120" -- 
    , "\xf268" -- 
    , "\xf108" -- 
    , "\xf108" -- 
    , "\xf108" -- 
    , "\xf108" -- 
    , "\xf095" -- 
    , "\xf2dc" -- 
    , "\xf11b \xf236" --  
    ]
  modString = modToString myModMask
  -- The utility xdotool must be on the system for this to work
  command at = "xdotool key " ++ modString ++ "+" ++ show at
  mouseKey = "1"
  wsToActionAt at = xmobarAction (command at) mouseKey

workspaceAt :: Int -> String
workspaceAt 0 = last myWorkspaces
workspaceAt n = ((myWorkspaces !!) . pred) n

-- explicit is better than implicit: don't use string representations of modifiers directly
modToString :: KeyMask -> String
modToString key | key == mod4Mask = "super"
                | key == mod1Mask = "alt"
                | otherwise       = undefined

myKeys conf@XConfig { XMonad.modMask = modm } =
  M.fromList
    $
    -- launch a terminal
       [ ( (modm, xK_Return)
         , spawn $ XMonad.terminal conf
         )

    -- launch rofi
       , ( (modm, xK_d)
         , spawn "rofi -modi run,drun -show drun"
         )

    -- launch gmrun
       , ( (modm .|. shiftMask, xK_p)
         , spawn "gmrun"
         )

    -- close focused window
       , ( (modm .|. shiftMask, xK_q)
         , kill
         )

     -- Rotate through the available layout algorithms
       , ( (modm, xK_space)
         , sendMessage NextLayout
         )

    --  Reset the layouts on the current workspace to default
       , ( (modm .|. shiftMask, xK_space)
         , setLayout $ XMonad.layoutHook conf
         )

    -- Resize viewed windows to the correct size
       , ( (modm, xK_n)
         , refresh
         )

    -- Move focus to the next window
       , ( (modm, xK_Tab)
         , windows W.focusDown
         )

    -- Move focus to the next window
       , ((modm, xK_j), windows W.focusDown)
       , ((modm, xK_a), sendMessage MirrorShrink)
       , ( (modm, xK_z)
         , sendMessage MirrorExpand
         )
    -- Move focus to the previous window
       , ( (modm, xK_k)
         , windows W.focusUp
         )

    -- Move focus to the master window
       , ( (modm, xK_m)
         , windows W.focusMaster
         )

    -- Swap the focused window and the master window
    -- , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
       , ( (modm .|. shiftMask, xK_j)
         , windows W.swapDown
         )

    -- Swap the focused window with the previous window
       , ( (modm .|. shiftMask, xK_k)
         , windows W.swapUp
         )

    -- Shrink the master area
       , ( (modm, xK_h)
         , sendMessage Shrink
         )

    -- Expand the master area
       , ( (modm, xK_l)
         , sendMessage Expand
         )

    -- Push window back into tiling
       , ( (modm, xK_t)
         , withFocused $ windows . W.sink
         )

    -- Increment the number of windows in the master area
       , ( (modm, xK_comma)
         , sendMessage (IncMasterN 1)
         )

    -- Deincrement the number of windows in the master area
       , ( (modm, xK_period)
         , sendMessage (IncMasterN (-1))
         )
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
       , ( (modm, xK_b)
         , sendMessage ToggleStruts
         )

    -- Quit xmonad
       , ( (modm .|. shiftMask, xK_x)
         , spawn
           "qdbus org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout -1 -1 -1"
         )
    -- Toggle full screen
       , ( (modm, xK_f)
         , doFullScreen
         )
    -- Restart xmonad
       , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
       ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
        [ ((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
        [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
       , (f  , m ) <- [(W.view, 0), (W.shift, shiftMask)]
       ]

    ++ xF86XK_keyMap

myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
  [ ( (modm, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )

    -- mod-button2, Raise the window to the top of the stack
  , ( (modm, button2)
    , \w -> focus w >> windows W.shiftMaster
    )

    -- mod-button3, Set the window to floating mode and resize by dragging
  , ( (modm, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )
  ]

-- Since gaps and spacing are enabled, this is a little different than the typical answer you  find online
doFullScreen :: X ()
doFullScreen = do
  sendMessage $ Toggle FULL
  sendMessage ToggleStruts
  toggleScreenSpacingEnabled
  sendMessage ToggleGaps

myLayout =
  smartBorders -- imporvments
    .   spacingRaw True (Border 0 10 10 10) True (Border 5 5 5 5) True -- between windows
    .   gaps [(U, 42), (R, 10), (L, 10), (D, 42)] -- along the screen
    .   avoidStruts -- show the bar
    .   mkToggle (NOBORDERS ?? FULL ?? EOT) -- toggle full screen
    $   tiled
    ||| Mirror tiled
    ||| Full
    ||| spiral (6 / 7)
 where
  tiled   = ResizableTall nmaster delta ratio []

  -- The default number of windows in the master pane
  nmaster = 1

  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2

  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

-- Note: If you google "Full screen support xmonad", you'd be pointed towards adding
-- a rule in the manage hook, and adding an event handler. The problem with that approach,
-- is that it isn't really flexible: sometimes I want to create that "picture-in-picture" feel
-- of youtube videos for example, without relying on third party modules (because they might not work
-- nicely with xmonad). For that reason, every application might "think" it is in fullscreen mode,
-- and change its looks, and I choose which one is in fullscreen mode with respect to the whole
-- system with mod-f. Suits me well!
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
  floatsClasses = ["MPlayer", "Gimp", "yakuake", "Plasma-desktop", "ksmserver"]
  floatsTitles  = ["alsamixer"]

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
  W.Stack t [] (r : rs) -> W.Stack t [r] rs
  _                     -> c

-- Expects the window name at the third place
formatOutput :: String -> (String, String)
formatOutput s
  | length splitted <= 2 = (intercalate actualSeparator splitted, greeting)
  | otherwise = (intercalate actualSeparator (init splitted), last splitted)
 where
  splitted = splitOn weirdSeparator s
  greeting = "Hey you, you're finally awake..."


outputLogger :: Handle -> Handle -> String -> IO ()
outputLogger topBar bottomBar output = do
  let (topOutput, bottomOutput) = formatOutput output
  hPutStrLn topBar    topOutput
  hPutStrLn bottomBar bottomOutput

-- Well... this is a hack
weirdSeparator :: String
weirdSeparator = ",;.#"

actualSeparator :: String
actualSeparator = "|"

spawnXMobar :: String -> IO Handle
spawnXMobar location = spawnPipe xmobarCommand
 where
  xmobarCommand = prefix ++ location ++ ".hs"
  prefix =
    "/home/yecinem/.local/bin/xmobar -x 0"
      ++ " -B "
      ++ wrap "\"" "\"" bgColor
      ++ " -F "
      ++ wrap "\"" "\"" fgColor
      ++ " /home/yecinem/.xmonad/xmobar_"

main :: IO ()
main = do

  xmproc_top    <- spawnXMobar "top"
  xmproc_bottom <- spawnXMobar "bottom"
  let bar = xmobarPP { ppOutput          = outputLogger xmproc_top xmproc_bottom
                     , ppTitle           = shorten 50
                     , ppCurrent         = xmobarBorder "Bottom" fgColor 4
                     , ppUrgent          = xmobarBorder "Bottom" "#CD3C66" 4
                     , ppHiddenNoWindows = xmobarColor "#98a0b3" ""
                     , ppSep             = weirdSeparator
                     }

  xmonad $ ewmh $ kde4Config
    { manageHook = manageDocks <+> myManageHook <+> manageHook kde4Config
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , mouseBindings      = myMouseBindings
    , keys               = \c -> myKeys c `M.union` keys kde4Config c
    , layoutHook         = myLayout
    , handleEventHook    = handleEventHook kde4Config -- <+> fullscreenEventHook
    , focusedBorderColor = bgColor
    , normalBorderColor  = fgColor
    , logHook            = dynamicLogWithPP bar
    }
