{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Config where

import           ColorScheme
import qualified Data.Map                      as M
import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           Control.Monad                  ( when )
import           XMonad
import           XMonad.Config.Kde
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           Data.Monoid                    ( All(All) )
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Gaps
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet               as W
import           XMonad.Util.Run
import           XMonad.Util.Loggers



myModMask :: KeyMask
myModMask = mod4Mask

altMask :: KeyMask
altMask = mod1Mask


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
modToString key | key == myModMask = "super"
                | key == mod1Mask  = "alt"
                | otherwise        = undefined

raiseVolume :: Int -> X ()
raiseVolume n =
  spawn $ "pactl set-sink-volume @DEFAULT_SINK@ +" ++ show n ++ "% "


lowerVolume :: Int -> X ()
lowerVolume n =
  spawn $ "pactl set-sink-volume @DEFAULT_SINK@ -" ++ show n ++ "% "

mute :: X ()
mute = spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"

xF86XK_keyMap = [ ((0, keySym), action) | (keySym, action) <- bindings ]
 where
  bindings =
    [ (xF86XK_AudioRaiseVolume, raiseVolume 10)
    , (xF86XK_AudioLowerVolume, lowerVolume 10)
    , (xF86XK_AudioMute       , mute)
    ]

myKeys conf@XConfig { XMonad.modMask = modm } =
  M.fromList
    $
    -- launch a terminal
       [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
       ,
      -- launch rofi
         ((modm, xK_d), spawn "rofi -modi run,drun -show drun")
       ,
      -- launch gmrun
         ((modm .|. shiftMask, xK_p), spawn "gmrun")
       ,
      -- close focused window
         ((modm .|. shiftMask, xK_q), kill)
       ,
      -- Rotate through the available layout algorithms
         ((modm, xK_space), sendMessage NextLayout)
       ,
      --  Reset the layouts on the current workspace to default
         ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
       ,
      -- Resize viewed windows to the correct size
         ((modm, xK_n), refresh)
       ,
      -- Move focus to the next window
         ((modm, xK_Tab), windows W.focusDown)
       ,
      -- Move focus to the next window
         ((modm, xK_j), windows W.focusDown)
       , ((modm, xK_a), sendMessage MirrorShrink)
       , ((modm, xK_z), sendMessage MirrorExpand)
       ,
      -- Move focus to the previous window
         ((modm, xK_k), windows W.focusUp)
       ,
      -- Move focus to the master window
         ((modm, xK_m), windows W.focusMaster)
       ,
      -- Swap the focused window and the master window
      -- , ((modm,               xK_Return), windows W.swapMaster)

      -- Swap the focused window with the next window
         ((modm .|. shiftMask, xK_j), windows W.swapDown)
       ,
      -- Swap the focused window with the previous window
         ((modm .|. shiftMask, xK_k), windows W.swapUp)
       ,
      -- Shrink the master area
         ((modm, xK_h), sendMessage Shrink)
       ,
      -- Expand the master area
         ((modm, xK_l), sendMessage Expand)
       ,
      -- Push window back into tiling
         ((modm, xK_t), withFocused $ windows . W.sink)
       ,
      -- Increment the number of windows in the master area
         ((modm, xK_comma), sendMessage (IncMasterN 1))
       ,
      -- Deincrement the number of windows in the master area
         ((modm, xK_period), sendMessage (IncMasterN (-1)))
       ,
      -- Toggle the status bar gap
      -- Use this binding with avoidStruts from Hooks.ManageDocks.
      -- See also the statusBar function from Hooks.DynamicLog.
      --
         ((modm, xK_b), sendMessage ToggleStruts)
       ,
      -- Quit xmonad
         ( (modm .|. shiftMask, xK_x)
         , spawn
           "qdbus org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout -1 -1 -1"
         )
       ,
      -- Toggle full screen
         ((modm, xK_f), doFullScreen)
       ,
      -- Restart xmonad
         ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
       ]
    ++
      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
       [ ((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
       , (f, m) <-
         [(W.view, 0), (W.shift, shiftMask), (W.greedyView, controlMask)]
       ]
    ++
      --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
       [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
       ]
    ++ xF86XK_keyMap

myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
  [ ( (modm, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )
  ,
      -- mod-button2, Raise the window to the top of the stack
    ((modm, button2), \w -> focus w >> windows W.shiftMaster)
  ,
      -- mod-button3, Set the window to floating mode and resize by dragging
    ( (modm, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )
  ]

-- Since gaps and spacing are enabled, this is a little different than the typical answer you  find online
doFullScreen :: X ()
doFullScreen = do
  sendMessage $ Toggle FULL
  doCollapse

doCollapse :: X()
doCollapse = do
  sendMessage ToggleStruts
  toggleScreenSpacingEnabled
  sendMessage ToggleGaps

myLayout =
  avoidStruts
    .   smartBorders -- imporvments
    .   spacingRaw True (Border 0 10 10 10) True (Border 5 5 5 5) True -- between windows
    .   gaps [(U, 10), (R, 10), (L, 10), (D, 10)] -- along the screen, excluding docks
    .   mkToggle (NOBORDERS ?? FULL ?? EOT) -- toggle full screen
    $   tiled
    ||| Mirror tiled
    ||| Full
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
  floatsClasses =
    ["MPlayer", "Gimp", "yakuake", "Plasma-desktop", "ksmserver", "r_x11"]
  floatsTitles = ["alsamixer"]

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
  W.Stack t [] (r : rs) -> W.Stack t [r] rs
  _                     -> c

spawnXMobar :: String -> IO Handle
spawnXMobar = spawnPipe . getXmobarCommand

getXmobarCommand location = prefix ++ location ++ ".hs"
 where
  prefix =
    "xmobar -x 0"
      ++ " -B "
      ++ wrap "\"" "\"" bgColor
      ++ " -F "
      ++ wrap "\"" "\"" fgColor
      ++ " $HOME/.xmonad/app/xmobar_"

greeter :: Logger
greeter = return $ Just  "Hey you, you're finally awake..."

logTitleOrGreet :: Logger
logTitleOrGreet = do
  maybeTitle <- logTitle
  case maybeTitle of
    Nothing -> greeter
    Just xs -> return . Just $ shorten 50 xs

bottomBarPP :: PP
bottomBarPP = def { ppOrder = \(_ : _ : _ : extras) -> extras, ppExtras = [logTitleOrGreet] }

windowedFullscreenFixEventHook :: Event -> X All
windowedFullscreenFixEventHook (ClientMessageEvent _ _ _ dpy win typ (_ : dats))
  = do
    wmstate    <- getAtom "_NET_WM_STATE"
    fullscreen <- getAtom "_NET_WM_STATE_FULLSCREEN"
    when (typ == wmstate && fromIntegral fullscreen `elem` dats) $ do
      withWindowAttributes dpy win $ \attrs -> liftIO $ resizeWindow
        dpy
        win
        (fromIntegral $ wa_width attrs - 1)
        (fromIntegral $ wa_height attrs)
      withWindowAttributes dpy win $ \attrs -> liftIO $ resizeWindow
        dpy
        win
        (fromIntegral $ wa_width attrs + 1)
        (fromIntegral $ wa_height attrs)
    return $ All True

windowedFullscreenFixEventHook _ = return $ All True

topBarPP :: PP
topBarPP = def { ppCurrent         = xmobarBorder "Bottom" fgColor 4
               , ppUrgent          = xmobarBorder "Bottom" "#CD3C66" 4
               , ppHiddenNoWindows = xmobarColor "#98a0b3" ""
               , ppSep             = "|"
               , ppOrder           = \(ws : l : _ : extras) -> ws : l : extras
               }


xmobarTop = statusBarConf (getXmobarCommand "top") topBarPP toggleStrutsKey
xmobarBottom =
  statusBarConf (getXmobarCommand "bottom") bottomBarPP toggleStrutsKey


privateConfig = combineStatusBars [xmobarTop, xmobarBottom] $ ewmh $ kde4Config
  { manageHook         = manageDocks <+> myManageHook <+> manageHook kde4Config
  , modMask            = myModMask
  , workspaces         = myWorkspaces
  , mouseBindings      = myMouseBindings
  , keys               = \c -> myKeys c `M.union` keys kde4Config c
  , layoutHook         = myLayout
  , handleEventHook    = handleEventHook kde4Config
                           <+> windowedFullscreenFixEventHook
  , focusedBorderColor = fgColor
  , normalBorderColor  = bgColor
  , terminal           = "termite"
  }


-------- Stuff for PRs

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

data StatusBarConf = StatusBarConf { logToDocks :: X ()
                                   , startDocks :: X ()
                                   , keyToggle :: XConfig Layout -> (KeyMask, KeySym)
                                   , toggleAction :: X ()}

instance Default StatusBarConf where
  def = StatusBarConf { logToDocks = def
                      , startDocks = def
                      , keyToggle = toggleStrutsKey
                      , toggleAction = sendMessage ToggleStruts
                      }

combineConfsWithAction
  :: (XConfig Layout -> (KeyMask, KeySym))
  -> X ()
  -> [StatusBarConf]
  -> StatusBarConf
combineConfsWithAction k action confs = StatusBarConf
  { logToDocks   = mapM_ logToDocks confs
  , startDocks   = mapM_ startDocks confs
  , keyToggle    = k
  , toggleAction = action
  }
combineConfs
  :: [StatusBarConf]
  -> StatusBarConf
combineConfs = combineConfsWithAction toggleStrutsKey (sendMessage ToggleStruts)


combineStatusBars :: LayoutClass l Window
                  => [IO StatusBarConf]
                  -> XConfig l
                  -> IO (XConfig (ModifiedLayout AvoidStruts l))

combineStatusBars statusBarConfs conf =
  sequence statusBarConfs >>= flip makeStatusBar' conf . combineConfs

makeStatusBar' :: LayoutClass l Window
               => StatusBarConf
               -> XConfig l
               -> IO (XConfig (ModifiedLayout AvoidStruts l))
makeStatusBar' sbconf conf = pure $ docks $ conf
  { layoutHook = avoidStruts (layoutHook conf)
  , logHook = logHook conf *> logToDocks sbconf
  , keys = (<>) <$> keys' <*> keys conf
  , startupHook = startupHook conf *> startDocks sbconf
  }
  where
    keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
    keys' = (`M.singleton` toggleAction sbconf) . keyToggle sbconf

statusBarConf
  :: String    -- ^ The command line to launch the status bar
  -> PP        -- ^ The pretty printing options
  -> (XConfig Layout -> (KeyMask, KeySym))
                       -- ^ The desired key binding to toggle bar visibility
  -> IO StatusBarConf
statusBarConf cmd pp k = do
  h <- spawnPipe cmd
  return $ def { logToDocks = dynamicLogWithPP (pp { ppOutput = hPutStrLn h })
               , keyToggle  = k
               }

statusBar :: LayoutClass l Window
          => String    -- ^ The command line to launch the status bar
          -> PP        -- ^ The pretty printing options
          -> (XConfig Layout -> (KeyMask, KeySym))
                       -- ^ The desired key binding to toggle bar visibility
          -> XConfig l -- ^ The base config
          -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBar cmd pp k conf = statusBarConf cmd pp k >>= flip makeStatusBar' conf
