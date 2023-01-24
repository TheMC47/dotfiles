{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import           Data.Bifunctor
import qualified Data.Map                      as M
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaceGroups
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.TopicSpace
                                         hiding ( TI
                                                , TopicItem
                                                , topicNames
                                                )
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Modal
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.WindowSwallowing
import           XMonad.Layout.FixedAspectRatio
import           XMonad.Layout.Gaps
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Prelude
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.OrgMode
import           XMonad.Prompt.Workspace
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet               as W
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.EZConfig
import           XMonad.Util.Hacks
import           XMonad.Util.Loggers
import           XMonad.Util.NamedScratchpad
                                         hiding ( name )

myBgColor = "#2E3440"
myFgColor = "#D8DEE9"

main :: IO ()
main =
  xmonad
    .                 setEwmhActivateHook doAskUrgent
    .                 docks
    .                 dynamicSBs barSpawner
    .                 javaHack
    .                 withUrgencyHook NoUrgencyHook
    .                 ewmh
    $                 def
                        { manageHook = myManageHook <> namedScratchpadManageHook scratchpads
                        , modMask            = myModMask
                        , workspaces         = topicNames topics
                        , layoutHook         = myLayout
                        , handleEventHook    = windowedFullscreenFixEventHook
                                               <> swallowEventHook (className =? "Alacritty") (not <$> checkDock)
                                               <> trayerPaddingXmobarEventHook
                                               <> trayerAboveXmobarEventHook
                        , logHook            = updatePointer (0.5, 0.5) (0, 0)
                        , focusedBorderColor = colorText
                        , normalBorderColor  = colorBase
                        , terminal           = myTerminal
                        , startupHook = spawn "pkill xembedsniproxy" >> addTopicGroups topics
                        }
    `additionalKeysP` ([ ("M-<Return>", terminalHere)
                       , ( "M-S-<Return>"
                         , namedScratchpadAction scratchpads "dropdown-term"
                         )
                       , ("M-d"   , rofi)
                       , ("M-b"   , toggleCollapse)
                       , ("M-f"   , toggleFullScreen)
                       , ("M1-C-q", kill)
                       , ("M-S-x" , logout)
                       , ("M-C-q" , xmonadRecompile)
                       ,
                            -- Prompts
                         ("M-x"   , xmonadPrompt myXPConfig)
                       , ( "M-n"
                         , workspacePrompt myXPConfig (switchTopic topicConfig)
                         )
                       , ( "M-g"
                         , promptTopicGroupView topicConfig
                                                myXPConfig
                                                "Go to group: "
                         )
                       , ("M-i"  , namedScratchpadAction scratchpads "irc")
                       , ("M-s"  , namedScratchpadAction scratchpads "signal")
                       , ("M-w"  , moveTo Next inUse)
                       , ("M-q"  , moveTo Prev inUse)
                       , ("M-S-w", shiftTo Next inUse)
                       , ("M-S-q", shiftTo Prev inUse)
                       , ("M-S-n", setMode noModModeLabel)
                       , ("M-S-r", setMode overlayedFloatModeLabel)
                       , ("M-S-b", spawn "brave")
                       , ("M-S-l", sendMessage MirrorExpand)
                       , ("M-S-h", sendMessage MirrorShrink)
                       ]
                      ++ screenKeys
                      ++ workspaceKeys
                      ++ emacsKeys
                      ++ ratioKeys
                      ++ dynamicScratchpads
                      )
 where
  workspaceNumbers = [1 :: Int .. 9] <> [0]
  workspaceKeys =
    [ ("M-" <> m <> show k, withNthWorkspace f i)
    | (k, i) <- zip workspaceNumbers [0 ..]
    , (m, f) <- [("", W.view), ("C-", W.greedyView), ("S-", W.shift)]
    ]
  dynamicScratchpads =
    [ ("M1-" <> m <> show k, f ("dyn" <> show k))
    | k      <- [0 :: Int .. 9]
    , (m, f) <- [("C-", withFocused . toggleDynamicNSP), ("", dynamicNSPAction)]
    ]
  screenKeys =
    [ ("M-" <> m <> [key], screenWorkspace sc >>= (`whenJust` windows . f))
      | (key, sc) <- zip "po" [0 ..]
      , (f  , m ) <- [(W.view, ""), (W.shift, "S-")]
      ]
      ++ [ ( "M-C-" <> [key]
           , screenWorkspace sc >>= (`whenJust` windows . W.greedyView)
           )
         | (key, sc) <- zip "op" [0 ..]
         ]
  emacsKeys = makeSubmap
    "e"
    (spawn emacs)
    [ ("t"  , namedScratchpadAction scratchpads "todos")
    , ("o"  , orgPrompt myXPConfig "TODO" "org/todos.org")
    , ("S-o", orgPromptPrimary myXPConfig "TODO" "org/todos.org")
    , ("r"  , spawn "systemctl restart --user emacs")
    ]
  mkRatioAction = (>> refresh) . withFocused . (broadcastMessage .)
  ratioKeys     = makeSubmap "a" (mkRatioAction $ ToggleRatio (16 / 9)) $ map
    (second mkRatioAction)
    [ ("r", ResetRatio)
    , ("6", FixRatio (16 / 9))
    , ("4", FixRatio (4 / 3))
    , ("c", FixRatio (6 / 7))
    ]
  inUse = hiddenWS :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag]

-- | A small helper function to make submaps. For the given key k,
-- it binds @M-k@ to the given action, and prefixes the keys in the
-- list with @M-C-k@
makeSubmap :: String -> X () -> [(String, X ())] -> [(String, X ())]
makeSubmap k action ks =
  ("M-" <> k, action) : map (first (("M-S-" <> k <> " ") <>)) ks

myModMask :: KeyMask
myModMask = mod4Mask

textIcon = xmobarFont 1
workspaceIcon = xmobarFont 2

myWorkspaces :: [String]
myWorkspaces = zipWith -- euum. yeah. I know. overengineered
  (<>)
  (map ((<> "") . show) [(1 :: Int) .. 10])
  $ map workspaceIcon
  [ book
  ,
      -- 
    "\xf120"
  ,
      -- 
    "\xf002"
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

book :: String
book = "\xf02d"

{--
=== Topics ===
A small DSL for topics. This was inspired by byorgey's TopicItem, and wa
expanded to work with DynamicWorkspaceGroups

--}

-- | Convenience for standard topics.
data TopicItem
  = TI
      { -- | 'Topic' = 'String'
        name :: Topic,
        -- | Directory associated with topic; 'Dir' = 'String'
        dir :: Dir,
        -- | Startup hook when topic is empty
        action :: X ()
      }
  | TG
      { name :: Topic,
        dir :: Dir,
        actions :: [(ScreenId, X ())]
      }

myDirs :: [TopicItem] -> M.Map Topic Dir
myDirs = M.fromList . concatMap go
 where
  go TI {..} = [(name, dir)]
  go TG {..} = map (bimap (`getTopicName` name) (const dir)) actions

myActions :: [TopicItem] -> M.Map Topic (X ())
myActions = M.fromList . concatMap go
 where
  go TI {..} = [(name, action)]
  go TG {..} = map (first (`getTopicName` name)) actions

topicNames :: [TopicItem] -> [String]
topicNames = concatMap go
 where
  go TI {..} = [name]
  go TG {..} = map ((`getTopicName` name) . fst) actions

getTopicName :: ScreenId -> Topic -> Topic
getTopicName 0     t = t
getTopicName (S n) t = t <> " - " <> show (n + 1)

genTopicConfig :: [TopicItem] -> TopicConfig
genTopicConfig ts = def { topicDirs          = myDirs ts
                        , topicActions       = myActions ts
                        , defaultTopicAction = const mempty
                        , defaultTopic       = name $ head ts
                        }

topics :: [TopicItem]
topics = map only myWorkspaces
 where
  noAction :: Topic -> Dir -> TopicItem
  noAction n d = TI n d mempty
  only :: Topic -> TopicItem
  only n = noAction n "./"

addTopicGroup :: TopicItem -> X ()
addTopicGroup TI{}    = mempty
addTopicGroup TG {..} = addRawWSGroup name . reverse $ map
  (\(sid, _) -> (sid, getTopicName sid name))
  actions

addTopicGroups :: [TopicItem] -> X ()
addTopicGroups = mapM_ addTopicGroup

topicConfig :: TopicConfig
topicConfig = genTopicConfig topics

--------
--- Scratchpads
--------

scratchpads =
  [ NS "todos"
       todoCommand
       (title =? todoTitle)
       (customFloating $ W.RationalRect (1 / 6) 0 (2 / 3) (1 / 3))
  , NS "dropdown-term"
       (inTerminal . withTitleT $ dropDownTitle)
       (title =? dropDownTitle)
       (customFloating $ W.RationalRect (1 / 6) 0 (2 / 3) (2 / 3))
  , NS "irc"
       "hexchat"
       (className =? "Hexchat")
       (customFloating $ W.RationalRect (1 / 8) (3 / 4) (3 / 4) (1 / 4))
  , NS "signal"
       "signal-desktop"
       (className =? "Signal")
       (customFloating $ W.RationalRect (2 / 3) (1 / 8) (1 / 3) (3 / 4))
  ]
 where
  todoTitle     = "TODOs"
  todoCommand   = inEmacs . eWithTitle todoTitle $ "org/todos.org"
  dropDownTitle = "Dropdown"

--------
--- Emacs
--------

myTerminal :: String
myTerminal = "alacritty "

inTerminal = (myTerminal <>)

withTitleT :: ShowS
withTitleT c = "--title " <> c <> " "

inDirT :: ShowS
inDirT dir = "--working-directory \"" <> dir <> "\""

terminalHere :: X ()
terminalHere = spawnHere . inTerminal . inDirT =<< currentTopicDir topicConfig

emacs :: String
emacs = "emacsclient -a '' -create-frame --no-wait "

eWithTitle :: String -> ShowS
eWithTitle t = (("-F '(quote (name . \"" <> t <> "\"))' ") <>)

inEmacs :: ShowS
inEmacs = (emacs <>)

openP :: ShowS
openP dir = "--eval '(open-magit-or-dired " <> dir <> ")'"

openDir :: X ()
openDir = spawnHere . inEmacs . openP . quote =<< currentTopicDir topicConfig

magit :: ShowS
magit dir = "--eval '(magit-status \"" <> dir <> "\")' "

magitHere :: X ()
magitHere = spawnHere . inEmacs . magit =<< currentTopicDir topicConfig

fileInDir :: String -> ShowS
fileInDir dir = ((dir <> "/") <>)

openFileInDir :: String -> X ()
openFileInDir file =
  spawnHere
    .   inEmacs
    .   quote
    .   (`fileInDir` file)
    =<< currentTopicDir topicConfig

browse :: String -> X ()
browse = spawnHere . ("brave --new-window " <>)

quote :: ShowS
quote s = "\"" <> s <> "\""

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
myManageHook =
  manageSpawn
    <> (not <$> willFloat --> insertPosition Below Newer)
    <> mconcat manageHooks
    <> (isDialog --> doFloat)
 where
  manageHooks = generalRules ++ concat windowRules
  generalRules =
    [ className =? "discord" --> doShift (workspaceAt 8)
    , title =? "Netflix" <||> className =? "vlc" --> doFixAspect (16 / 9)
    ]
  windowRules =
    [ [ className =? c --> doFloat | c <- floatsClasses ]
    , [ title =? t --> doFloat | t <- floatsTitles ]
    ]
  floatsClasses =
    [ "MPlayer"
    , "Gimp"
    , "yakuake"
    , "Plasma-desktop"
    , "ksmserver-logout-greeter"
    , "R_x11"
    ]
  floatsTitles = ["alsamixer"]

---------------------
-- Status Bars
---------------------

onLayout :: String -> ScreenId -> X Bool
onLayout l sid = fromMaybe False <$> (== l) <$$> logLayoutOnScreen sid

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

logF :: Logger
logF = ifM (onLayout "F" 0) nwindows (return Nothing)
 where
  nwindows = flip withScreen 0 $ \ws -> do
    let n = length . W.integrate' . W.stack . W.workspace $ ws
    if n > 1 then return . Just . show $ n else return Nothing

-- TODO export?
withScreen :: (WindowScreen -> Logger) -> ScreenId -> Logger
withScreen f n = do
  ss <- withWindowSet $ return . W.screens
  case find ((== n) . W.screen) ss of
    Just s  -> f s
    Nothing -> pure Nothing

topPP :: X PP
topPP = clickablePP . filterOutWsPP [scratchpadWorkspaceTag] $ def
  { ppCurrent = teal . xmobarBorder "Bottom" colorTeal 4
  , ppUrgent  = red
  , ppVisible = yellow . xmobarBorder "Bottom" colorYellow 1
  , ppSep     = " | "
  , ppExtras  = [ logLayoutOnScreen 0
                , shortenL 50 (logTitleOnScreen 0)
                , logF
                , logMode
                ]
  , ppOrder   = \(ws : _ : _ : extras) -> ws : extras
  }

secondaryPP :: ScreenId -> X PP
secondaryPP s = pure $ def
  { ppOrder  = \(_ : _ : _ : extras) -> extras
  , ppSep     = " | "
  , ppExtras = [ logCurrentOnScreen s
               , logLayoutOnScreen s
               , shortenL 50 $ logTitleOnScreen s
               , logWhenActive s (logConst "*")
               ]
  }

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0       = pure $ statusBarProp "xmobar top" topPP <> traySB

barSpawner s@(S n) = pure $ statusBarPropTo
  ("_XMONAD_LOG__Secondary_" <> show n)
  ("xmobar secondary " <> show n)
  (secondaryPP s)

traySB :: StatusBarConfig
traySB =
  statusBarGeneric
    ( unwords
        [ "trayer",
          "--edge top",
          "--align right",
          "--widthtype request",
          "--expand true",
          "--monitor primary",
          "-l",
          "--tint 0x2E3440",
          "--height 30",
          "--distancefrom top,right",
          "--distance 15,26"
        ]
    )
    mempty

---------------------
-- Prompt
---------------------
myXPConfig :: XPConfig
myXPConfig = def
  { font            =
    "xft:Source Code Pro:size=11:regular:antialias=true,FontAwesome:pixelsize=13"
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
