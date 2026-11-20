{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.Aeson
import Data.Bifunctor
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Map as M
import GHC.Generics
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.OnScreen
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace hiding (
    TI,
    TopicItem,
    noAction,
    topicNames,
 )
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Modal
import XMonad.Hooks.Rescreen
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WindowSwallowing
import XMonad.Layout.FixedAspectRatio
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Prelude
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.OrgMode
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as S
import qualified XMonad.StackSet as W
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Hacks
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad hiding (
    name,
 )
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.StickyWindows
import XMonad.Util.WorkspaceCompare (getSortByIndex)

myBgColor = "#2E3440"

myFgColor = "#D8DEE9"

main :: IO ()
main =
    xmonad
        . sticky
        . setEwmhActivateHook doAskUrgent
        . docks
        . dynamicSBs barSpawner
        . javaHack
        . setRescreenDelay 500000 -- 500ms
        -- . addRandrChangeHook (spawn "autorandr --change")
        . addAfterRescreenHook
            ( spawn "feh --bg-fill Wallpapers/mountain.png"
            )
        . withUrgencyHook NoUrgencyHook
        . modal [overlayedFloatMode 10]
        . enableEwmhManageAboveBelowState
        . ewmh
        $ def
            { manageHook = myManageHook <> namedScratchpadManageHook scratchpads
            , modMask = myModMask
            , workspaces = topicNames topics
            , layoutHook = myLayout
            , handleEventHook =
                trayerPaddingXmobarEventHook
                    <> windowedFullscreenFixEventHook
                    <> swallowEventHook
                        (className =? "com.mitchellh.ghostty")
                        (not <$> checkDock)
                    <> trayerAboveXmobarEventHook
                    <> trayAbovePanelEventHook (className =? "trayer") (className =? "eww")
            , logHook = updatePointer (0.5, 0.5) (0, 0) >> dumpState
            , focusedBorderColor = colorText
            , normalBorderColor = colorBase
            , terminal = myTerminal
            , startupHook = spawn "pkill xembedsniproxy" >> addTopicGroups topics
            }
            `additionalKeysP` ( [ ("M-<Return>", terminalHere)
                                ,
                                    ( "M-S-<Return>"
                                    , namedScratchpadAction scratchpads "dropdown-term"
                                    )
                                , ("M-d", rofi)
                                , ("M-b", toggleCollapse)
                                , ("M-f", toggleFullScreen)
                                , ("M-c", kill)
                                , ("M-S-x", logout)
                                , ("M-C-q", xmonadRecompile)
                                , -- Prompts
                                  ("M-x", xmonadPrompt myXPConfig)
                                ,
                                    ( "M-n"
                                    , workspacePrompt myXPConfig (switchTopic topicConfig)
                                    )
                                ,
                                    ( "M-g"
                                    , promptTopicGroupView
                                        topicConfig
                                        myXPConfig
                                        "Go to group: "
                                    )
                                , ("M-i", namedScratchpadAction scratchpads "irc")
                                , ("M-s", namedScratchpadAction scratchpads "signal")
                                , ("M-w", moveTo Next inUse)
                                , ("M-q", moveTo Prev inUse)
                                , ("M-S-w", shiftTo Next inUse)
                                , ("M-S-q", shiftTo Prev inUse)
                                , ("M-S-n", setMode noModModeLabel)
                                , ("M-S-r", setMode overlayedFloatModeLabel)
                                , ("M-S-b", spawn "brave")
                                , ("M-S-l", sendMessage MirrorExpand)
                                , ("M-S-h", sendMessage MirrorShrink)
                                , ("M-z", toggleStatusBar)
                                , ("M-S-s", spawn "slock")
                                , ("M-S-u", withFocused stick)
                                , ("M-S-i", withFocused unstick)
                                , ("<XF86MonBrightnessUp>", spawn "light -A 10")
                                , ("<XF86MonBrightnessDown>", spawn "light -U 10")
                                , ("M-v", windows (onScreen (W.view "1") FocusCurrent 1))
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
        | k <- [0 :: Int .. 9]
        , (m, f) <- [("C-", withFocused . toggleDynamicNSPAndFloat), ("", dynamicNSPAction)]
        ]
    toggleDynamicNSPAndFloat s w = toggleDynamicNSP s w >> float w -- kinda works
    screenKeys =
        [ ("M-" <> m <> [key], screenWorkspace sc >>= (`whenJust` windows . f))
        | (key, sc) <- zip "po" [0 ..]
        , (f, m) <- [(W.view, ""), (W.shift, "S-")]
        ]
            ++ [ ( "M-C-" <> [key]
                 , screenWorkspace sc >>= (`whenJust` windows . W.greedyView)
                 )
               | (key, sc) <- zip "op" [0 ..]
               ]
    emacsKeys =
        makeSubmap
            "e"
            (spawn emacs)
            [ ("t", namedScratchpadAction scratchpads "todos")
            , ("o", orgPrompt myXPConfig "TODO" "org/todos.org")
            , ("S-o", orgPromptPrimary myXPConfig "TODO" "org/todos.org")
            , ("r", spawn "systemctl restart --user emacs")
            ]
    mkRatioAction = (>> refresh) . withFocused . (broadcastMessage .)
    ratioKeys =
        makeSubmap "a" (mkRatioAction $ ToggleRatio (16 / 9)) $
            map
                (second mkRatioAction)
                [ ("r", ResetRatio)
                , ("6", FixRatio (16 / 9))
                , ("4", FixRatio (4 / 3))
                , ("c", FixRatio (6 / 7))
                ]
    inUse = hiddenWS :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag]

-- | A small helper function to make submaps. For the given key k,
--  it binds @M-k@ to the given action, and prefixes the keys in the
--  list with @M-C-k@
makeSubmap :: String -> X () -> [(String, X ())] -> [(String, X ())]
makeSubmap k action ks =
    ("M-" <> k, action) : map (first (("M-S-" <> k <> " ") <>)) ks

myModMask :: KeyMask
myModMask = mod4Mask

textIcon = xmobarFont 1

workspaceIcon = xmobarFont 2

myWorkspaces :: [String]
myWorkspaces = map show [(1 :: Int) .. 10]

{--
=== Topics ===
A small DSL for topics. This was inspired by byorgey's TopicItem, and wa
expanded to work with DynamicWorkspaceGroups

--}

-- | Convenience for standard topics.
data TopicItem
    = TI
        { name :: Topic
        -- ^ 'Topic' = 'String'
        , dir :: Dir
        -- ^ Directory associated with topic; 'Dir' = 'String'
        , action :: X ()
        -- ^ Startup hook when topic is empty
        }
    | TG
        { name :: Topic
        , dir :: Dir
        , actions :: [(ScreenId, X ())]
        }

myDirs :: [TopicItem] -> M.Map Topic Dir
myDirs = M.fromList . concatMap go
  where
    go TI{..} = [(name, dir)]
    go TG{..} = map (bimap (`getTopicName` name) (const dir)) actions

myActions :: [TopicItem] -> M.Map Topic (X ())
myActions = M.fromList . concatMap go
  where
    go TI{..} = [(name, action)]
    go TG{..} = map (first (`getTopicName` name)) actions

topicNames :: [TopicItem] -> [String]
topicNames = concatMap go
  where
    go TI{..} = [name]
    go TG{..} = map ((`getTopicName` name) . fst) actions

getTopicName :: ScreenId -> Topic -> Topic
getTopicName 0 t = t
getTopicName (S n) t = t <> " - " <> show (n + 1)

genTopicConfig :: [TopicItem] -> TopicConfig
genTopicConfig ts =
    def
        { topicDirs = myDirs ts
        , topicActions = myActions ts
        , defaultTopicAction = const mempty
        , defaultTopic = name $ head ts
        }

topics :: [TopicItem]
topics =
    map only myWorkspaces
        <> [ TG
                "predium"
                "predium"
                [ (0, magitHere)
                , (1, terminalHere)
                ]
           ]
  where
    noAction :: Topic -> Dir -> TopicItem
    noAction n d = TI n d mempty
    only :: Topic -> TopicItem
    only n = noAction n "./"

addTopicGroup :: TopicItem -> X ()
addTopicGroup TI{} = mempty
addTopicGroup TG{..} =
    addRawWSGroup name . reverse $
        map
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
    [ NS
        "todos"
        todoCommand
        (title =? todoTitle)
        (customFloating $ W.RationalRect (1 / 6) 0 (2 / 3) (1 / 3))
    , NS
        "dropdown-term"
        (inTerminal . withTitleT $ dropDownTitle)
        (title =? dropDownTitle)
        (customFloating $ W.RationalRect (1 / 6) 0 (2 / 3) (2 / 3))
    , NS
        "irc"
        "hexchat"
        (className =? "Hexchat")
        (customFloating $ W.RationalRect (1 / 8) (3 / 4) (3 / 4) (1 / 4))
    , NS
        "signal"
        "signal-desktop"
        (className =? "Signal")
        (customFloating $ W.RationalRect (2 / 3) (1 / 8) (1 / 3) (3 / 4))
    ]
  where
    todoTitle = "TODOs"
    todoCommand = inEmacs . eWithTitle todoTitle $ "org/todos.org"
    dropDownTitle = "Dropdown"

--------
--- Emacs
--------

-- Open windows in the long-running Ghostty instance (systemd user service)
-- instead of cold-starting a new process each time.
myTerminal :: String
myTerminal = "ghostty +new-window "

inTerminal = (myTerminal <>)

withTitleT :: ShowS
withTitleT c = "--title=" <> c <> " "

inDirT :: ShowS
inDirT dir = "--working-directory=\"" <> dir <> "\""

terminalIn :: String -> X ()
terminalIn = spawnHere . inTerminal . inDirT

terminalFromProjectDir :: String -> X ()
terminalFromProjectDir loc =
    terminalIn . (\d -> d <> "/" <> loc) =<< currentTopicDir topicConfig

terminalHere :: X ()
terminalHere = terminalIn =<< currentTopicDir topicConfig

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
        . inEmacs
        . quote
        . (`fileInDir` file)
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
        . avoidStruts
        . fixedAspectRatio (0.5, 0.5)
        . layoutHintsWithPlacement (0.5, 0.5)
        . spacingRaw False (Border 0 0 0 0) True (Border 10 10 10 10) True -- between windows
        . gaps [(U, 15), (R, 15), (L, 15), (D, 15)] -- along the screen, excluding docks
        . mkToggle (single NBFULL) -- toggle full screen
        . smartBorders
        $ tiled
            ||| mtiled
            ||| full
  where
    tiled = renamed [Replace "T "] $ ResizableTall 1 (3 / 100) (5 / 8) []
    mtiled = renamed [Replace "MT"] $ Mirror tiled
    full = renamed [Replace "F "] Full

---------------------
-- ManageHook
---------------------
myManageHook :: ManageHook
myManageHook =
    manageSpawn
        <> (not <$> willFloat --> insertPosition Below Newer)
        <> (isDialog --> doFloat)
        <> (title =? "float-emacs" --> doCenterFloat <> doFocus)
        <> (className =? "TestClass" --> doCenterFloat <> doShift (workspaceAt 2))
        <> (stringProperty "WM_WINDOW_ROLE" =? "browser" --> ewmhDesktopsManageHook)
        <> (className =? "discord" --> doShift (workspaceAt 8))
        <> (title =? "Netflix" <||> className =? "vlc" --> doFixAspect (16 / 9))
        <> (title =? "alsamixer" --> doFloat)
        -- <> (title ^? "Mercury" --> doCenterFloat)
        <> mconcat [className =? c --> doFloat | c <- floatsClasses]
  where
    floatsClasses =
        [ "MPlayer"
        , "Gimp"
        , "yakuake"
        , "Plasma-desktop"
        , "ksmserver-logout-greeter"
        , "R_x11"
        ]

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
        Just s -> f s
        Nothing -> pure Nothing

data WorkspaceDump = WorkspaceDump
    { tag :: !String
    , isCurrent :: !Bool
    , isVisible :: !Bool
    , isUrgent :: !Bool
    , hasWindows :: !Bool
    , layout :: !String
    , currentWindow :: !(Maybe String)
    , index :: !(Maybe Int)
    , screenId :: !(Maybe ScreenId)
    }
    deriving (Generic, Show)

data ScreenDump = ScreenDump
    { screen :: !ScreenId
    , currentDesktop :: !WorkspaceDump
    }
    deriving (Generic, Show)

data StateDump = StateDump
    { _workspaces :: ![WorkspaceDump]
    , screens :: ![ScreenDump]
    }
    deriving (Generic, Show)

instance ToJSON ScreenId where
    toJSON (S n) = toJSON n
instance ToJSON WorkspaceDump
instance ToJSON ScreenDump
instance ToJSON StateDump where
    toJSON StateDump{..} =
        object
            [ "workspaces" .= _workspaces
            , "screens" .= screens
            ]

dumpWindowSpace :: [Window] -> WindowSet -> Maybe Int -> WindowSpace -> X WorkspaceDump
dumpWindowSpace urgents windowSet idx ws =
    do
        let stack = S.stack ws
            windowName = (fmap show . getName) . S.focus <$> stack
        currentWindow <- sequence windowName
        return
            WorkspaceDump
                { tag = S.tag ws
                , isCurrent = S.tag ws == S.currentTag windowSet
                , isVisible = S.tag ws `elem` map (S.tag . S.workspace) (S.visible windowSet)
                , isUrgent = any (\x -> (== Just (S.tag ws)) (S.findTag x windowSet)) urgents
                , hasWindows = isJust stack
                , layout = description . S.layout $ ws
                , currentWindow = currentWindow
                , index = idx
                , screenId = fmap S.screen . find ((== S.tag ws) . S.tag . S.workspace) . S.screens $ windowSet
                }

dumpScreen :: [Window] -> WindowSet -> WindowScreen -> X ScreenDump
dumpScreen urgents windowSet screen =
    do
        windowSpaceDump <- dumpWindowSpace urgents windowSet Nothing (W.workspace screen)
        return
            ScreenDump
                { screen = W.screen screen
                , currentDesktop = windowSpaceDump
                }

getStateDump :: X StateDump
getStateDump = do
    winset <- gets windowset
    wSort <- getSortByIndex
    urgents <- readUrgents
    wsDump <- zipWithM (dumpWindowSpace urgents winset) (map Just [0 ..]) . wSort . filter ((/= scratchpadWorkspaceTag) . S.tag) . S.workspaces $ winset
    ssDump <- traverse (dumpScreen urgents winset) . W.screens $ winset
    return
        StateDump
            { _workspaces = wsDump
            , screens = ssDump
            }

dumpState :: X ()
dumpState = getStateDump <&> toString . encode >>= xmonadPropLog' "_XMONAD_DUMP"

renameAddIcon :: String -> WindowSpace -> String
renameAddIcon = const . workspaceWithIcon

workspaceWithIcon :: String -> String
workspaceWithIcon = lookupIcon
  where
    wsPairs =
        [ ("1", "\xf02d")
        , ("2", "\xf120")
        , ("3", "\xf002")
        , ("4", "\xf108")
        , ("5", "\xf108")
        , ("6", "\xf108")
        , ("7", "\xf108")
        , ("8", "\xf095")
        , ("9", "\xf2dc")
        , ("10", "\xf236")
        ]
    lookupIcon w = maybe w ((w <>) . workspaceIcon) (lookup w wsPairs)

topPP :: X PP
topPP =
    clickablePP . filterOutWsPP [scratchpadWorkspaceTag] $
        def
            { ppCurrent = teal . xmobarBorder "Bottom" colorTeal 4
            , ppUrgent = red
            , ppVisible = yellow . xmobarBorder "Bottom" colorYellow 1
            , ppSep = " | "
            , ppExtras =
                [ logLayoutOnScreen 0
                , shortenL 50 (logTitleOnScreen 0)
                , logF
                , logMode
                ]
            , ppOrder = \(ws : _ : _ : extras) -> ws : extras
            , ppRename = renameAddIcon
            }

secondaryPP :: ScreenId -> X PP
secondaryPP s =
    pure $
        def
            { ppOrder = \(_ : _ : _ : extras) -> extras
            , ppSep = " | "
            , ppExtras =
                [ workspaceWithIcon <$$> logCurrentOnScreen s
                , logLayoutOnScreen s
                , shortenL 50 $ logTitleOnScreen s
                , logWhenActive s (logConst "*")
                ]
            }

data EwwWindow = EwwWindow
    { windowName :: !String
    , windowArgs :: ![(String, String)]
    , windowId :: !String
    }
    deriving (Show, Generic)

ewwStatusBar :: [EwwWindow] -> StatusBarConfig
ewwStatusBar ewws =
    def
        { sbStartupHook =
            spawn $
                unwords
                    ( [ "eww"
                      , "open-many"
                      , windowNames
                      ]
                        ++ args
                    )
        , sbCleanupHook = spawn $ "eww close " <> unwords (map windowId ewws)
        }
  where
    windowNames = unwords $ map (\EwwWindow{..} -> windowName <> ":" <> windowId) ewws
    args =
        concatMap
            ( \EwwWindow{..} ->
                concatMap
                    ( \(k, v) -> ["--arg", windowId <> ":" <> k <> "=" <> v]
                    )
                    windowArgs
            )
            ewws

ewwWorkspacesPrimary :: EwwWindow
ewwWorkspacesPrimary =
    EwwWindow
        { windowName = "workspacesWin"
        , windowArgs = [("screenId", "0"), ("verbose", "true")]
        , windowId = "workspaces0"
        }

barSpawner :: ScreenId -> X StatusBarConfig
barSpawner 0 = pure $ statusBarProp (withLog "echo \"starting xmobar\"; xmobar top") topPP <> traySB -- <> ewwStatusBar [ewwWorkspacesPrimary]
barSpawner s@(S n) =
    pure $
        statusBarPropTo
            ("_XMONAD_LOG__Secondary_" <> show n)
            (withLog ("xmobar secondary " <> show n))
            (secondaryPP s)

traySB :: StatusBarConfig
traySB =
    statusBarGeneric
        ( unwords
            [ "trayer"
            , "--edge top"
            , "--align right"
            , "--widthtype request"
            , "--expand true"
            , "--monitor primary"
            , "-l"
            , "--tint 0x2E3440"
            , "--height 30"
            , "--distancefrom top,right"
            , "--distance 15,26"
            ]
        )
        mempty

withLog :: String -> String
withLog = ("systemd-cat --priority=info --stderr-priority=warning --level-prefix=false -- " <>)

---------------------
-- Prompt
---------------------
myXPConfig :: XPConfig
myXPConfig =
    def
        { font =
            "xft:Source Code Pro:size=11:regular:antialias=true,FontAwesome:pixelsize=13"
        , position = CenteredAt (1 / 4) (2 / 3)
        , bgColor = myBgColor
        , fgColor = myFgColor
        , bgHLight = myFgColor
        , fgHLight = myBgColor
        , borderColor = myFgColor
        , searchPredicate = fuzzyMatch
        , sorter = fuzzySort
        , height = 30
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

data StatusBarState = Enabled | Disabled
    deriving (Read, Show)

instance ExtensionClass StatusBarState where
    initialValue = Enabled
    extensionType = PersistentExtension

toggleStatusBar :: X ()
toggleStatusBar =
    XS.get >>= \case
        Enabled -> do
            killAllStatusBars
            XS.put Disabled
        Disabled -> do
            startAllStatusBars
            XS.put Enabled
