{-# LANGUAGE BlockArguments, RecordWildCards #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
import           Data.Bifunctor
import qualified Data.Map                      as M
import           XMonad
import           XMonad.Actions.CopyWindow
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
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.WindowSwallowing
import           XMonad.Layout.FixedAspectRatio
import           XMonad.Layout.Gaps
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimplestFloat
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
                                               <> swallowEventHook (className =? "Alacritty")
                                                                   (return True)
                        , logHook            = updatePointer (0.5, 0.5) (0, 0)
                        , focusedBorderColor = myFgColor
                        , normalBorderColor  = myBgColor
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
                          -- Prompts
                       , ("M-x"   , xmonadPrompt myXPConfig)
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
                       , ("M-c"  , windows copyToAll)
                       , ("M-S-c", killAllOtherCopies)
                       ]
                      ++ screenKeys
                      ++ workspaceKeys
                      ++ emacsKeys
                      ++ ratioKeys
                      )
 where
  workspaceNumbers = [1 :: Int .. 9] <> [0]
  workspaceKeys =
    [ ("M-" <> m <> show k, withNthWorkspace f i)
    | (k, i) <- zip workspaceNumbers [0 ..]
    , (m, f) <- [("", W.view), ("C-", W.greedyView), ("S-", W.shift)]
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


myWorkspaces :: [String]
myWorkspaces = zipWith -- euum. yeah. I know. overengineered
  (<>)
  (map ((<> ":") . show) [(1 :: Int) .. 10])
  [ book
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

book :: String
book = "\xf02d"

{--
=== Topics ===
A small DSL for topics. This was inspired by byorgey's TopicItem, and wa
expanded to work with DynamicWorkspaceGroups

--}

-- | Convenience for standard topics.
data TopicItem = TI
  { name   :: Topic  -- ^ 'Topic' = 'String'
  , dir    :: Dir    -- ^ Directory associated with topic; 'Dir' = 'String'
  , action :: X ()   -- ^ Startup hook when topic is empty
  } |
  TG { name :: Topic
     , dir :: Dir
     , actions :: [(ScreenId, X ())]}

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
topics =
  map only myWorkspaces
    ++ [ TG
         "I2DL"
         "Studium/6.Semester/I2DL"
         [(0, spawnHere emacs), (1, browse "https://niessner.github.io/I2DL/")]
       , TI "Compilerbau I"
            "Studium/6.Semester/Compilerbau I"
            (openFileInDir "slides.pdf")
       , TG -- Thesis
         "Thesis"
         "Studium/6.Semester/BA/bachelor-thesis-isabelle-linter"
         [ (0, magitHere >> terminalHere)
         , ( 1
           , browse
               "https://github.com/TheMC47/bachelor-thesis-isabelle-linter/projects/1"
             >> terminalHere
           )
         ]
       , TG -- ElBaldiya
         "ElBaladiya"
         "elBaladiya.tn/smartup-backend"
         [ (0, magitHere >> terminalHere)
         , (1, spawnHere "insomnia" >> terminalHere)
         ]
       , TI "h/Crypto Tracker"
            "haskell/crypto-tracker"
            (magitHere >> terminalHere)
       , TG formalMethodsGroup
            "Studium/FormalMethods"
            [(0, spawnHere "eclipse" >> spawnHere emacs), (1, mempty)]
       ]
 where
  -- | Associate a directory with the topic, but don't spawn anything.
  noAction :: Topic -> Dir -> TopicItem
  noAction n d = TI n d mempty

  -- | Basically a normal workspace.
  only :: Topic -> TopicItem
  only n = noAction n "./"

formalMethodsGroup :: String
formalMethodsGroup = book <> " Formal Methods"

kivworkspace :: String
kivworkspace = formalMethodsGroup <> " - 2"

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
browse = spawnHere . ("chromium --new-window " <>)


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
    .   onWorkspace kivworkspace simplestFloat
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
willFloat :: Query Bool
willFloat = ask >>= \w -> liftX $ withDisplay $ \d -> do
  sh <- io $ getWMNormalHints d w
  let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
  isTransient <- isJust <$> io (getTransientForHint d w)
  return (isFixedSize || isTransient)


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
    , className
      =?   "sun-awt-X11-XFramePeer"
      <||> className
      =?   "Main"
      -->  doShift kivworkspace
    , title =? "Netflix" <||> className =? "vlc" --> doFixAspect (16 / 9)
    ]
  windowRules =
    [ [ className =? c --> doFloat | c <- floatsClasses ]
    , [ title =? t --> doFloat | t <- floatsTitles ]
    ]
  floatsClasses =
    ["MPlayer", "Gimp", "yakuake", "Plasma-desktop", "ksmserver", "R_x11"]
  floatsTitles = ["alsamixer"]

---------------------
-- Status Bars
---------------------

circleSep :: String
circleSep =
  "<icon=circle_right.xpm/></fc>  <fc=#D8DEE9,#2E3440:0><icon=circle_left.xpm/>"

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

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0       = pure $ statusBarProp "xmobar top" topPP <> trayerSB
barSpawner s@(S n) = pure $ statusBarPropTo
  ("_XMONAD_LOG__Secondary_" <> show n)
  ("xmobar secondary " <> show n)
  (secondaryPP s)

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
