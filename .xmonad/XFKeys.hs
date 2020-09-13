module XFKeys where

import           XMonad

-- XF86XK keys that I care about

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
