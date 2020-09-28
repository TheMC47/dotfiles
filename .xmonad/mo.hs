import Xmobar

config =
  defaultConfig
    { -- font = "xft:SFN display:size=10"
      font = "xft:Source Code Pro:size=11:regular:antialias=true",
      additionalFonts = ["xft:FontAwesome:pixelsize=13"],
      position = TopSize L 100 30,
      iconRoot = "/home/yecinem/.xmonad/xpm",
      commands =
        [ Run $
            Cpu
            [ "--template",
              "<ipat><total>%",
              "--Low",
              "55", -- units: %
              "--High",
              "77", -- units: %
              -- , "--low"      , "#26734D"
              -- , "--normal"   , "#CC5200"
              "--high",
              "#CD3C66",
              "--ppad",
              "3",
              "--width",
              "3",
              "--maxtwidth",
              "4",
              "--",
              "--load-icon-pattern",
              "<icon=cpu/cpu_%%.xpm/>"
            ]
            10,
          Run $
            Memory
            [ "--template",
              "<usedipat><usedratio>%",
              "--Low",
              "55", -- units: %
              "--High",
              "77", -- units: %
              -- , "--low"      , "#26734D"
              -- , "--normal"   , "#CC5200"
              "--high",
              "#CD3C66",
              "--ppad",
              "3",
              "--width",
              "3",
              "--maxtwidth",
              "4",
              "--",
              "--used-icon-pattern",
              "<icon=ram/ram_%%.xpm/>"
            ]
            10,
          Run $ Date "%a %b %_d <fn=1>\61463</fn> %H:%M" "date" 10, --- 
          Run $
            Battery
            [ "--template",
              "<leftipat> <acstatus>",
              "--Low",
              "20", -- units: %
              "--High",
              "90", -- units: %
              "--low",
              "#CD3C66",
              -- , "--normal"    , "#CC5200"
              "--high",
              "#26734D",
              "--maxtwidth",
              "10",
              "--",
              "--on-icon-pattern",
              "<icon=battery/on/battery_on_%%.xpm/>",
              "--off-icon-pattern",
              "<icon=battery/off/battery_off_%%.xpm/>",
              "--idle-icon-pattern",
              "<icon=battery/idle/battery_idle_%%.xpm/>",
              "-o",
              "<left><fc=#263640>%</fc> <timeleft>", -- discharging status
              "-O",
              "<left><fc=#263640>% <timeleft></fc>", -- plugged in status
              "-i",
              "<fc=#707880>IDLE</fc>", -- charged status
              "-A",
              "15",
              "-a",
              "notify-send -u critical 'Battery running out!'"
            ]
            50,
          Run $ UnsafeStdinReader
        ],
      sepChar = "%",
      alignSep = "}{",
      template = "<icon=haskell_20.xpm/> %UnsafeStdinReader%} <fn=1>\6155</fn> %date% { %cpu% | %memory% | %battery% " -- 
    }

main :: IO ()
main = xmobar config
