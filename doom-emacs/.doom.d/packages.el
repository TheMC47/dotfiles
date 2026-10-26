;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! frames-only-mode)
(package! autothemer)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! string-inflection)
(package! gptel)
(package! jest)
(package! catppuccin-theme)

(package! shell-maker)
(package! acp)
(package! agent-shell)
(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"))

;; ghostel and evil-ghostel now come from the :term ghostel Doom module
;; (init.el), which pins them and applies persp-mode/direnv compat hacks.

(package! agent-review
  :recipe (:host github
           :repo "nineluj/agent-review"
           :files ("*.el")))

(package! agent-shell)   
(package! agent-shell-hud
  :recipe (:host github :repo "nohzafk/agent-shell-hud"
           :build (:not compile)))

(package! agent-shell-hq
  :recipe (:host nil :repo "https://github.com/sreenivasvrao/agent-shell-hq"
           :files ("*.el")))
;; agent-shell-hq-peek requires posframe, not pulled in by any other module here.
(package! posframe)

;; agent-shell-knockknock needs xenodium's fork (unmerged PR #4: image icon
;; support), not the konrad1977 mainline.
(package! knockknock
  :recipe (:host github :repo "xenodium/knockknock"))
(package! agent-shell-knockknock
  :recipe (:host github :repo "xenodium/agent-shell-knockknock"))
