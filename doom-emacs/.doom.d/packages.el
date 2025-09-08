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
