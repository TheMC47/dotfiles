;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! frames-only-mode)
(package! autothemer)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))
