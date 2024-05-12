;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Yecine Megdiche"
      user-mail-address "yecine.megdiche@gmail.com")
(setq truncate-lines nil)
(setq doom-font (font-spec :family "Fira Code" :size 14))

(setq doom-catppuccin-dark-variant "frappe")
(setq doom-theme 'doom-catppuccin)
(setq display-line-numbers-type `relative)
(setq doom-themes-treemacs-theme "doom-colors")
(setq-default indent-tabs-mode nil)
(setq fancy-splash-image (concat doom-user-dir "club-mate.svg"))

(defun my/dired-go-home ()
  (interactive)
  (dired "~"))

(map!
 :after dired
 :map dired-mode-map
 :n "~" #'my/dired-go-home)
(map!
 :after yasnippet
 :map yas-minor-mode-map
 :i "C-," #'yas-expand)

(defun open-magit-or-dired (DIRECTORY)
  (interactive)
  (let ((toplevel (doom-git-toplevel DIRECTORY)))
    (setq DIRECTORY (file-name-as-directory
                     (expand-file-name DIRECTORY)))
    (if (and toplevel (file-equal-p DIRECTORY toplevel))
        (magit-status-setup-buffer DIRECTORY)
      (dired DIRECTORY))))

(setq +workspaces-switch-project-function #'open-magit-or-dired)

(setq lsp-haskell-formatting-provider "fourmolu")

(setq-hook! #'typescript-tsx-mode +format-with-lsp nil)
(setq-hook! 'typescript-mode-hook +format-with-lsp nil)

(setq pdf-view-resize-factor 1.1)
(use-package! frames-only-mode
  :hook (after-init . frames-only-mode))
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-M-<return>" . 'copilot-accept-completion)
              ("C-ö" . 'copilot-accept-completion)))

(setq auth-sources '("~/.authinfo"))

(after! flyspell
  (add-hook! prog-mode
    (setq! flyspell-prog-text-faces
           '(font-lock-string-face
             font-lock-comment-face
             font-lock-doc-face
             tree-sitter-hl-face:string
             tree-sitter-hl-face:comment
             tree-sitter-hl-face:doc)
           flyspell-lazy-idle-seconds 0.5)))
