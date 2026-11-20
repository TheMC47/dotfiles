;; ;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Yecine Megdiche"
      user-mail-address "yecine.megdiche@gmail.com")
(setq truncate-lines nil)
(setq doom-font (font-spec :family "Fira Code" :size 12.0))
(setq doom-font-increment 1.0)

(setq doom-theme 'doom-palenight)
(setq display-line-numbers-type `relative)
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
;; haskell-indent-offset was removed from newer haskell-mode but Doom still references it
(defvar haskell-indent-offset 2)

(after! lsp-ui-mode
  (set-lookup-handlers! 'lsp-mode
    :definition #'+lsp-lookup-definition-handler
    :references #'+lsp-lookup-references-handler))

(setq pdf-view-resize-factor 1.1)

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-M-<return>" . 'copilot-accept-completion)
              ("C-ö" . 'copilot-accept-completion)))

(setq auth-sources '("~/.authinfo"))


(custom-set-faces!
  '(corfu-current :background "#51576d"))

(setq! corfu-preselect 'valid)

(setq! sql-postgres-login-params nil)

(use-package! agent-shell
  :bind ("C-c C-a" . agent-shell)
  :config
  ;; direnv-mode resets exec-path per-buffer (even without an .envrc), so use
  ;; an absolute path. Installed with `npm i -g' under prefix ~/.local.
  (setq agent-shell-anthropic-claude-acp-command
        (list (expand-file-name "~/.local/bin/claude-agent-acp")))
  (setq agent-shell-anthropic-default-model-id "opus")
  (setq agent-shell-anthropic-default-session-mode-id "auto")
  (setq agent-shell-session-restore-verbosity 'full)
  ;; Without CLAUDE_AGENT_LOGS, claude-agent-acp aliases console.log to
  ;; console.error, so routine diagnostics land on stderr -- which acp.el
  ;; treats as a fatal error, aborting agent-review over nothing.
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "CLAUDE_AGENT_LOGS" (expand-file-name "claude-agent-acp" doom-cache-dir)))
  ;; agent-shell-anthropic-make-claude-code-config has no thought-level
  ;; defcustom of its own (unlike agent-shell-droid), so default it via
  ;; :default-config-options, which agent-shell-make-agent-config supports.
  (advice-add 'agent-shell-anthropic-make-claude-code-config :filter-return
              (lambda (config)
                (map-put! config :default-config-options
                          (lambda () '(("thought_level" . "high"))))
                config))
  ;; Line numbers don't belong in the viewport's read-only transcript view.
  (add-hook 'agent-shell-viewport-view-mode-hook
            (lambda () (display-line-numbers-mode -1)))
  (define-key agent-shell-viewport-view-mode-map (kbd "C-c C-r") #'agent-shell-viewport-reply)
  ;; C-c C-p peeks by swapping this buffer's mode, so it can't show both
  ;; views at once. The full chat lives in its own paired buffer -- split
  ;; to that instead for a real side-by-side view while composing.
  (defun my/agent-shell-viewport-split-to-chat ()
    "Toggle this viewport's underlying agent-shell chat buffer in another window."
    (interactive)
    (if-let* ((shell-buffer (agent-shell-viewport--shell-buffer)))
        (if-let* ((win (get-buffer-window shell-buffer)))
            (delete-window win)
          (display-buffer shell-buffer '((display-buffer-pop-up-window))))
      (user-error "No associated agent-shell buffer found")))
  (define-key agent-shell-viewport-edit-mode-map (kbd "C-c C-w") #'my/agent-shell-viewport-split-to-chat)
  ;; Every send path funnels through compose-send, so close the peek
  ;; window here rather than duplicating this in each one.
  (advice-add 'agent-shell-viewport-compose-send :before
              (lambda (&rest _)
                (when-let* ((shell-buffer (agent-shell-viewport--shell-buffer))
                            (win (get-buffer-window shell-buffer)))
                  (ignore-errors (delete-window win))))))

;; emojify treats agent-shell's ▶/▼/✓/✗ glyphs as emoji and clobbers their
;; `keymap' property, breaking RET on fold indicators.
(after! emojify
  (dolist (mode '(agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
    (add-to-list 'emojify-inhibit-major-modes mode)))

(use-package! agent-shell-knockknock
  :after (agent-shell knockknock)
  :hook (agent-shell-mode . agent-shell-knockknock-mode))

(use-package! agent-shell-hq
  :commands (agent-shell-hq-toggle agent-shell-hq-peek agent-shell-hq-label)
  :custom
  (agent-shell-hq-toggle-sidebar-width 50)
  (agent-shell-hq-peek-position 'right)
  (agent-shell-hq-peek-width 52)
  (agent-shell-hq-peek-height 60)
  (agent-shell-hq-peek-parameters '((background-color . "black")))
  (agent-shell-hq-label-command '("claude" "-p" "--model" "haiku"))
  (agent-shell-hq-label-context-chars 2000)
  (agent-shell-hq-label-prompt
   "Reply with ONLY a terse 8-10 word title for this conversation, lowercase, no punctuation:\n\n%s")
  :bind
  ("C-c a h" . agent-shell-hq-toggle)
  ("C-c a p" . agent-shell-hq-peek))

;; agent-shell-hq-toggle/-peek have no dedicated major mode, so evil can't
;; target them and their single-letter keymaps lose to evil-normal-state-map.
(after! (agent-shell-hq-toggle agent-shell-hq-peek evil)
  (evil-make-overriding-map agent-shell-hq-toggle-map 'normal)
  (evil-make-overriding-map agent-shell-hq-peek-map 'normal))
