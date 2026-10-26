;; ;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(require 'acp)
(require 'agent-shell)
(setq user-full-name "Yecine Megdiche"
      user-mail-address "yecine.megdiche@gmail.com")
(setq truncate-lines nil)
(setq doom-font (font-spec :family "Fira Code" :size 14.0))
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

(after! lsp-ui-mode
  (set-lookup-handlers! 'lsp-mode
    :definition #'+lsp-lookup-definition-handler
    :references #'+lsp-lookup-references-handler))

(add-hook! 'typescript-mode-hook
  (add-function :before-while (local 'tree-sitter-hl-face-mapping-function)
                (lambda (capture-name)
                  (pcase capture-name
                    ("private" 'font-lock-comment-face)))))

(setq pdf-view-resize-factor 1.1)
;; (use-package! frames-only-mode
;;   :hook (after-init . frames-only-mode))
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
             ;; Use plain `setq` (not `setq!`) so the custom setter's `:type` check
             ;; doesn't warn about the `tree-sitter-hl-face:*` symbols, which aren't
             ;; declared in `flyspell-prog-text-faces`'s `(set (const ...))` type.
             (setq flyspell-prog-text-faces
                   '(font-lock-string-face
                     font-lock-comment-face
                     font-lock-doc-face
                     tree-sitter-hl-face:string
                     tree-sitter-hl-face:comment
                     tree-sitter-hl-face:doc)
                   flyspell-lazy-idle-seconds 0.5)))


(custom-set-faces!
  '(corfu-current :background "#51576d"))

(setq! corfu-preselect 'valid)

(setq! sql-postgres-login-params nil)

(defun my/ts-visit-test (arg)
  "Visit the test file for the current buffer."
  (interactive "P")
  (if (not (derived-mode-p 'typescript-mode))
      (message "Not in a typescript file!")
    (let* ((file-name (file-name-sans-extension buffer-file-name))
           (unit-test (not arg))
           (test-file (if unit-test
                          (concat file-name ".spec.ts")
                        (concat file-name ".integration-spec.ts"))))
      (if (file-exists-p test-file)
          (find-file test-file)
        (if (yes-or-no-p (format "Test file %s does not exist. Create it?" test-file))
            (find-file test-file))))))


(defun my/ts-toggle-test (arg)
  "Toggle between implementation and test file."
  (interactive "P")
  (if (not (derived-mode-p 'typescript-mode))
      (message "Not in a typescript file!")
    (if (string-match-p "spec" buffer-file-name)
        (let* ((base-file-name (file-name-sans-extension (file-name-sans-extension buffer-file-name)))
               (src-file (concat base-file-name ".ts")))
          (if (file-exists-p src-file)
              (find-file src-file)
            (message "File %s does not exist" src-file)))
      (my/ts-visit-test arg))))


(map! :after typescript-ts-mode
      :map typescript-ts-mode-map
      :leader
      :desc "Visit test file" "o t" #'my/ts-visit-test
      :desc "Toggle test file" "t t" #'my/ts-toggle-test)

(after! flycheck
  (defun my/oxlint-find-working-directory (_checker)
    "Locate the directory holding `.oxlintrc.json' for the oxlint CHECKER.
oxlint only treats a config as the *root* config when it sits in the
process's working directory; one discovered further up the tree is a
nested config, where root-only keys like `options.typeAware' are a parse
error.  Flycheck otherwise runs from the buffer's own directory."
    (when buffer-file-name
      (locate-dominating-file buffer-file-name ".oxlintrc.json")))

  ;; Plain `put' rather than `setf' on `flycheck-checker-get': `after!' expands
  ;; its body before flycheck is loaded, so flycheck's `gv-define-setter' isn't
  ;; known yet and `setf' would compile to a nonexistent runtime setter.
  (put 'javascript-oxlint 'flycheck-working-directory
       #'my/oxlint-find-working-directory))

(after! apheleia
  ;; apheleia's stock `oxfmt' entry formats a copy under
  ;; `temporary-file-directory', so oxfmt can never walk up to the project's
  ;; `.oxfmtrc.json' and silently formats with its own defaults (printWidth 100
  ;; rather than 120, no import sorting).  Feeding it stdin keeps the real file
  ;; name visible, which is what oxfmt resolves the config from.
  (setf (alist-get 'oxfmt apheleia-formatters)
        '("apheleia-npx" "oxfmt" "--stdin-filepath" filepath))

  ;; Choosing the formatter here rather than from a major-mode hook means
  ;; buffers visited before apheleia lazy-loaded aren't left stuck on prettier.
  ;; `js-json-mode' is listed separately because it derives from `prog-mode',
  ;; not `js-base-mode'.
  (defadvice! my/apheleia-prefer-oxfmt (fn &optional interactive)
    :around #'apheleia--get-formatters
    (if (and (not (eq interactive 'prompt))
             buffer-file-name
             (derived-mode-p '(typescript-ts-base-mode typescript-mode
                               js-base-mode js-json-mode json-ts-mode))
             (locate-dominating-file buffer-file-name ".oxfmtrc.json"))
        '(oxfmt)
      (funcall fn interactive))))

(after! lsp-mode
  ;; lsp-mode hardcodes `(inlineCompletion . ())', which serializes to
  ;; "inlineCompletion": null; tsgo unmarshals capabilities into Go structs and
  ;; rejects an explicit null there, failing `initialize'.  Other servers accept
  ;; it, so only rewrite it for tsgo.
  (defadvice! my/lsp-tsgo-fix-inline-completion (caps)
    :filter-return #'lsp--client-capabilities
    (if (not (eq 'tsgo (and lsp--cur-workspace
                            (lsp--client-server-id
                             (lsp--workspace-client lsp--cur-workspace)))))
        caps
      (let ((td (assq-delete-all 'inlineCompletion
                                 (copy-alist (alist-get 'textDocument caps)))))
        (cons (cons 'textDocument
                    (cons (cons 'inlineCompletion (make-hash-table :test 'equal)) td))
              (assq-delete-all 'textDocument (copy-alist caps)))))))

(after! lsp-javascript
  ;; TypeScript 7's `tsc' binary serves `--lsp' itself, so use the version the
  ;; project pins instead of the one lsp-mode downloads into its cache.
  (defun my/lsp-use-project-tsgo ()
    (when-let* ((dir (locate-dominating-file default-directory "node_modules/.bin/tsc")))
      (setq-local lsp-clients-tsgo-path
                  (expand-file-name "node_modules/.bin/tsc" dir))))
  (add-hook 'typescript-ts-base-mode-hook #'my/lsp-use-project-tsgo))

(use-package! agent-shell
  :bind ("C-c C-a" . agent-shell)
  :config
  ;; direnv-mode resets exec-path per-buffer (even without an .envrc), which
  ;; drops the fnm bin dir. Use an absolute path so PATH lookup never matters.
  (setq agent-shell-anthropic-claude-acp-command
        '("/Users/yecinem/.local/share/fnm/node-versions/v22.23.2/installation/bin/claude-agent-acp"))
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

(use-package! claude-code-ide
  :bind ("C-c C-l" . claude-code-ide-menu) 
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'ghostel)
  (setq claude-code-ide-cli-path "/Users/yecinem/.local/bin/claude")) 

(after! ghostel
  (defun my/ghostel-send-escape ()
    (interactive)
    (ghostel-send-key "escape"))
  (map! :map ghostel-mode-map :i "C-c <escape>" #'my/ghostel-send-escape)
  ;; ghostel-project already handles per-project terminals (with -next/
  ;; -previous/-list-buffers and numbered instances); it just always
  ;; switches into the buffer instead of toggling it away again.
  (defvar-local my/ghostel-project--return-buffer nil
    "Buffer to return to when toggling out of this ghostel buffer.")
  (defun my/ghostel-project-toggle (&optional arg)
    "Toggle between this project's ghostel terminal and the buffer before it.
With a prefix ARG, create a new terminal instance for this project
instead (passed through to `ghostel-project')."
    (interactive "P")
    (if (and (derived-mode-p 'ghostel-mode) (not arg))
        (if (buffer-live-p my/ghostel-project--return-buffer)
            (switch-to-buffer my/ghostel-project--return-buffer)
          (previous-buffer))
      (let ((return-buffer (current-buffer)))
        (ghostel-project arg)
        (setq my/ghostel-project--return-buffer return-buffer))))
  (map! "C-c g g" #'my/ghostel-project-toggle
        "C-c g l" #'ghostel-project-list-buffers
        "C-c g n" #'ghostel-project-next
        "C-c g p" #'ghostel-project-previous))

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

;; Review one commit at a time. State lives in a git ref, not an Elisp
;; variable, so a crash leaves `refs/my-review/original' behind to reset to
;; by hand. `my/review-next' fast-forwards to the original SHA when the
;; staged tree is unchanged, and only recommits when it isn't.
(after! magit
  (defun my/review--ref (name) (format "refs/my-review/%s" name))

  (defun my/review--remaining ()
    (magit-git-lines "log" "--reverse" "--format=%H"
                     (concat "HEAD.." (my/review--ref "original"))))

  (defun my/review--check-clean (sha)
    (when (magit-anything-unstaged-p)
      (user-error "Unstaged changes remain; stage or discard them first"))
    (let* ((touched (magit-git-lines "show" "--format=" "--name-only" sha))
           (stray (seq-intersection (magit-untracked-files) touched)))
      (when stray
        (user-error "Untracked, not staged: %s" (string-join stray ", ")))))

  (defun my/review--expose (sha)
    (magit-run-git "cherry-pick" "-n" sha)
    (message "Reviewing %s. Stage hunks, then C-c r n for the next commit."
             (magit-rev-abbrev sha)))

  (defun my/review--clean-untracked ()
    "Remove untracked leftovers under paths the reviewed range touched."
    (when-let* ((paths (magit-git-lines "diff" "--name-only"
                                        (my/review--ref "base") (my/review--ref "original"))))
      (apply #'magit-run-git "clean" "-fd" "--" paths)))

  (defun my/review--cleanup-refs ()
    (magit-run-git "update-ref" "-d" (my/review--ref "original"))
    (magit-run-git "update-ref" "-d" (my/review--ref "base")))

  (defun my/review-start (base)
    "Tag HEAD and BASE, reset to BASE, and expose HEAD's commits one at a time."
    (interactive (list (magit-read-branch-or-commit "Review commits back to")))
    (when (magit-rev-verify (my/review--ref "original"))
      (user-error "Review already in progress; call `my/review-end' first"))
    (when (magit-anything-modified-p)
      (user-error "Working tree isn't clean"))
    (magit-run-git "update-ref" (my/review--ref "original") "HEAD")
    (magit-run-git "update-ref" (my/review--ref "base") base)
    (magit-reset-hard base)
    (if-let* ((sha (car (my/review--remaining))))
        (my/review--expose sha)
      (message "Nothing to review between %s and HEAD" base)))

  (defun my/review-next ()
    "Commit (or fast-forward to) the commit under review, then expose the next one."
    (interactive)
    (unless (magit-rev-verify (my/review--ref "original"))
      (user-error "No review in progress"))
    (let ((sha (car (my/review--remaining))))
      (unless sha (user-error "No commit is currently exposed for review"))
      (my/review--check-clean sha)
      (if (equal (magit-git-string "write-tree") (magit-rev-parse (concat sha "^{tree}")))
          (magit-reset-hard sha)
        (magit-run-git "commit" "-C" sha)))
    (if-let* ((next (car (my/review--remaining))))
        (my/review--expose next)
      (my/review--cleanup-refs)
      (message "Review complete. HEAD is %s" (magit-rev-abbrev "HEAD"))))

  (defun my/review-end ()
    "Abort the review in progress, restoring the HEAD saved by `my/review-start'."
    (interactive)
    (unless (magit-rev-verify (my/review--ref "original"))
      (user-error "No review in progress"))
    (magit-reset-hard (my/review--ref "original"))
    (my/review--clean-untracked)
    (my/review--cleanup-refs)
    (message "HEAD restored to %s" (magit-rev-abbrev "HEAD")))

  (map! "C-c r s" #'my/review-start
        "C-c r n" #'my/review-next
        "C-c r e" #'my/review-end)

  (defun my/magit-insert-review-status ()
    (when (magit-rev-verify (my/review--ref "original"))
      (magit-insert-section (my-review)
        (magit-insert-heading "Commit Review")
        (let* ((all (magit-git-lines "log" "--reverse" "--format=%H"
                                     (concat (my/review--ref "base") ".." (my/review--ref "original"))))
               (remaining (my/review--remaining))
               (done (- (length all) (length remaining))))
          (dolist (sha all)
            ;; type `commit' reuses Magit's own keymap: RET opens the full
            ;; commit, TAB expands this section to show the body inline.
            (magit-insert-section (commit sha t)
              (magit-insert-heading
                (format "%s %s %-9s %s"
                        (cond ((equal sha (car remaining)) "->")
                              ((member sha remaining) "  ")
                              (t "ok"))
                        (propertize (magit-rev-abbrev sha) 'face 'magit-hash)
                        (if (equal sha (car remaining))
                            (format "(%d/%d)" (1+ done) (length all))
                          "")
                        (magit-git-string "log" "-1" "--format=%s" sha)))
              (insert (magit-git-string "log" "-1" "--format=%b" sha) "\n"))))
        (insert "\n"))))

  (magit-add-section-hook 'magit-status-sections-hook
                          #'my/magit-insert-review-status
                          #'magit-insert-staged-changes)

  ;; Magit has no "worktree created" hook, so advise the two commands that
  ;; make one; plain branch checkout is untouched. `my/worktree-create-command'
  ;; is a dir-local naming a function to call once, with `default-directory'
  ;; bound to the new worktree -- a raw dir-locals `eval' would rerun on
  ;; every later visit instead of just once at creation.
  (put 'my/worktree-create-command 'safe-local-variable #'functionp)
  (defun my/directory-local-value (directory symbol)
    "Return SYMBOL's dir-local value for DIRECTORY, or nil."
    (with-temp-buffer
      (setq default-directory directory)
      (hack-dir-local-variables-non-file-buffer)
      (and (local-variable-p symbol) (symbol-value symbol))))
  (defun my/magit-worktree-switch-to-workspace-a (directory &rest _)
    (setq directory (magit--expand-worktree directory))
    (when (file-directory-p directory)
      (projectile-switch-project-by-name directory)
      (when-let* ((command (my/directory-local-value directory 'my/worktree-create-command)))
        (let ((default-directory directory))
          (funcall command)))))
  (advice-add 'magit-worktree-checkout :after #'my/magit-worktree-switch-to-workspace-a)
  (advice-add 'magit-worktree-branch :after #'my/magit-worktree-switch-to-workspace-a))

(defun my/bun-install ()
  "Run `bun install' in `default-directory' via `compile', if it has a package.json."
  (interactive)
  (if (file-exists-p (expand-file-name "package.json" default-directory))
      (compile "bun install; bun agents:sync")
    (message "No package.json in %s, skipping bun install" default-directory)))
