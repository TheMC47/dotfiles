;; ;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Yecine Megdiche"
      user-mail-address "yecine.megdiche@gmail.com")
(setq truncate-lines nil)
(setq doom-font (font-spec :family "Fira Code" :size 12.0))
(setq doom-font-increment 1.0)

;; (setq catppuccin-flavor 'frappe)
;; (load-theme 'catppuccin t t)
(setq doom-theme 'modus-vivendi-tinted)
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
    (setq! flyspell-prog-text-faces
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


(map! :after typescript-mode
      :map typescript-mode-map
      :leader
      :desc "Visit test file" "o t" #'my/ts-visit-test
      :desc "Toggle test file" "t t" #'my/ts-toggle-test)
