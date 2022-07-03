;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
; https://github.com/tkf/org-mode/blob/master/lisp/org-faces.el
(defun org-copy-face (old-face new-face docstring &rest attributes)
  (unless (facep new-face)
    (if (fboundp 'set-face-attribute)
        (progn
          (make-face new-face)
          (set-face-attribute new-face nil :inherit old-face)
          (apply 'set-face-attribute new-face nil attributes)
          (set-face-doc-string new-face docstring))
      (copy-face old-face new-face)
      (if (fboundp 'set-face-doc-string)
          (set-face-doc-string new-face docstring)))))
(put 'org-copy-face 'lisp-indent-function 2)
(defun ndu-view-tag (str)
  "Concat STR with @6-months-ago +unread."
  (concat "@6-months-ago +unread " str))
(defun ndu-eww-open (&optional use-generic-p)
  "elfeed open with eww"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (eww-browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun ndu-nov-mode ()
  (archive-mode)
  (nov-mode)
  (face-remap-add-relative 'variable-pitch
                           :family "Liberation Serif"
                           :height 1.0)
  (olivetti-mode))
(defun ndu-elfeed-mode ()
  (require 'elfeed)
  (define-key elfeed-search-mode-map (kbd "m") 'ndu-eww-open)
  (defhydra ap/elfeed-search-view (elfeed-search-mode-map "d" :color blue)
                                        ; press d + (h, j, etc)
    "Set elfeed-search filter tags."
    ("h" (elfeed-search-set-filter nil) "Default")
    ("j" (elfeed-search-set-filter (ndu-view-tag "+left")) "left")
    ("k" (elfeed-search-set-filter (ndu-view-tag "+news")) "news")
    ("l" (elfeed-search-set-filter (ndu-view-tag "+foreign")) "foreign")
    ("b" (elfeed-search-set-filter (ndu-view-tag "+opinion")) "opinion")
    ("n" (elfeed-search-set-filter (ndu-view-tag "+tech")) "tech"))
  (ndu-set-hooks '((elfeed-search-mode-hook (olivetti-mode))
                   (elfeed-show-mode-hook (olivetti-mode))
                   (eww-mode-hook (olivetti-mode))
                   (eww-mode-hook (writeroom-mode))))
  (add-hook 'eww-after-render-hook 'eww-readable))
(defun ndu-save-recompile () (interactive) (save-buffer) (recompile))
(defun ndu-set-leader (package binds)
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (car x) (cadr x)))
        binds))
(defun ndu-chronos ()
  (setq-default chronos-shell-notify-program "xterm"
                chronos-shell-notify-parameters
                '("-e" "tput bel; sleep 2; tput bel; sleep 2; tput bel;
                        sleep 2")
                chronos-expiry-functions #'(chronos-shell-notify
                                            chronos-message-notify)))
(defun ndu-set-hooks (dat)
  (mapc (lambda (x)
          (mapc (lambda (y)
                  (add-hook (car x) y))
                (cadr x)))
        dat))
(defun ndu-set-keys (dat global)
  (mapc (lambda (x)
          (let ((key (kbd (car x)))
                (key-function (eval (cadr x))))
            (funcall (if global #'global-unset-key #'local-unset-key)
                     key)
            (funcall (if global #'global-set-key #'local-set-key)
                     key key-function)))
        dat))
(defun ndu-indent-relative-below ()
  (interactive)
  "Indent relative, but works for the line below rather than above"
  (defun ndu-move-line-down ()
    (let ((col (current-column)))
      (save-excursion
        (forward-line)
        (transpose-lines 1))
      (forward-line)
      (move-to-column col)))
  (defun ndu-move-line-up ()
    (let ((col (current-column)))
      (save-excursion
        (forward-line)
        (transpose-lines -1))
      (forward-line -2)
      (move-to-column col)))
  (ndu-move-line-down)
  (indent-relative)
  (ndu-move-line-up))
(defun ndu-org-mode ()
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (with-eval-after-load 'org
    ;; faster math input
    (dolist (face '(org-level-1
                    org-level-2
                    org-level-3
                    org-level-4
                    org-level-5))
      (set-face-attribute face nil :weight 'regular :height 1.0))
    (ndu-set-hooks '((org-mode-hook (turn-on-org-cdlatex))
                     (org-mode-hook (auto-complete-mode))
                     (org-mode-hook (olivetti-mode))
                     (org-mode-hook (writeroom-mode))
                     (org-mode-hook
                      ((lambda ()
                         (push '("[ ]" .  "🞎") prettify-symbols-alist)
                         (push '("[X]" . "🗷" ) prettify-symbols-alist)
                         (push '("-" . "—" ) prettify-symbols-alist)
                         (push '("::" . "⁚" ) prettify-symbols-alist)
                         (prettify-symbols-mode))))
                     (org-after-todo-statistics-hook (org-summary-todo))))
		;; https://orgmode.org/worg/org-contrib/org-drill.html#orgeb853d5
		(setq org-capture-templates
          `(("n" "note" plain (file+headline "~/org/gtd.org" "Notes")
             "- %?" :prepend t)))
    (setq-default
     org-emphasis-alist '(("*" bold)
                          ("/" italic)
                          ("_" underline)
                          ("=" (bold :foreground "black" :background "red"))
                          ("~" (bold :foreground "black" :background "orange"))
                          ("+" (bold :foreground "black" :background "green")))
     org-superstar-item-bullet-alist '((?* . ?↠) (?+ . ?⇝) (?- . ?→))
     org-superstar-headline-bullets-list '("■" "◆" "•" "◉" "○" "▶")
     org-checkbox-hierarchical-statistics nil
      ;; disable confirm message when evaluating stuff for org-babel
      ;org-confirm-babel-evaluate nil
      ;; org-capture
      ;;http://orgmode.org/manual/Setting-up-capture.html
      ;; If you would like a TODO entry to automatically change to DONE when all
      ;; children are done, you can use the following setup.
      ;; (http://orgmode.org/manual/Breaking-down-tasks.html)
      org-log-done 'time
      org-todo-keyword-faces '(("TODO" . (:foreground "purple4" :weight bold))
                               ("DONE" . (:foreground "purple1" :weight bold)))
      org-directory "~/org"
      org-agenda-files '("~/org/gtd.org")
      ;; MobileOrg iphone app
      ;; http://mobileorg.ncogni.to/doc/getting-started/using-dropbox/
      ;; Set to the location of your Org files on your local system
      ;; Set to the name of the file where new notes will be stored
      ;org-mobile-inbox-for-pull "~/org/in.org"
      ;org-mobile-directory "~/windoze/Dropbox/Apps/MobileOrg"
      org-priority-faces '((?A . (:foreground "PaleVioletRed" :weight bold))
                           (?B . (:foreground "orange"        :weight bold))
                           (?C . (:foreground "green"         :weight bold)))))
  (custom-set-faces '(org-checkbox ((t (:foreground "red" :weight bold)))))
  (org-copy-face 'org-todo 'org-checkbox-statistics-todo
                 "Face used for unfinished checkbox statistics."))
(defun ndu-latex ()
  (defun add-envs ()
    (LaTeX-add-environments '("IEEEeqnarray" "alignment")
                            '("IEEEeqnarray*" "alignment")))
  ;; support for ieeeeqnarray environment
  ;; http://tex.aspcode.net/view/635399273629833626123752/auctex-how-to-enable-auto-expansion-of-sub-and-superscript-in-custom-math-environment
  ;; note: support for this environment is only partial if auctex sees
  ;; the 'usepackage' macro for it - need following code for full support
  (ndu-set-hooks '((LaTeX-mode-hook ((lambda () (setq evil-shift-width 2))
                                     add-envs))
                   (LaTeX-mode-hook (olivetti-mode))
                   (LaTeX-mode-hook (writeroom-mode))))
  (setq-default
    font-latex-math-environments '("display" "displaymath" "equation"
                                    "eqnarray" "gather" "multline" "align"
                                    "alignat" "xalignat" "IEEEeqnarray"
                                    "IEEEeqnarray*")
    ;; reftex code to recognize this environment as an equation
    reftex-label-alist '(("IEEEeqnarray" ?e nil nil t)
                         ("IEEEeqnarray*" ?e nil nil t))
    texmathp-tex-commands '(("IEEEeqnarray" env-on) ("IEEEeqnarray*" env-on))
    ;; auto-expand sub/superscript
    TeX-electric-sub-and-superscript t))
(defun ndu-ansi-color ()
  "Ansi colors in compilation mode"
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))
(defun ndu-doxymacs ()
  ;(mapc #'load '("~/.emacs.d/manuallyinstalled/xml-parse.el"
  ;              "~/.emacs.d/manuallyinstalled/doxymacs.el"))
  (ndu-set-keys '(("C-'"   #'doxymacs-insert-function-comment)
                  ("C-\""  #'doxymacs-insert-file-comment))
                nil))
(defun ndu-taskjuggler ()
   (load "~/.emacs.d/manuallyInstalled/taskjuggler-mode.el"))
(defun ndu-clojure ()
  (setq-default clojure-enable-fancify-symbols t))
(defun ndu-emacs-lisp ()
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook (olivetti-mode)))
(defun ndu-c-mode () ;;helm-gtags-mode
  (ndu-set-hooks '((c-mode-hook (doxymacs-mode
                                 (lambda ()
                                   (setq evil-shift-width 4)
                                   ; (ggtags-mode 1)
                                   (setq-default c-basic-offset 4)
                                   (flycheck-select-checker 'c/c++-gcc)))))))
(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
   You should not put any user code in this function besides modifying the
   variable values."
  (setq-default
    ;; Base distribution to use. This is a layer contained in the directory
    ;; `+distribution'. For now available distributions are `spacemacs-base'
    ;; or `spacemacs'. (default 'spacemacs)
    dotspacemacs-distribution 'spacemacs
    ;; List of additional paths where to look for configuration layers.
    ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
    dotspacemacs-configuration-layer-path '()
    ;; List of configuration layers to load. If it is the symbol `all' instead
    ;; of a list then all discovered layers will be installed.
    dotspacemacs-configuration-layers
    '(html ivy org git pdf
      (shell :variables shell-default-shell 'ansi-term)
      (auto-completion
       :variables spacemacs-default-company-backends '(company-files
                                                       company-capf))
      better-defaults markdown
      syntax-checking  ; gtags java
      (lsp :variables lsp-lens-enable t)
      (c-c++
       :variables
       c-c++-adopt-subprojects t
       c-c++-backend 'lsp-cclas)
      dap emacs-lisp elfeed ;mu4e semantic themes-megapack
      csv python ess clojure scheme octave
      (latex :variables latex-build-command "LaTeX")
      ;(mu4e :variables mu4e-use-maildirs-extension t)
      ;(mu4e :variables mu4e-enable-notifications t)
      ;(shell :variables shell-enable-smart-Eshell t)
      ;(elfeed :variables elfeed-enable-web-interface t)
      (elfeed :variables elfeed-enable-goodies nil)
      (elfeed :variables rmh-elfeed-org-files (list "~/org/feeds.org"))
	    (spell-checking :variables spell-checking-enable-by-default nil))
    ; List of additional packages that will be installed without being
    ;; wrapped in a layer. If you need some configuration for these
    ;; packages then consider to create a layer, you can also put the
    ;; configuration in `dotspacemacs/config'.helm-R
    dotspacemacs-additional-packages '(chronos ansi-color anki-editor
                                       evil-smartparens cdlatex vterm
                                       latex-extra latex-math-preview
                                       wordnut adaptive-wrap matlab-mode
                                       nov olivetti hydra lsp-mode lsp-ui
                                       ccls)
    ;; A list of packages and/or extensions that will not be install and loaded
    dotspacemacs-excluded-packages '(org-projectile)
    ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
    ;; are declared in a layer which is not a member of
    ;; the list `dotspacemacs-configuration-layers'. (default t)
    dotspacemacs-delete-orphan-packages t))
(defun dotspacemacs/init ()
  "Initialization function.
   This function is called at the very startup of Spacemacs initialization
   before layers configuration.
   You should not put any user code in there besides modifying the variable
   values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
    ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
    ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
    ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
    ;; unchanged. (default 'vim)
    dotspacemacs-editing-style 'vim
    dotspacemacs-line-numbers t
    ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
    dotspacemacs-verbose-loading nil
    ;; Specify the startup banner. Default value is `official', it displays
    ;; the official spacemacs logo. An integer value is the index of text
    ;; banner, `random' chooses a random text banner in `core/banners'
    ;; directory. A string value must be a path to an image format supported
    ;; by your Emacs build.
    ;; If the value is nil then no banner is displayed. (default 'official)
    dotspacemacs-startup-banner 'official
    ;; List of items to show in the startup buffer. If nil it is disabled.
    ;; Possible values are: `recents' `bookmarks' `projects'.
    ;; (default '(recents projects))
    ; ;dotspacemacs-startup-lists '(recents)
    ;; List of themes, the first of the list is loaded when spacemacs starts.
    ;; Press <SPC> T n to cycle to the next theme in the list (works great
    ;; with 2 themes variants, one dark and one light)
    dotspacemacs-themes '(tsdh-light)
    ;; If non nil the cursor color matches the state color.
    dotspacemacs-colorize-cursor-according-to-state t
    ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
    ;; size to make separators look not too crappy.
    dotspacemacs-default-font '("Source Code Pro Light"
                                :size 30
                                :weight light
                                :width normal
                                :powerline-scale 1.1)
    ;; The leader key
    dotspacemacs-leader-key "SPC"
    ;; The leader key accessible in `emacs state' and `insert state'
    ;; (default "M-m")
    dotspacemacs-emacs-leader-key "M-m"
    ;; Major mode leader key is a shortcut key which is the equivalent of
    ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
    dotspacemacs-major-mode-leader-key ","
    ;; Major mode leader key accessible in `emacs state' and `insert state'.
    ;; (default "C-M-m)
    dotspacemacs-major-mode-emacs-leader-key "C-M-m"
    ;; The command key used for Evil commands (ex-commands) and
    ;; Emacs commands (M-x).
    ;; By default the command key is `:' so ex-commands are executed like in
    ;; Vim with `:' and Emacs commands are executed with `<leader> :'.
    dotspacemacs-command-key ":"
    ;; If non nil `Y' is remapped to `y$'. (default t)
    dotspacemacs-remap-Y-to-y$ t
    ;; Location where to auto-save files. Possible values are `original' to
    ;; auto-save the file in-place, `cache' to auto-save the file to another
    ;; file stored in the cache directory and `nil' to disable auto-saving.
    ;; (default 'cache)
    dotspacemacs-auto-save-file-location 'cache
    ;; If non nil then `ido' replaces `helm' for some commands. For now only
    ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
    ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
    dotspacemacs-use-ido nil
    ;; If non nil, `helm' will try to miminimize the space it uses.
    ;; (default nil)
    dotspacemacs-helm-resize nil
    ;; if non nil, the helm header is hidden when there is only one source.
    ;; (default nil)
    dotspacemacs-helm-no-header nil
    ;; define the position to display `helm', options are `bottom', `top',
    ;; `left', or `right'. (default 'bottom)
    dotspacemacs-helm-position 'bottom
    ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
    ;; several times cycle between the kill ring content. (default nil)
    dotspacemacs-enable-paste-micro-state nil
    ;; Which-key delay in seconds. The which-key buffer is the popup listing
    ;; the commands bound to the current keystroke sequence. (default 0.4)
    dotspacemacs-which-key-delay 0.4
    ;; Which-key frame position. Possible values are `right', `bottom' and
    ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
    ;; right; if there is insufficient space it displays it at the bottom.
    ;; (default 'bottom)
    dotspacemacs-which-key-position 'bottom
    ;; If non nil a progress bar is displayed when spacemacs is loading. This
    ;; may increase the boot time on some systems and emacs builds, set it to
    ;; nil to boost the loading time. (default t)
    dotspacemacs-loading-progress-bar t
    ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
    ;; (Emacs 24.4+ only)
    dotspacemacs-fullscreen-at-startup nil
    ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
    ;; Use to disable fullscreen animations in OSX. (default nil)
    dotspacemacs-fullscreen-use-non-native nil
    ;; If non nil the frame is maximized when Emacs starts up.
    ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
    ;; (default nil) (Emacs 24.4+ only)
    dotspacemacs-maximized-at-startup t
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's active or selected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-active-transparency 90
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's inactive or deselected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-inactive-transparency 90
    ;; If non nil unicode symbols are displayed in the mode line. (default t)
    dotspacemacs-mode-line-unicode-symbols t
    ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
    ;; scrolling overrides the default behavior of Emacs which recenters the
    ;; point when it reaches the top or bottom of the screen. (default t)
    dotspacemacs-smooth-scrolling t
    ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
    ;; (default nil)
    dotspacemacs-smartparens-strict-mode t
    ;; Select a scope to highlight delimiters. Possible values are `any',
    ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
    ;; emphasis the current one). (default 'all)
    dotspacemacs-highlight-delimiters 'all
    ;; If non nil advises quit functions to keep server open when quitting.
    ;; (default nil)
    dotspacemacs-persistent-server nil
    ;; List of search tool executable names. Spacemacs uses the first installed
    ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
    ;; (default '("ag" "pt" "ack" "grep"))
    dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
    ;; The default package repository used if no explicit repository has been
    ;; specified with an installed package.
    ;; Not used for now. (default nil)
    dotspacemacs-default-package-repository nil))
(defun dotspacemacs/user-init ()
  "Initialization function for user code.
   It is called immediately after `dotspacemacs/init'.  You are free to put any
   user code."
  (setq-default
     line-spacing 40 ; space between lines
     ;mu4e-maildir "~/mail/mbsyncmail"
     ;mu4e-get-mail-command "mbsync -c ~/.emacs.d/.mbsyncrc -a"
     ;mu4e-index-update-in-background t
     ;mu4e-update-interval 120
     ;mu4e-headers-auto-update t
     ;mu4e-attachment-dir "~/Downloads"
     ;mu4e-drafts-folder "/Drafts"
     ;mu4e-sent-folder   "/Sent Items"
     ;mu4e-trash-folder  "/Deleted Items"
     ;mu4e-view-show-images t
     ;mu4e-enable-notifications t
     )
  ;(with-eval-after-load 'mu4e-alert
  ;  (mu4e-alert-set-default-style 'libnotify))
  (define-key key-translation-map (kbd "C-<escape>") (kbd "ESC"))
  (define-key key-translation-map (kbd "ESC") (kbd "C-C C-G"))
  (global-set-key (kbd "C-c C-g") 'evil-escape))
(defun dotspacemacs/user-config ()
  "Configuration function for user code.
   This function is called at the very end of Spacemacs initialization after
   layers configuration. You are free to put any user code. "
  ;(with-eval-after-load 'company
  ; (add-to-list 'company-backends '(company-capf company-dabbrev)))
  (mapc (lambda (x)
          (add-to-list (car x) (cadr x)))
        '((load-path "/usr/share/wordnet")
          (evil-emacs-state-modes wordnut-mode)))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode)
  (use-package nov
    :mode ("\\.epub\\'" . ndu-nov-mode))
  (global-visual-line-mode t)
  ;(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (ndu-set-leader 'chronos
                  '(("on" chronos-add-timers-from-string)
                    ("om" chronos-delete-all-expired)))
  (ndu-set-leader 'anki-editor
                  '(("oa" anki-editor-cloze-dwim)
                    ("os" anki-editor-cloze-region)
                    ("ol" anki-editor-latex-region)
                    ("op" anki-editor-push-notes)
                    ("oo" anki-editor-retry-failure-notes)
                    ("oi" anki-editor-insert-note)))
  (spacemacs/set-leader-keys-for-major-mode 'nov-mode
    "g" 'nov-render-document
    "v" 'nov-view-source
    "V" 'nov-view-content-source
    "m" 'nov-display-metadata
    "n" 'nov-next-document
    "]" 'nov-next-document
    "p" 'nov-previous-document
    "[" 'nov-previous-document
    "t" 'nov-goto-toc)
  (when (fboundp 'imagemagick-register-types)
   (imagemagick-register-types))
  ;(add-to-list 'mu4e-headers-actions
  ; '("View in browser" . mu4e-action-view-in-browser) t)
  (setq-default
    org-clock-sound "~/.emacs.d/manuallyInstalled/bell.wav"
    dotspacemacs-whitespace-cleanup 'all
    dotspacemacs-check-for-update t
    spacemacs-yank-indent-threshold 0
    flycheck-python-pycompile-executable "python3"
    python-shell-interpreter "python3"
    auto-save-default t
    visual-fill-column-center-text t
    vterm-always-compile-module t
    olivetti-style 'fancy
    olivetti-body-width 86
    writeroom-width 86
    olivetti-margin-width 5
    nov-text-width 60
    c-default-style "k&r"
    c-basic-offset 4
    org-hide-emphasis-markers t
    whitespace-line-column 80                    ; After 79 chars,
    whitespace-style '(face lines-tail)          ; highlight columns.
    backup-directory-alist `(("." . "~/.saves")) ; file backups
    flycheck-highlighting-mode 'symbols
    flycheck-indication-mode 'left-fringe
    evil-use-y-for-yank t
    c-c++-lsp-enable-semantic-highlight t)
  (ndu-set-keys '(("C->"  #'indent-relative)
                  ("C-<"  #'ndu-indent-relative-below)
                  ("C-\'" #'ndu-save-recompile))
                t)
  (ndu-set-hooks '((python-mode-hook (olivetti-mode))
                   (c-mode-hook (olivetti-mode))
                   (emacs-lisp-mode-hook (olivetti-mode))
                   (sh-mode-hook (olivetti-mode))
                   (c++-mode-hook (olivetti-mode))
                   (c++-mode-hook (lsp))
                   (term-mode-hook (olivetti-mode))
                   (term-mode-hook (writeroom-mode))))
  (load "~/.emacs.d/manuallyInstalled/ed-mode.el")
  (mapc #'funcall
        #'(spacemacs/toggle-menu-bar-on
           spacemacs/toggle-highlight-current-line-globally-off
           global-whitespace-mode
           global-flycheck-mode
           ; global-auto-complete-mode
           ndu-chronos
           ndu-org-mode
           ndu-latex
           ndu-ansi-color
           ndu-doxymacs
           ndu-taskjuggler
           ndu-clojure
           ndu-emacs-lisp
           ndu-c-mode
           ndu-elfeed-mode))
  (find-file "~/org/gtd.org"))
