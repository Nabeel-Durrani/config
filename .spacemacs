(defconst olivetti-packages
  '(olivetti))
(defun olivetti/init-olivetti ()
  (use-package olivetti))
(defun ndu/toggle-hscroll-mode ()
  (interactive)
  (if (eq auto-hscroll-mode 'current-line)
      (setq auto-hscroll-mode t)
    (setq auto-hscroll-mode 'current-line)))
(defun ndu/sync-beorg ()
  (interactive)
  (call-process-shell-command "rsync ~/org/*.org ~/org/*.bib ~/Dropbox/org &; rsync ~/org/*.pdf ~/Dropbox/UoCdocs &" nil 0))
(defun ndu/toggle-indent-mode ()
  (interactive)
  (cond ((and (bound-and-true-p electric-indent-mode)
              (bound-and-true-p aggressive-indent-mode))
         (electric-indent-mode nil)
         (aggressive-indent-mode -1))
        ((bound-and-true-p  electric-indent-mode)
         (electric-indent-mode nil)
         (aggressive-indent-mode t))
        ((bound-and-true-p aggressive-indent-mode)
         (aggressive-indent-mode -1)
         (electric-indent-mode t))
        (t (electric-indent-mode t)
           (aggressive-indent-mode -1))))
(defun ndu/toggle-follow-split ()
  (interactive)
  (if (bound-and-true-p follow-mode)
      (turn-off-follow-mode)
    (follow-delete-other-windows-and-split))
  (defun ndu/toggle-follow-split-off ()
    (interactive)
    (delete-other-windows)
    (turn-off-follow-mode)))
(defun ndu/org-syntax-table-modify ()
  "Modify `org-mode-syntax-table' for the current org buffer."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))
(defun ndu/table-edit-formulas ()
  (interactive)
  (ndu/expand)
  (org-table-edit-formulas))
(defun ndu/previous-match ()
  (interactive)
  (previous-error)
  (org-show-entry))
(defun ndu/next-match ()
  (interactive)
  (next-error)
  (org-show-entry))
(defun ndu/unsplit-table-field ()
  (interactive)
  (ndu/table-field-split-unsplit "\n" "^"))
(defun ndu/split-table-field ()
  (interactive)
  (ndu/table-field-split-unsplit "\\^" "\n"))
(defun ndu/table-field-split-unsplit (delim1 delim2)
  (interactive)
  (unless (not (string= "*Org Table Edit Field*" (buffer-name)))
    (save-excursion
      (beginning-of-buffer)
      (kill-matching-lines "^#.*$")
      (let ((buf (buffer-string)))
        (kill-matching-lines ".*")
        (insert (mapconcat 'identity
                           (split-string buf delim1)
                           delim2))))))
(defun ndu/edit-field ()
  (interactive)
  (if (or (not (symbolp 'org-at-table-p))
          (org-at-table-p))
      (progn
        (ndu/shrink)
        (split-window-horizontally)
        (org-table-edit-field nil)
        (ndu/split-table-field))
    (progn ; If we're already in the org-table-edit-field buffer
      (ndu/unsplit-table-field)
      (org-ctrl-c-ctrl-c)
      (delete-other-windows)
      (ndu/shrink))))
(defun ndu/next-column-edit ()
  (interactive)
  (ndu/next-edit-field #'evil-next-line #'evil-previous-line))
(defun ndu/next-row-edit ()
  (interactive)
  (ndu/next-edit-field #'org-cycle #'org-shifttab))
(defun ndu/next-edit-field (next-fn prev-fn)
  (interactive)
  (ndu/unsplit-table-field)
  (org-ctrl-c-ctrl-c)
  (save-excursion
    (funcall next-fn)
    (unless (org-at-table-p)
      (progn
        (funcall prev-fn)
        (org-table-insert-row 1))))
  (funcall next-fn)
  (org-table-edit-field nil)
  (ndu/split-table-field))
(defun ndu/expand ()
  (interactive)
  (org-table-expand)
  (ndu/show-tbmlfm))
(defun ndu/expand-all ()
  (interactive)
  (org-table-map-tables #'org-table-expand t)
  (org-table-map-tables #'org-table-align t)
  (ndu/show-tbmlfm))
(defun ndu/shrink ()
  (interactive)
  (ndu/hide-tbmlfm)
  (org-table-shrink))
(defun ndu/shrink-all ()
  (interactive)
  (org-table-map-tables #'org-table-shrink t)
  (org-table-map-tables #'org-table-align t)
  (ndu/hide-tbmlfm))
(defun ndu/remove-tag (tag)
  (interactive)
  (org-toggle-tag tag 'off))
(defun ndu/add-tag (tag)
  (interactive)
  (org-toggle-tag tag 'on))
(defun ndu/remove-question-tag ()
  (interactive)
  (ndu/remove-tag "question"))
(defun ndu/add-question-tag ()
  (interactive)
  (ndu/add-tag "question"))
(defun ndu/remove-card-tag ()
  (interactive)
  (ndu/remove-tag "card"))
(defun ndu/add-card-tag ()
  (interactive)
  (ndu/add-tag "card"))
(defun ndu/remove-outline-tag ()
  (interactive)
  (ndu/remove-tag "card")
  (ndu/remove-tag "outline"))
(defun ndu/cite-insert ()
  (interactive)
  (org-cite-insert nil)
  (ndu/add-card-tag))
(defun ndu/cite-insert-outline ()
  (interactive)
  (org-insert-heading-after-current)
  (org-cite-insert nil)
  (ndu/add-outline-tag)
  (evil-open-below nil))
(defun ndu/cite-insert-card ()
  (interactive)
  (org-insert-heading-after-current)
  (org-cite-insert nil)
  (ndu/add-card-tag)
  (evil-open-below nil))
(defun ndu/add-outline-tag ()
  (interactive)
  (ndu/add-tag "card")
  (ndu/add-tag "outline"))
(defun ndu/add-review-tag ()
  (interactive)
  (ndu/add-tag "review"))
(defun ndu/remove-review-tag ()
  (interactive)
  (ndu/remove-tag "review"))
(defun ndu/align-tags ()
  (interactive)
  (org-set-tags-command '(4))) ; '(4) is the universal argument
(defun ndu/hide-tbmlfm ()
  (interactive)
  (vanish-set-hide 'tblfm t))
(defun ndu/show-tbmlfm ()
  (interactive)
  (vanish-set-hide 'tblfm nil))
(defun ndu/get-remote-range (x y)
  (let ((ret (org-table-get-remote-range x y)))
    (if (listp ret) ret (list ret))))
(defun mapcar* (function &rest args)
  "Apply FUNCTION to successive cars of all ARGS.
Return the list of results."
  ;; If no list is exhausted,
  (if (not (memq nil args))
      ;; apply function to CARs.
      (cons (apply function (mapcar #'car args))
            (apply #'mapcar* function
                   ;; Recurse for rest of elements.
                   (mapcar #'cdr args)))))
(defun ndu/insert-last-stored-link ()
  (require 'org)
  (interactive)
  (insert " ")
  (call-interactively 'org-insert-last-stored-link)
  (kill-line)
  (forward-line -1)
  (end-of-line))
(defun ndu/set-startup-visibility ()
  (interactive)
  (org-set-startup-visibility))
(defun ndu/outline-path ()
  (interactive)
  (string-join (org-get-outline-path t) "/"))
(defun ndu/nov-mode ()
  (use-package nov :mode ("\\.epub\\'" . ndu/nov-mode))
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
  (archive-mode)
  (nov-mode)
  (face-remap-add-relative 'variable-pitch
                           :family "Liberation Serif"
                           :height 1.0))
(defun ndu/save-recompile ()
  (interactive)
  (save-buffer)
  (recompile))
(defun ndu/set-leader (binds)
  (mapc (lambda (x)
          (spacemacs/set-leader-keys (car x) (cadr x)))
        binds))
(defun ndu/set-hooks (dat)
  (mapc (lambda (x)
          (mapc (lambda (y)
                  (add-hook (car x) y))
                (cadr x)))
        dat))
(defun ndu/set-keys (dat global)
  (mapc (lambda (x)
          (let ((key (kbd (car x)))
                (key-function (eval (cadr x))))
            (funcall (if global #'global-unset-key #'local-unset-key)
                     key)
            (funcall (if global #'global-set-key #'local-set-key)
                     key key-function)))
        dat))
(defun ndu/indent-relative-below ()
  (interactive)
  "Indent relative, but works for the line below rather than above"
  (defun ndu/move-line-down ()
    (let ((col (current-column)))
      (save-excursion
        (forward-line)
        (transpose-lines 1))
      (forward-line)
      (move-to-column col)))
  (defun ndu/move-line-up ()
    (let ((col (current-column)))
      (save-excursion
        (forward-line)
        (transpose-lines -1))
      (forward-line -2)
      (move-to-column col)))
  (ndu/move-line-down)
  (indent-relative)
  (ndu/move-line-up))
(defun ndu/buffer-backlinks ()
  (interactive)
  (rg (buffer-name)
      "*.org"
      org-directory))
(defun ndu/entry-backlinks ()
  (interactive)
  (rg (org-entry-get nil "ID") "*.org" org-directory))
(defun ndu/id-link-open-new-window (path _)
  "Open info in a new buffer"
  (setq available-windows
        (delete (selected-window) (window-list)))
  (setq new-window
        (or (car available-windows)
            (split-window-right)
            (split-window-sensibly)))
  (select-window new-window)
  (org-id-open path _)
  (evil-window-move-far-right))
(defun ndu/org-mode ()
  (load "~/.emacs.d/manuallyInstalled/vanish.el")
  (require 'vanish)
  (require 'org-collector)
  (with-eval-after-load 'org
    ;; Allow multiple line Org emphasis markup.
    ;; http://emacs.stackexchange.com/a/13828/115
    (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
    ;; Below is needed to apply the modified `org-emphasis-regexp-components'
    ;; settings from above.
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))
  (add-hook 'org-mode-hook #'ndu/org-syntax-table-modify)
  (add-hook 'org-mode-hook #'electric-pair-mode)
  (add-hook 'org-mode-hook #'electric-indent-mode)
  (add-hook 'org-mode-hook
            (lambda ()
              (turn-off-show-smartparens-mode)))
  (add-hook 'org-mode-hook
            '(lambda ()
               (delete '("\\.pdf\\'" . default) org-file-apps)
               (delete '("\\.png\\'" . default) org-file-apps)
               (add-to-list 'org-file-apps '("\\.pdf\\'" . "open %s"))
               (add-to-list 'org-file-apps '("\\.png\\'" . "open %s"))
               (plist-put org-format-latex-options :scale 2.0)
               (org-link-set-parameters "id" :follow #'ndu/id-link-open-new-window)))
  (use-package org-tidy
    :ensure t
    :config
    (add-hook 'org-mode-hook #'org-tidy-mode))
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
    (ndu/set-hooks '(;(org-mode-hook (turn-on-org-cdlatex))
                     (org-mode-hook (auto-fill-mode))
                     (org-mode-hook (vanish-mode))))
    (setq org-capture-templates
          `(("a" "map" plain (file+headline "~/org/main.org" "Map")
             "  * %?" :prepend t :empty-lines-before 0 :empty-lines-after 0)
            ("s" "tasks" plain (file+headline "~/org/main.org" "Tasks")
             "  * %?" :prepend t :empty-lines-before 0 :empty-lines-after 0)
            ("d" "note" plain (file+headline "~/org/main.org" "Notes")
             "  * %?" :prepend t :empty-lines-before 0 :empty-lines-after 0)
            ("f" "revision" plain (file+headline "~/org/main.org" "Revision")
             "  * %?" :prepend t :empty-lines-before 0 :empty-lines-after 0)))
    (add-hook 'org-capture-after-finalize-hook 'ndu/align-tags)
    (add-hook 'org-mode-hook '(lambda ()
                                (setq-local header-line-format (list '(:eval (substring-no-properties
                                                                              (org-table-get-field)))))))
    (setq-default
     org-table-automatic-realign nil ; less frenetic editing of tables. Align using C-c C-c
     org-emphasis-alist '(("*" bold)
                          ("/" italic)
                          ("_" underline)
                          ("=" (bold :foreground "black" :background "red"))
                          ("~" (bold :foreground "black" :background "orange"))
                          ("+" (bold :foreground "black" :background "green")))
     org-checkbox-hierarchical-statistics nil
     ;; disable confirm message when evaluating stuff for org-babel
     org-confirm-babel-evaluate nil
     org-log-done 'time
     org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "ON-HOLD" "DONE"))
     org-todo-keyword-faces '(("TODO" . (:foreground "purple4" :weight bold))
                              ("IN-PROGRESS" . (:foreground "purple3" :weight bold))
                              ("ON-HOLD" . (:foreground "purple2" :weight bold))
                              ("DONE" . (:foreground "purple1" :weight bold)))
     org-directory "~/org"
     org-agenda-files '("~/org")
     org-priority-faces '((?A . (:foreground "PaleVioletRed"    :weight bold))
                          (?B . (:foreground "orange"           :weight bold))
                          (?C . (:foreground "SeaGreen"         :weight bold))
                          (?D . (:foreground "MediumSeaGreen"   :weight bold))
                          (?E . (:foreground "MediumAquamarine" :weight bold)))))
  (custom-set-faces '(org-checkbox ((t (:foreground "red" :weight bold)))))
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "j" #'org-match-sparse-tree)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "n" #'ndu/previous-match)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "m" #'ndu/next-match))
(defun ndu/latex ()
  (require 'ox-latex)
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (setq org-latex-default-packages-alist (cons '("mathletters" "ucs" nil) org-latex-default-packages-alist))
  (defun add-envs ()
    (LaTeX-add-environments '("IEEEeqnarray" "alignment")
                            '("IEEEeqnarray*" "alignment")))
  ;; support for ieeeeqnarray environment
  ;; http://tex.aspcode.net/view/635399273629833626123752/auctex-how-to-enable-auto-expansion-of-sub-and-superscript-in-custom-math-environment
  ;; note: support for this environment is only partial if auctex sees
  ;; the 'usepackage' macro for it - need following code for full support
  (ndu/set-hooks '((LaTeX-mode-hook ((lambda () (setq evil-shift-width 2))
                                     add-envs))))
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
(defun ndu/ansi-color ()
  "Ansi colors in compilation mode"
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))
(defun ndu/doxymacs ()
  (mapc #'load '("~/.emacs.d/manuallyinstalled/xml-parse.el"
                 "~/.emacs.d/manuallyinstalled/doxymacs.el"))
  (ndu/set-keys '(("C-'"   #'doxymacs-insert-function-comment)
                  ("C-\""  #'doxymacs-insert-file-comment)) nil))
(defun ndu/clojure ()
  (setq-default clojure-enable-fancify-symbols t))
(defun ndu/emacs-lisp ()
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (aggressive-indent-mode t)
              (electric-indent-mode nil)))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (turn-off-show-smartparens-mode)))
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode))
(defun ndu/c-mode ()
  (ndu/set-hooks '((c-mode-hook (doxymacs-mode
                                 (lambda ()
                                   (setq evil-shift-width 4)
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
   '(python
     pdf-tools html osx org git pdf ivy olivetti
     (shell :variables shell-default-shell 'eshell)
     (auto-completion
      :variables spacemacs-default-company-backends '(company-files
                                                      company-capf))
     better-defaults markdown
     syntax-checking
     emacs-lisp ;csv python clojure (latex :variables latex-build-command "LaTeX")
     (spell-checking :variables spell-checking-enable-by-default nil)) ; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.helm-R
   dotspacemacs-additional-packages '(ansi-color
                                      rg org-tidy olivetti
                                      evil-smartparens ; cdlatex latex-extra latex-math-preview
                                      hydra            ;lsp-mode lsp-ui nov
                                      (evil-ediff
                                       :location (recipe :fetcher github :repo "emacs-evil/evil-ediff"))
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
  (add-to-list 'default-frame-alist '(background-color . "#FFFFF0"))
  (set-background-color "#FFFFF0")
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   dotspacemacs-line-numbers nil
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
                                        ; ;dotspacemacs-startup-lists '(recents)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(default)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Iosevka Fixed" ; Good unicode support (monospaced)
                               :size 24
                               :weight thin
                               :width expanded
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
   dotspacemacs-use-ido t
   ;; If non nil, `helm' will try to miminimize the space it uses.
   ;; (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
                                        ;dotspacemacs-helm-position 'bottom
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
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server t
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
  (setq-default line-spacing 40)
  (define-key key-translation-map (kbd "C-<escape>") (kbd "ESC"))
  (define-key key-translation-map (kbd "ESC") (kbd "C-C C-G"))
  (global-set-key (kbd "C-c C-g") 'evil-escape))
(defun dotspacemacs/user-config ()
  "Configuration function for user code.
   This function is called at the very end of Spacemacs initialization after
   layers configuration. You are free to put any user code. "
  (mapc (lambda (x)
          (add-to-list (car x) (cadr x)))
        '((load-path "/opt/homebrew/bin")))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'text-mode-hook (lambda ()
                              (interactive)
                              (message "Olivetti text-mode-hook")
                              (hidden-mode-line-mode)
                              (olivetti-mode 1)
                              (visual-line-mode -1)))
  (add-hook 'prog-mode-hook (lambda ()
                              (interactive)
                              (message "Olivetti prog-mode-hook")
                              (olivetti-mode 1)
                              (visual-line-mode -1)))
  (ndu/set-leader
   '(("om" ndu/hide-tbmlfm)                 ("oM" ndu/show-tbmlfm)
     ("oz" ndu/buffer-backlinks)            ("oZ" ndu/entry-backlinks)
     ("os" ndu/sync-beorg)                  ("oS" org-latex-export-to-pdf)
     ("oh" ndu/cite-insert-card)            ("oH" ndu/cite-insert-outline)
     ("oj" ndu/add-card-tag)                ("oJ" ndu/remove-card-tag)
     ("ol" ndu/add-outline-tag)             ("oL" ndu/remove-outline-tag)
     ("on" tab-bar-switch-to-next-tab)      ("oN" tab-bar-switch-to-prev-tab)
     ("ok" ndu/add-question-tag)            ("oK" ndu/remove-question-tag)
     ("ov" ndu/expand)                      ("oV" ndu/expand-all)
     ("og" ndu/align-tags)                  ("oG" vanish-mode)
     ("oi" ndu/cite-insert)                 ("oI" org-cite-insert)
     ("op" ndu/shrink)                      ("oP" ndu/shrink-all)
     ("oY"  ndu/insert-last-stored-link)    ("oy" org-store-link)
     ("o\\" outline-cycle-buffer)           ("o|" org-set-property)
     ("o["  outline-hide-other)             ("o]" outline-show-subtree)
     ("o{"  ndu/set-startup-visibility)     ("o}" outline-hide-body)
     ("o,"  evil-numbers/dec-at-pt)         ("oo" org-capture)
     ("of"  ndu/toggle-follow-split)        ("oe" ndu/toggle-follow-split-off)
     ("or"  org-edit-src-code)              ("oR" ndu/toggle-indent-mode)
     ("oc"  org-cut-subtree)                ("oC" ndu/toggle-hscroll-mode)))
  (setq-default
   org-tags-column -64
   dotspacemacs-whitespace-cleanup 'all
   dotspacemacs-check-for-update t
   blink-matching-paren nil
   auto-hscroll-mode 'current-line
   evil-move-beyond-eol t
   spacemacs-yank-indent-threshold 0
   pixel-scroll-precision-mode t
   flycheck-python-pycompile-executable "python3"
   python-shell-interpreter "python3"
   auto-save-default t
   vterm-always-compile-module t
   org-use-property-inheritance t
                                        ; nov-text-width 60
   org-id-link-to-org-use-id 'create-if-interactive
   org-adapt-indentation t
   c-default-style "k&r"
   c-basic-offset 4
   find-file-visit-truename t
   org-hide-emphasis-markers t
   org-startup-with-inline-images t
   org-startup-folded 'showall
   org-image-actual-width nil      ; images requre size attribute
   whitespace-line-column 65       ; After 79 chars,
   fill-column            65
   whitespace-style '(face) ;'(face lines-tail) ; highlight columns.
   backup-directory-alist `(("." . "~/.saves")) ; file backups
   flycheck-highlighting-mode 'symbols
   truncate-partial-width-windows nil
   rg-command-line-flags '("--before-context=1")
   flycheck-indication-mode 'left-fringe
   evil-use-y-for-yank t
   maximum-scroll-margin 0.5
   scroll-margin 20
   org-use-property-inheritance t
   org-highest-priority ?A
   show-smartparens-global-mode nil
   org-lowest-priority  ?E
   org-default-priority org-lowest-priority
   git-magit-status-fullscreen t
   c-c++-lsp-enable-semantic-highlight t)
  (custom-set-variables
   '(org-link-frame-setup
     '((vm . vm-visit-folder-other-frame)
       (vm-imap . vm-visit-imap-folder-other-frame)
       (gnus . org-gnus-no-new-news)
       (file . find-file)
       (wl . wl-other-frame))))
  (global-set-key (kbd "M-q") 'fill-paragraph)
  (turn-off-show-smartparens-mode)
  (mapc #'funcall
        #'(spacemacs/toggle-menu-bar-on
           ndu/latex
           spacemacs/toggle-highlight-current-line-globally-off
           global-whitespace-mode
           ndu/org-mode
           tab-bar-mode
           ndu/emacs-lisp))
  ;; Suppress eshell warnings
  (custom-set-variables '(warning-suppress-types '((:warning))))
  (add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))
  (mapc (lambda (hook) (remove-hook 'find-file-hook hook))
        '(vc-refresh-state projectile-find-file-hook-function yas-global-mode-check-buffers
                           global-flycheck-mode-check-buffers undo-tree-load-history-from-hook
                           projectile-find-file-hook-function global-flycheck-mode-check-buffers
                           yas-global-mode-check-buffers undo-tree-load-history-from-hook))
  (global-unset-key (kbd "C-a"))
  (ndu/set-keys '(("C-<"    #'ndu/next-row-edit)
                  ("C->"    #'ndu/next-column-edit)
                  ("C-:"    #'ndu/edit-field)
                  ("C-;"    #'counsel-outline)) t)
  (find-file "~/org/main.org")
  (line-number-mode nil)
  (vanish-set-hide 'tblfm t))
