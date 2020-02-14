;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
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
    (ndu-set-hooks '((org-mode-hook (turn-on-org-cdlatex))
                    (org-after-todo-statistics-hook (org-summary-todo))))
		;; https://orgmode.org/worg/org-contrib/org-drill.html#orgeb853d5
		(setq org-capture-templates
			   `(("n" "note" plain (file+headline "~/org/gtd.org" "Inbox") "- %?")))
    (setq-default
      org-bullets-bullet-list '("■" "◆" "▲" "▶")
      ;; disable confirm message when evaluating stuff for org-babel
      ;org-confirm-babel-evaluate nil
      ;; org-capture
      ;;http://orgmode.org/manual/Setting-up-capture.html
      ;; If you would like a TODO entry to automatically change to DONE when all
      ;; children are done, you can use the following setup.
      ;; (http://orgmode.org/manual/Breaking-down-tasks.html)
      org-log-done 'time
      org-directory "~/org"
      org-agenda-files '("~/org/gtd.org")
      ;; MobileOrg iphone app
      ;; http://mobileorg.ncogni.to/doc/getting-started/using-dropbox/
      ;; Set to the location of your Org files on your local system
      ;; Set to the name of the file where new notes will be stored
      ;org-mobile-inbox-for-pull "~/org/in.org"
      ;org-mobile-directory "~/windoze/Dropbox/Apps/MobileOrg"
      )))
(defun ndu-latex ()
  (defun add-envs ()
    (LaTeX-add-environments '("IEEEeqnarray" "alignment")
                            '("IEEEeqnarray*" "alignment")))
  ;; support for ieeeeqnarray environment
  ;; http://tex.aspcode.net/view/635399273629833626123752/auctex-how-to-enable-auto-expansion-of-sub-and-superscript-in-custom-math-environment
  ;; note: support for this environment is only partial if auctex sees
  ;; the 'usepackage' macro for it - need following code for full support
  (ndu-set-hooks '((LaTeX-mode-hook ((lambda () (setq evil-shift-width 2))
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
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode))
(defun ndu-c-mode ()
  (ndu-set-hooks '((c-mode-hook (helm-gtags-mode doxymacs-mode
                                 (lambda ()
                                   (setq evil-shift-width 4)
                                   (ggtags-mode 1)
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
	'(helm org git shell pdf-tools auto-completion better-defaults markdown
	  syntax-checking semantic gtags java c-c++ emacs-lisp
      themes-megapack csv python ess clojure scheme octave
      (latex :variables latex-build-command "LaTeX")
	  (spell-checking :variables spell-checking-enable-by-default nil))
    ; List of additional packages that will be installed without being
    ;; wrapped in a layer. If you need some configuration for these
    ;; packages then consider to create a layer, you can also put the
    ;; configuration in `dotspacemacs/config'.
    dotspacemacs-additional-packages '(chronos ansi-color anki-editor
                                       evil-smartparens cdlatex helm-R
                                       latex-extra latex-math-preview
                                       wordnut adaptive-wrap matlab-mode)
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
    dotspacemacs-startup-lists '(recents projects)
    ;; List of themes, the first of the list is loaded when spacemacs starts.
    ;; Press <SPC> T n to cycle to the next theme in the list (works great
    ;; with 2 themes variants, one dark and one light)
    dotspacemacs-themes '(sanityinc-tomorrow-day)
    ;; If non nil the cursor color matches the state color.
    dotspacemacs-colorize-cursor-according-to-state t
    ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
    ;; size to make separators look not too crappy.
    dotspacemacs-default-font '("Courier New"
                                :size 24
                                :weight normal
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
  (define-key key-translation-map (kbd "C-<escape>") (kbd "ESC"))
  (define-key key-translation-map (kbd "ESC") (kbd "C-C C-G"))
  (global-set-key (kbd "C-c C-g") 'evil-escape))
(defun dotspacemacs/user-config ()
  "Configuration function for user code.
   This function is called at the very end of Spacemacs initialization after
   layers configuration. You are free to put any user code. "
  (with-eval-after-load 'company
    (add-to-list 'company-backends '(company-capf company-dabbrev)))
  (mapc (lambda (x)
          (add-to-list (car x) (cadr x)))
        '((load-path "/usr/share/wordnet")
          (evil-emacs-state-modes wordnut-mode)))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode)
  (global-visual-line-mode t)
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
  (setq-default
    dotspacemacs-whitespace-cleanup 'all
    dotspacemacs-check-for-update t
    python-shell-interpreter "python3"
    auto-save-default t
    whitespace-line-column 80                    ; After 79 chars,
    whitespace-style '(face lines-tail)          ; highlight columns.
    line-spacing 16                              ; space between lines
    backup-directory-alist `(("." . "~/.saves")) ; file backups
    flycheck-highlighting-mode 'symbols
    flycheck-indication-mode 'left-fringe
    evil-use-y-for-yank t)
  (ndu-set-keys '(("C->"  #'indent-relative)
                  ("C-<"  #'ndu-indent-relative-below)
                  ("C-\'" #'ndu-save-recompile))
                t)
  (mapc #'funcall
        #'(spacemacs/toggle-menu-bar-on
           spacemacs/toggle-highlight-current-line-globally-off
           global-whitespace-mode
           global-flycheck-mode
           global-company-mode
           ndu-chronos
           ndu-org-mode
           ndu-latex
           ndu-ansi-color
           ndu-doxymacs
           ndu-taskjuggler
           ndu-clojure
           ndu-emacs-lisp
           ndu-c-mode))
  (find-file "~/org/gtd.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (anki-editor zenburn-theme zen-and-art-theme yapfify xterm-color ws-butler wordnut winum white-sand-theme which-key volatile-highlights vi-tilde-fringe uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stickyfunc-enhance srefactor spaceline spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle shell-pop seti-theme reverse-theme restart-emacs rebecca-theme rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pdf-tools pcre2el paradox orgit organic-green-theme org-present org-pomodoro org-mime org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme neotree naquadah-theme mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme matlab-mode material-theme markdown-toc majapahit-theme magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum live-py-mode linum-relative link-hint light-soap-theme latex-math-preview latex-extra jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag helm-R hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md ggtags geiser gandalf-theme fuzzy flyspell-correct-helm flycheck-pos-tip flx-ido flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-smartparens evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu ess-smart-equals ess-R-data-view espresso-theme eshell-z eshell-prompt-extras esh-help elisp-slime-nav dumb-jump dracula-theme django-theme disaster diminish define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode company-statistics company-emacs-eclim company-c-headers company-auctex company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized cmake-mode clues-theme clojure-snippets clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu chronos cherry-blossom-theme cdlatex busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
