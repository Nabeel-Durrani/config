;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
(defun ndu/priority (pri)
  (setq pri-map '(("A" . "\"A\"")
                  ("B" . "\"A\"\|PRIORITY=\"B\"")
                  ("C" . "\"A\"\|PRIORITY=\"B\"\|PRIORITY=\"C\"")
                  ("D" . "\"A\"\|PRIORITY=\"B\"\|PRIORITY=\"C\"\|PRIORITY=\"D\"")
                  ("E" . "\"A\"\|PRIORITY=\"B\"\|PRIORITY=\"C\"\|PRIORITY=\"D\"\|PRIORITY=\"E\"")))
  (cdr (assoc pri pri-map)))
(defun ndu/insert-topic-item-capture (type)
  (concat "** " type (format-time-string "-%Y-%m-%d-%H:%M:%S")))
(defun ndu/insert-link-capture ()
 (concat "** " (read-from-minibuffer "Description: ") "\n"
         "   :PROPERTIES:\n"
         "   :URL: %?\n"
         "   :URL-DATE: " (format-time-string "%Y-%m-%d-%H:%M:%S\n")
         "   :END:"))
(defun ndu/insert-link ()
  (interactive)
  (setq description (read-from-minibuffer "Description: ")
        url         (read-from-minibuffer "URL: "))
  (org-insert-heading)
  (setq spaces (make-string (current-column) ?\s))
  (insert description)
  (newline-and-indent)
  (insert (concat        ":PROPERTIES:\n"
                  spaces ":URL: " url "\n"
                  spaces ":URL-DATE: " (format-time-string "%Y-%m-%d-%H:%M:%S\n")
                  spaces ":END:\n")))
(defun ndu/insert-item ()
  (interactive)
  (org-insert-heading)
  (insert (format-time-string "I-%Y-%m-%d-%H:%M:%S"))
  (org-set-tags "drill:item"))
(defun ndu/insert-topic ()
  (interactive)
  (org-insert-heading)
  (insert (format-time-string "T-%Y-%m-%d-%H:%M:%S"))
  (org-set-tags "drill:topic"))
(defun ndu/get-tag-priority-match ()
  (setq priority (read-from-minibuffer "Priority (A-E):")
        pri      (concat "+PRIORITY=" (ndu/priority priority))
        tag      (read-from-minibuffer "+tag_1...+tag_N: ")
        pri-tag  (concat pri tag)
        pri-pred (if (member priority '("A" "B" "C" "D" "E")) t nil)
        tag-pred (> (length tag) 0)
        out-map  '(((nil nil) . nil)
                   ((nil t)   . tag)
                   ((t   nil) . pri )
                   ((t   t)   . pri-tag)))
  (eval (cdr (assoc (list pri-pred tag-pred) out-map))))
(defun ndu/get-item-match ()
  (setq input    (read-from-minibuffer "Priority (A-E):")
        priority (if (member input '("A" "B" "C" "D" "E"))
                     (concat "+PRIORITY=" (ndu/priority input))
                     "")
        todo     "+item")
  (concat todo priority))
(defun ndu/get-topic-match ()
  (setq input    (read-from-minibuffer "Priority (A-E):")
        priority (if (member input '("A" "B" "C" "D" "E"))
                     (concat "+PRIORITY=" (ndu/priority input))
                     "")
        todo     "+topic")
  (concat todo priority))
(defun ndu/get-todo-match ()
  (setq input    (read-from-minibuffer "Priority (A-E):")
        priority (if (member input '("A" "B" "C" "D" "E"))
                     (concat "+PRIORITY="
                             "\"" input "\"")
                     "")
        todo     "+TODO=\"TODO\"")
  (concat todo priority))
(defun ndu/org-drill ()
 (require 'org-drill)
 (interactive)
 (org-drill nil (ndu/get-tag-priority-match)))
(defun ndu/org-cram ()
 (require 'org-drill)
 (interactive)
 (org-drill-cram nil (ndu/get-tag-priority-match)))
(defun ndu/org-drill-tree ()
 (require 'org-drill)
 (interactive)
 (org-drill 'tree (ndu/get-tag-priority-match)))
(defun ndu/org-cram-tree ()
 (require 'org-drill)
 (interactive)
 (org-drill-cram 'tree (ndu/get-tag-priority-match)))
(defun ndu/org-drill-topic ()
 (require 'org-drill)
 (interactive)
 (org-drill nil (ndu/get-topic-match)))
(defun ndu/org-drill-item ()
 (require 'org-drill)
 (interactive)
 (org-drill nil (ndu/get-item-match)))
(defun ndu/org-cram-topic ()
 (require 'org-drill)
 (interactive)
 (org-drill-cram nil (ndu/get-topic-match)))
(defun ndu/org-cram-item ()
 (require 'org-drill)
 (interactive)
 (org-drill-cram nil (ndu/get-item-match)))
(defun ndu/org-drill-todo ()
  (require 'org-drill)
  (interactive)
  (org-drill nil (ndu/get-todo-match)))
(defun ndu/org-drill-todo-tree ()
  (require 'org-drill)
  (interactive)
  (org-drill 'tree (ndu/get-todo-match)))
(defun ndu/org-drill-topic-tree ()
  (require 'org-drill)
  (interactive)
  (org-drill 'tree (ndu/get-topic-match)))
(defun ndu/org-drill-item-tree ()
  (require 'org-drill)
  (interactive)
  (org-drill 'tree (ndu/get-item-match)))
(defun ndu/org-cram-topic-tree ()
  (require 'org-drill)
  (interactive)
  (org-drill-cram 'tree (ndu/get-topic-match)))
(defun ndu/org-cram-item-tree ()
  (require 'org-drill)
  (interactive)
  (org-drill-cram 'tree (ndu/get-item-match)))
(defun ndu/org-cram-todo ()
  (require 'org-drill)
  (interactive)
  (org-drill-cram nil (ndu/get-todo-match)))
(defun ndu/org-cram-todo-tree ()
  (require 'org-drill)
  (interactive)
  (org-drill-cram 'tree (ndu/get-todo-match)))
(defun ndu/insert-progress-buffer ()
  (interactive)
  (org-map-entries 'ndu/insert-progress-tree "-TODO=\"TODO\"-TODO=\"DONE\""))
(defun ndu/insert-progress-tree ()
  (interactive)
  (setq complete   (length (org-map-entries t "+TODO=\"DONE\"" 'tree))
        incomplete (length (org-map-entries t "+TODO=\"TODO\"" 'tree))
        total      (+ complete incomplete)
        progress   (if (> total 0)
                       (* (/ complete (float total)) 100)
                       0))
  (org-entry-put (point) "COMPLETE" (format "%d / %d" complete total))
  (org-entry-put (point) "PROGRESS" (format "%d%%" progress))
  (org-entry-put (point)
                 "LAST-UPDATE" (format-time-string "%Y-%m-%d-%H:%M:%S")))
;; https://github.com/telotortium/emacs-od2ae/blob/main/od2ae.el
(defun ndu/convert-cloze ()
  "Convert an org-drill Cloze card to one compatible with anki-editor."
  (require 'anki-editor)
  (require 'org-drill)
  (interactive)
  (atomic-change-group
    (org-with-point-at (point)
      (org-back-to-heading)
      (org-narrow-to-subtree)
      (save-excursion
        (let* ((cloze-count 1)
               (beginning-marker (make-marker))
               (end-marker (make-marker))
               cloze-beginning)
          (save-excursion
            (while (re-search-forward org-drill-cloze-regexp nil t)
              (let ((hint (match-string-no-properties 2)))
                (unless (string-blank-p hint)
                  ;; Strip leading hint separator
                  (setq hint (substring hint
                                        (length org-drill-hint-separator)
                                        (length hint)))
                  ;; Delete hint (with separator)
                  (delete-region (match-beginning 2)
                                 (match-end 2))
                  ;; Move before matched region and retry.
                  (goto-char (match-beginning 0))
                  (forward-char -1)
                  (re-search-forward org-drill-cloze-regexp))
                (setq cloze-beginning
                      (+ (match-beginning 0)
                         (length org-drill-left-cloze-delimiter)))
                (set-marker beginning-marker cloze-beginning)
                (set-marker end-marker (match-end 2))
                (delete-region (match-beginning 3) (match-end 3))
                (delete-region (match-beginning 0) cloze-beginning)
                (anki-editor-cloze
                 (marker-position beginning-marker)
                 (marker-position end-marker)
                 cloze-count
                 hint)
                (setq cloze-count (+ 1 cloze-count))))))))))
(defun ndu/open-externally()
 (interactive)
 (shell-command (format (concat "open " (browse-url-url-at-point)))))
(defun ndu/org-screenshot-anki ()
  (interactive)
  (setq filename-short
        (concat (make-temp-name (format-time-string "%Y%m%d%H%M%S")) ".png"))
  (setq filename (concat "anki_imgs/" filename-short))
  (ndu/org-screenshot filename-short filename))
(defun ndu/org-screenshot-regular ()
  (interactive)
  (setq filename-short
        (concat (make-temp-name (format-time-string "%Y%m%d%H%M%S")) ".png"))
  (setq filename
        ;; note: capture buffer has buffer name null
        (concat (file-name-nondirectory (if (buffer-file-name)
                                            (buffer-file-name) "misc"))
                "_imgs/"
                filename-short))
  (ndu/org-screenshot filename-short filename)
  (org-display-inline-images))
;; https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
(defun ndu/org-screenshot (filename-short filename)
  "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
  (interactive)
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))

  (setq spaces (make-string (+ (current-column) 1) ?\s))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat " #+NAME: " filename-short "\n"
                       spaces "#+CAPTION: "
                       (format-time-string "%F" (current-time)) "\n"
                       spaces "#+ATTR_ORG: :width 400\n"
                       spaces "[[file:" filename "]]"))))
(defun ndu/cloze-region-auto-incr (&optional arg)
  "Cloze region without hint and increase card number."
  (require 'anki-editor)
  (interactive)
  (anki-editor-cloze-region my-anki-editor-cloze-number "")
  (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
  (forward-sexp))
(defun ndu/cloze-region-dont-incr (&optional arg)
  "Cloze region without hint using the previous card number."
  (require 'anki-editor)
  (interactive)
  (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
  (forward-sexp))
(defun ndu/reset-cloze-number (&optional arg)
  "Reset cloze number to ARG or 1"
  (require 'anki-editor)
  (interactive)
  (setq my-anki-editor-cloze-number (or arg 1)))
(defun ndu/push-notes ()
  (require 'anki-editor)
  (interactive)
  ;(anki-editor-push-notes '(4)) ; would push all notes under a tree
  (anki-editor-push-notes)
  (ndu/reset-cloze-number))
; https://abizjak.github.io/emacs/2016/03/06/latex-fill-paragraph.html
(defun ndu/fill-paragraph (&optional P)
  "When called with prefix argument call `fill-paragraph'.
Otherwise split the current paragraph into one sentence per line."
  (interactive "P")
  (if (not P)
      (save-excursion
        (let ((fill-column 12345678)) ; relies on dynamic binding
          (fill-paragraph) ; this will not work correctly if the paragraph is
                           ; longer than 12345678 characters (in which case the
                           ; file must be at least 12MB long. This is   unlikely.)
          (let ((end (save-excursion
                       (forward-paragraph 1)
                       (backward-sentence)
                       (point-marker))))  ;; remember where to stop
            (beginning-of-line)
            (while (progn (forward-sentence)
                          (<= (point) (marker-position end)))
              (just-one-space) ; leaves only one space, point is after it
              (delete-char -1) ; delete the space
              ;; and insert a newline
              (newline)
              (indent-relative)))))
    (fill-paragraph P))) ; otherwise do ordinary fill paragraph
(defun my/play-sound (orgin-fn sound)
  (cl-destructuring-bind (_ _ file) sound
    (make-process :name (concat "play-sound-" file)
                  :connection-type 'pipe
                  :command `("afplay" ,file))))
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
(defun ndu/view-tag (str)
  "Concat STR with @6-months-ago +unread."
  (concat "@6-months-ago +unread " str))
(defun ndu/eww-open (&optional use-generic-p)
  "elfeed open with eww"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (eww-browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))
(defun ndu/nov-mode ()
  (archive-mode)
  (nov-mode)
  (face-remap-add-relative 'variable-pitch
                           :family "Liberation Serif"
                           :height 1.0)
  (olivetti-mode))
(defun ndu/elfeed-mode ()
  (require 'elfeed)
  (define-key elfeed-search-mode-map (kbd "m") 'ndu/eww-open)
  (defhydra ap/elfeed-search-view (elfeed-search-mode-map "d" :color blue) ; press d + (h, j, etc)
    "Set elfeed-search filter tags."
    ("h" (elfeed-search-set-filter nil) "Default")
    ("j" (elfeed-search-set-filter (ndu/view-tag "+left")) "left")
    ("k" (elfeed-search-set-filter (ndu/view-tag "+news")) "news")
    ("l" (elfeed-search-set-filter (ndu/view-tag "+foreign")) "foreign")
    ("b" (elfeed-search-set-filter (ndu/view-tag "+opinion")) "opinion")
    ("n" (elfeed-search-set-filter (ndu/view-tag "+tech")) "tech"))
  (ndu/set-hooks '((elfeed-search-mode-hook (olivetti-mode))
                   (elfeed-show-mode-hook (olivetti-mode))
                   (eww-mode-hook (olivetti-mode))
                   (eww-mode-hook (writeroom-mode))))
  (add-hook 'eww-after-render-hook 'eww-readable))
(defun ndu/save-recompile () (interactive) (save-buffer) (recompile))
(defun ndu/set-leader (package binds)
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
  (rg (buffer-name) "*.org" org-directory))
(defun ndu/entry-backlinks ()
  (interactive)
  (rg (org-entry-get nil "ID") "*.org" org-directory))
(defun ndu/org-mode ()
  (add-hook 'org-mode-hook
   '(lambda ()
     (delete '("\\.pdf\\'" . default) org-file-apps)
     (delete '("\\.png\\'" . default) org-file-apps)
     (add-to-list 'org-file-apps '("\\.pdf\\'" . "open %s"))
     (add-to-list 'org-file-apps '("\\.png\\'" . "open %s"))
     (plist-put org-format-latex-options :scale 2.0)))
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
    (ndu/set-hooks '((org-mode-hook (turn-on-org-cdlatex))
                     (org-mode-hook (auto-complete-mode))
                     (org-mode-hook (olivetti-mode))
                     (org-mode-hook (writeroom-mode))
                     (org-mode-hook
                      ((lambda ()
                         (push '("[ ]" .  "☐") prettify-symbols-alist)
                         (push '("[X]" . "☑" ) prettify-symbols-alist)
                         (push '("-" . "—" ) prettify-symbols-alist)
                         (push '("::" . "⁚" ) prettify-symbols-alist)
                         (prettify-symbols-mode))))
                     (org-after-todo-statistics-hook (org-summary-todo))))
		;; https://orgmode.org/worg/org-contrib/org-drill.html#orgeb853d5
		(setq org-capture-templates
          `(("q" "note" plain (file+headline "~/org/gtd.org" "Notes")
             "  * %?" :prepend t)
            ("w" "link" plain (file+headline "~/org/gtd.org" "Links")
              "%(ndu/insert-link-capture)"
             :prepend t)
            ("e" "topic" plain (file+headline "~/org/gtd.org" "Inbox")
             "%(ndu/insert-topic-item-capture \"T\")%(org-set-tags \"drill:topic\")\n   %?"
             :prepend t)
            ("r" "item" plain (file+headline "~/org/gtd.org" "Inbox")
             "%(ndu/insert-topic-item-capture \"I\")%(org-set-tags \"drill:item\")\n   %?"
             :prepend t)
            ("t" "todo" plain (file+headline "~/org/gtd.org" "Inbox")
                        "** TODO %?" :prepend t)
            ("a" "anki-low" plain
             (file+headline "~/org/anki.org" "Priority Low")
             "*** Note %T\n    :PROPERTIES:\n    :ANKI_NOTE_TYPE: tts_cloze\n    :ANKI_DECK: main\n    :ANKI_TAGS: priority_1 priority_2 priority_3 priority_4 tts\n    :END:\n***** text\n      %?\n***** index\n      %<%s>\n***** extra\n      "
             :prepend t)
            ("s" "anki-medium" plain
             (file+headline "~/org/anki.org" "Priority Medium")
             "*** Note %T\n    :PROPERTIES:\n    :ANKI_NOTE_TYPE: tts_cloze\n    :ANKI_DECK: main\n    :ANKI_TAGS: priority_2 priority_3 priority_4 tts\n    :END:\n***** text\n      %?\n***** index\n      %<%s>\n***** extra\n      "
             :prepend t)
            ("d" "anki-high" plain
             (file+headline "~/org/anki.org" "Priority High")
             "*** Note %T\n    :PROPERTIES:\n    :ANKI_NOTE_TYPE: tts_cloze\n    :ANKI_DECK: main\n    :ANKI_TAGS: priority_3 priority_4 tts\n    :END:\n***** text\n      %?\n***** index\n      %<%s>\n***** extra\n      "
             :prepend t)
            ("f" "anki-highest" plain
             (file+headline "~/org/anki.org" "Priority Highest")
             "*** Note %T\n    :PROPERTIES:\n    :ANKI_NOTE_TYPE: tts_cloze\n    :ANKI_DECK: main\n    :ANKI_TAGS: priority_4 tts\n    :END:\n***** text\n      %?\n***** index\n      %<%s>\n***** extra\n      "
             :prepend t)
            ("z" "anki-occlusion-low" plain
             (file+headline "~/org/anki.org" "Priority Low")
             "*** Note %T\n    :PROPERTIES:\n    :ANKI_NOTE_TYPE: image_occlusion\n    :ANKI_DECK: main\n    :ANKI_TAGS: priority_1 priority_2 priority_3 priority_4 occlusion\n    :END:\n***** unoccluded\n      \n***** index\n      %<%s>\n***** extra\n      %?\n***** occlusion1\n      #+BEGIN_EXPORT html\n      #+END_EXPORT\n"
             :prepend t)
            ("x" "anki-occlusion-medium" plain
             (file+headline "~/org/anki.org" "Priority Medium")
             "*** Note %T\n    :PROPERTIES:\n    :ANKI_NOTE_TYPE: image_occlusion\n    :ANKI_DECK: main\n    :ANKI_TAGS: priority_2 priority_3 priority_4 occlusion\n    :END:\n***** unoccluded\n      \n***** index\n      %<%s>\n***** extra\n      %?\n***** occlusion1\n      #+BEGIN_EXPORT html\n      #+END_EXPORT\n"
             :prepend t)
            ("c" "anki-occlusion-high" plain
             (file+headline "~/org/anki.org" "Priority High")
             "*** Note %T\n    :PROPERTIES:\n    :ANKI_NOTE_TYPE: tts_cloze\n    :ANKI_DECK: main\n    :ANKI_TAGS: priority_3 priority_4 occlusion\n    :END:\n***** unoccluded\n      \n***** index\n      %<%s>\n***** extra\n      %?\n***** occlusion1\n      #+BEGIN_EXPORT html\n      #+END_EXPORT\n"
             :prepend t)
            ("v" "anki-occlusion-highest" plain
             (file+headline "~/org/anki.org" "Priority Highest")
             "*** Note %T\n    :PROPERTIES:\n    :ANKI_NOTE_TYPE: tts_cloze\n    :ANKI_DECK: main\n    :ANKI_TAGS: priority_4 occlusion\n    :END:\n***** unoccluded\n      \n***** index\n      %<%s>\n***** extra\n      %?\n***** occlusion1\n      #+BEGIN_EXPORT html\n      #+END_EXPORT\n"
             :prepend t)))
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
     ;; http://orgmode.org/manual/Setting-up-capture.html
     ;; If you would like a TODO entry to automatically change to DONE when all
      ;; children are done, you can use the following setup.
     ;; (http://orgmode.org/manual/Breaking-down-tasks.html)
      org-log-done 'time
      org-todo-keyword-faces '(("TODO" . (:foreground "purple4" :weight bold))
                               ("DONE" . (:foreground "purple1" :weight bold)))
      org-directory "~/org"
      org-agenda-files '("~/org")
      org-drill-add-random-noise-to-intervals-p t
      org-drill-spaced-repetition-algorithm 'sm2
      org-drill-hide-item-headings-p t       ; so priorities not clozed
      org-drill-leech-method nil             ; for reading text
      org-drill-forgetting-index 100         ; for reading text
      org-drill-leech-failure-threshold nil  ; for reading text
      org-drill-scope 'directory
      ;; MobileOrg iphone app
      ;; http://mobileorg.ncogni.to/doc/getting-started/using-dropbox/
      ;; Set to the location of your Org files on your local system
      ;; Set to the name of the file where new notes will be stored
      ; org-mobile-inbox-for-pull "~/org/in.org"
      ; org-mobile-directory "~/windoze/Dropbox/Apps/MobileOrg"
      org-priority-faces '((?A . (:foreground "PaleVioletRed"    :weight bold))
                           (?B . (:foreground "orange"           :weight bold))
                           (?C . (:foreground "SeaGreen"         :weight bold))
                           (?D . (:foreground "MediumSeaGreen"   :weight bold))
                           (?E . (:foreground "MediumAquamarine" :weight bold)))))
  (custom-set-faces '(org-checkbox ((t (:foreground "red" :weight bold)))))
  (org-copy-face 'org-todo 'org-checkbox-statistics-todo
                 "Face used for unfinished checkbox statistics."))
(defun ndu/latex ()
  (defun add-envs ()
    (LaTeX-add-environments '("IEEEeqnarray" "alignment")
                            '("IEEEeqnarray*" "alignment")))
  ;; support for ieeeeqnarray environment
  ;; http://tex.aspcode.net/view/635399273629833626123752/auctex-how-to-enable-auto-expansion-of-sub-and-superscript-in-custom-math-environment
  ;; note: support for this environment is only partial if auctex sees
  ;; the 'usepackage' macro for it - need following code for full support
  (ndu/set-hooks '((LaTeX-mode-hook ((lambda () (setq evil-shift-width 2))
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
(defun ndu/ansi-color ()
  "Ansi colors in compilation mode"
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))
(defun ndu/doxymacs ()
  ;(mapc #'load '("~/.emacs.d/manuallyinstalled/xml-parse.el"
  ;              "~/.emacs.d/manuallyinstalled/doxymacs.el"))
  (ndu/set-keys '(("C-'"   #'doxymacs-insert-function-comment)
                  ("C-\""  #'doxymacs-insert-file-comment))
                nil))
(defun ndu/taskjuggler ()
   (load "~/.emacs.d/manuallyInstalled/taskjuggler-mode.el"))
(defun ndu/clojure ()
  (setq-default clojure-enable-fancify-symbols t))
(defun ndu/emacs-lisp ()
  (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode))
(defun ndu/c-mode () ;;helm-gtags-mode
  (ndu/set-hooks '((c-mode-hook (doxymacs-mode
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
    '(html osx org git pdf
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
      (elfeed :variables rmh-elfeed-org-files (list "~/org/elfeed/feeds.org"))
	    (spell-checking :variables spell-checking-enable-by-default nil))
    ; List of additional packages that will be installed without being
    ;; wrapped in a layer. If you need some configuration for these
    ;; packages then consider to create a layer, you can also put the
    ;; configuration in `dotspacemacs/config'.helm-R
    dotspacemacs-additional-packages '(ansi-color anki-editor rg
                                       org-drill adaptive-wrap
                                       evil-smartparens cdlatex vterm
                                       latex-extra latex-math-preview
                                       wordnut matlab-mode
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
    dotspacemacs-themes '(default)
    ;; If non nil the cursor color matches the state color.
    dotspacemacs-colorize-cursor-according-to-state t
    ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
    ;; size to make separators look not too crappy.
    dotspacemacs-default-font '("Iosevka Fixed" ; Good unicode support (monospaced)
                                :size 28
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
  ; https://yiufung.net/post/anki-org/
  (use-package anki-editor
      :after org
      :bind (:map org-mode-map
                  ("<f12>" . ndu/cloze-region-auto-incr)
                  ("<f11>" . ndu/cloze-region-dont-incr)
                  ("<f10>" . ndu/reset-cloze-number)
                  ("<f9>"  . ndu/push-notes))
      :hook (org-capture-after-finalize . ndu/reset-cloze-number) ; Reset cloze-number after each capture.
      :config
      ; Allow anki-editor to create a new deck if it doesn't exist
      (setq anki-editor-create-decks t
            anki-editor-org-tags-as-anki-tags t)
      ;; Initialize
      (ndu/reset-cloze-number))
  (mapc (lambda (x)
          (add-to-list (car x) (cadr x)))
        '((load-path "/opt/homebrew/bin")
          (evil-emacs-state-modes wordnut-mode)))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode)
  (use-package nov
    :mode ("\\.epub\\'" . ndu/nov-mode))
  ;(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (ndu/set-leader
    'anki-editor
    '(("on" ndu/insert-topic)                ("om" ndu/insert-item)
      ("oz" ndu/buffer-backlinks)            ("ox" ndu/entry-backlinks)
      ("oc" ndu/open-externally)             ("ov" ndu/convert-cloze)
      ("oq" ndu/org-drill-todo)              ("ow" ndu/org-cram-todo)
      ("oe" ndu/org-drill-todo-tree)         ("or" ndu/org-cram-todo-tree)
      ("oh" ndu/org-drill-topic)             ("oj" ndu/org-drill-item)
      ("ok" ndu/org-drill-topic-tree)        ("ol" ndu/org-drill-item-tree)
      ("oH" ndu/org-cram-topic)              ("oJ" ndu/org-cram-item)
      ("oK" ndu/org-cram-topic-tree)         ("oL" ndu/org-cram-item-tree)
      ("oa" ndu/org-drill)                   ("os" ndu/org-cram)
      ("od" ndu/org-drill-tree)              ("of" ndu/org-cram-tree)
      ("oz" ndu/cloze-region-auto-incr)      ("ox" ndu/cloze-region-dont-incr)
      ("oc" anki-editor-latex-region)        ("ot" ndu/insert-progress-tree)
      ("og" ndu/org-screenshot-anki)         ("ob" ndu/insert-progress-buffer)
      ("ou" anki-editor-retry-failure-notes) ("oi" ndu/push-notes)
      ("oo" org-capture)                     ("op" ndu/org-screenshot-regular)
      ("oY" org-insert-last-stored-link)     ("oy" org-store-link)
      ("o\\" outline-cycle-buffer)           ("o|" org-set-property)
      ("o["  outline-hide-other)             ("o]" outline-show-subtree)
      ("o{"  outline-show-all)               ("o}" outline-hide-body)
      ("o;"  org-drill-resume)               ("o'" org-drill-again)))
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
  (advice-add 'play-sound :around 'my/play-sound)
  ; (load "~/.emacs.d/manuallyInstalled/ed-mode.el")
  (setq-default
    org-clock-sound "~/.emacs.d/manuallyInstalled/bell.wav"
    org-timer-default-timer "0:25:00"
    dotspacemacs-whitespace-cleanup 'all
    dotspacemacs-check-for-update t
    spacemacs-yank-indent-threshold 0
    pixel-scroll-precision-mode t
    flycheck-python-pycompile-executable "python3"
    python-shell-interpreter "python3"
    auto-save-default t
    visual-fill-column-center-text t
    vterm-always-compile-module t
    olivetti-style 'fancy
    olivetti-body-width 86
    writeroom-width 86
    org-use-property-inheritance t
    olivetti-margin-width 5
    nov-text-width 60
    org-id-link-to-org-use-id 'create-if-interactive
    org-adapt-indentation t
    ; org-odd-levels-only t
    c-default-style "k&r"
    c-basic-offset 4
    org-hide-emphasis-markers t
    org-startup-with-inline-images t
    org-image-actual-width nil ; images requre size attribute
    whitespace-line-column 80                     ; After 79 chars,
    whitespace-style '(face) ;'(face lines-tail) ; highlight columns.
    backup-directory-alist `(("." . "~/.saves")) ; file backups
    flycheck-highlighting-mode 'symbols
    flycheck-indication-mode 'left-fringe
    evil-use-y-for-yank t
    org-use-property-inheritance t
    org-highest-priority ?A
    org-lowest-priority  ?E
    org-default-priority org-lowest-priority
    git-magit-status-fullscreen t
    helm-full-frame t
    c-c++-lsp-enable-semantic-highlight t)
  (ndu/set-keys '(("C->"  #'indent-relative)
                  ("C-<"  #'ndu/indent-relative-below)
                  ("C-\'" #'ndu/save-recompile))
                t)
  (ndu/set-hooks '((python-mode-hook (olivetti-mode))
                   (c-mode-hook (olivetti-mode))
                   (emacs-lisp-mode-hook (olivetti-mode))
                   (sh-mode-hook (olivetti-mode))
                   (c++-mode-hook (olivetti-mode))
                   (c++-mode-hook (lsp))))
  (global-visual-line-mode t)
  (evil-define-minor-mode-key 'motion 'visual-line-mode "$" 'evil-end-of-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode "^" 'evil-first-non-blank-of-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
  (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
  ;; Adaptive wrap anyways needs the `visual-line-mode' to be enabled. So
  ;; enable it only when the latter is enabled.
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  (global-set-key (kbd "M-q") 'ndu/fill-paragraph)
  (mapc #'funcall
        #'(spacemacs/toggle-menu-bar-on
           spacemacs/toggle-highlight-current-line-globally-off
           global-whitespace-mode
           global-flycheck-mode
           ; global-auto-complete-mode
           ndu/org-mode
           ndu/latex
           ndu/ansi-color
           ndu/doxymacs
           ; ndu/taskjuggler
           ndu/clojure
           ndu/emacs-lisp
           ndu/c-mode
           ndu/elfeed-mode))
  (custom-set-variables '(warning-suppress-types '((:warning))))
  (eshell)
  (find-file "~/org/gtd.org"))
