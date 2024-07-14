(defun ndu/anki-editor ()
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
        (ndu/reset-cloze-number)))
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
  (if (org-at-table-p)
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
  (ndu/shrink)
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
  (spacemacs/toggle-truncate-lines-on)
  (org-table-expand)
  (ndu/show-tbmlfm))
(defun ndu/expand-all ()
  (interactive)
  (spacemacs/toggle-truncate-lines-on)
  (org-table-map-tables #'org-table-expand t)
  (org-table-map-tables #'org-table-align t)
  (ndu/show-tbmlfm))
(defun ndu/shrink ()
  (interactive)
  (spacemacs/toggle-truncate-lines-off)
  (ndu/hide-tbmlfm)
  (org-table-shrink))
(defun ndu/shrink-all ()
  (interactive)
  (spacemacs/toggle-truncate-lines-off)
  (org-table-map-tables #'org-table-shrink t)
  (org-table-map-tables #'org-table-align t)
  (ndu/hide-tbmlfm))
(defun ndu/set-confidence ()
  (interactive)
  (let ((confidence (ndu/sum-str-to-num (list (org-table-get-remote-range "constantsTable" "@3$3")
                                              (read-from-minibuffer "Confidence (1 (high) to 5 (low)): ")))))
    (org-set-property "CONFIDENCE"
                      (number-to-string confidence))))
(defun ndu/remove-tag (tag)
  (interactive)
  (org-set-tags (delete tag (org-get-tags))))
(defun ndu/add-tag (tag)
  (interactive)
  (setq lst (org-get-tags))
  (org-set-tags (push tag lst)))
(defun ndu/remove-drill-tag ()
  (interactive)
  (ndu/remove-tag "drill"))
(defun ndu/add-drill-tag ()
  (interactive)
  (ndu/add-tag "drill"))
(defun ndu/remove-cached-tag ()
  (interactive)
  (ndu/remove-tag "cached"))
(defun ndu/add-cached-tag ()
  (interactive)
  (ndu/add-tag "cached"))
(defun ndu/add-leitner-tag ()
  (interactive)
  (ndu/add-tag "leitner"))
(defun ndu/remove-leitner-tag ()
  (interactive)
  (ndu/remove-tag "leitner"))
(defun ndu/align-tags ()
  (interactive)
  (org-set-tags-command '(4))) ; '(4) is the universal argument
(defun ndu/insert-blocks-task ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (setq spaces (make-string (current-column) ?\s))
  (insert (concat "#+NAME: " (org-entry-get nil "ITEM") "\n"
           spaces "| <10>  | <10>  | <10>  | <3> |"     "\n"
           spaces "| I-TBL | I-TSK | B-TBL | BLK |"     "\n"
           spaces "|-------+-------+-------+-----|"     "\n"
           spaces "|       |       |       |     |")))
(defun ndu/insert-blocks-table ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (setq spaces (make-string (current-column) ?\s))
  (insert (concat "#+NAME: " (org-entry-get nil "ITEM") "\n"
                  spaces "| <10>  | <10>  | <3> |"      "\n"
                  spaces "| I-TBL | B-TBL | BLK |"      "\n"
                  spaces "|-------+-------+-----|"      "\n"
                  spaces "|       |       |     |")))
(defun ndu/insert-topic-table ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (setq spaces (make-string (current-column) ?\s))
  (insert (concat "#+NAME: " (org-entry-get nil "ITEM")                              "\n"
           spaces "| <10>  | <0>   | <0>   | <0>   | <7>   | <0>  | <0> | <6>     |" "\n"
           spaces "| I-TBL | U-IMP | U-UGH | S-EFF | I-PRI | PROG | TSK | TSK-PRI |" "\n"
           spaces "|-------+-------+-------+-------+-------+------+-----+---------|" "\n"
           spaces "|       |       |       |       |       |      |     |         |" "\n"
           spaces "#+TBLFM: "
                  "$2=vmean(remote($1, @I$2..@>$2);%.2f::"
                  "$3=vmean(remote($1, @I$3..@>$3);%.2f::"
                  "$4=min(vsum(remote($1, @I$7..@>$7)), 1000);%.2f::"
                  "$5=vsum(remote($1, @I$8..@>$8))/$4;%.2f::"
                  "$6=vsum(remote($1,@I$6..@>$6))/vlen(remote($1,@I$6..@>$6));%.2f::"
                  "$7='(org-lookup-first $8 '(remote($1, @I$8..@>$8)) '(remote($1, @I$1..@>$1)))::"
                  "$8=vmax(remote($1, @I$8..@>$8));%.2f")))
(defun ndu/insert-item-table ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent)
  (let ((spaces    (make-string (current-column) ?\s))
        (blocksTbl (read-from-minibuffer "Blocking items table: " nil nil nil nil "blocksTbl"))
        (blocksTsk (read-from-minibuffer "Blocking tasks table: " nil nil nil nil "blocksTsk")))
    (insert (concat "#+NAME: " (org-entry-get nil "ITEM") "\n"
                    spaces "| <0> | <1> | <1> | <1> | <1> | <1>  | <0> | <5> | <0>   | <0>  | <4>  |" "\n"
                    spaces "| TSK | IMP | UGH | TIM | BLK | DONE | EFF | PRI | PRI-N | MAYB | DESC |" "\n"
                    spaces "|-----+-----+-----+-----+-----+------+-----+-----+-------+------+------|" "\n"
                    spaces "|     |     |     |     |     |      |     |     |       |      |      |" "\n"
                    spaces "#+TBLFM: "
                           "$1=@#-2::"
                           "$5='(ndu/get-blocks" " " "\"" blocksTbl "\"" " " "\"" blocksTsk "\"" " " "$1)::"
                           "$7=(1-$6)*min(exp(4*min(vsum(@3$5..@>$5), 5))*$4,100);%.2f::"
                           "$8=" "(" "(1-$5)*(1-$6)" "*" "(remote(constantsTable,@5$3)^$10)" "*"
                           "($2*$3^(remote(constantsTable,@3$3)))^(1/3)/($7 + 0.1*$6);%.2f"
                           "$9=5*$8/vmax(@3$8..@>$8);%.1f"))))
(defun ndu/insert-task-topic-item ()
  (interactive)
  (let* ((task (read-from-minibuffer "Heading Name: "))
         (tag  (concat ":" task "@map" ":")))
   (concat "** " task " " tag)))
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
(defun ndu/sum-str-to-num (strs)
  (apply '+ (mapcar 'string-to-number strs)))

(defun ndu/cmp-pairs (x y)
 (and (eq (car x) (car y))
      (eq (cadr x) (cadr y))))
(defun ndu/lookup-all-sum  (val slist rlist &optional predicate)
 (let ((ret (ndu/sum-str-to-num (org-lookup-all
                                 val
                                 slist
                                 rlist))))
  (if ret ret 0)))
(defun ndu/get-blocks (blocksTbl blocksTsk tsk)
  (interactive)
  (cond ((> (ndu/lookup-all-sum
             (org-entry-get nil "ITEM")
             (ndu/get-remote-range blocksTbl "@2$1..@>$1")
             (ndu/get-remote-range blocksTbl "@2$3..@>$3"))
            0) 1)
        ((> (let ((tbls (ndu/get-remote-range blocksTsk "@2$1..@>$1"))
                  (tsks (ndu/get-remote-range blocksTsk "@2$2..@>$2"))
                  (blks (ndu/get-remote-range blocksTsk "@2$4..@>$4")))
             (ndu/lookup-all-sum
              (list (org-entry-get nil "ITEM") tsk)
              (mapcar* 'list tbls tsks)
              blks
              'ndu/cmp-pairs))
            0) 1)
        (t 0)))
(defun ndu/update-tables ()
  "Similar to org-table-iterate-buffer-tables, but excludes headings with tag 'cached'."
  (interactive)
  (let* ((imax 100)
        (i imax)
        (checksum (md5 (buffer-string)))
        c1)
    (org-with-wide-buffer
      (catch 'exit
        (while (> i 0)
          (setq i (1- i))
          (org-table-map-tables
            (lambda ()
              (unless (member "cached" (org-get-tags))
                (org-table-recalculate t t)))
            t)
          (if (equal checksum (setq c1 (md5 (buffer-string))))
              (progn
                (org-table-map-tables #'org-table-align t)
                (ndu/hide-tbmlfm)
                (message "Convergence after %d iterations" (- imax i))
                (throw 'exit t))
            (setq checksum c1)))
        (org-table-map-tables #'org-table-align t)
        (ndu/hide-tbmlfm)
        (user-error "No convergence after %d iterations" imax)))))
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
(defun ndu/insert-note-item ()
  (interactive)
   (concat "** " (format-time-string "I-%Y-%m-%d-%H-%M-%S")))
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
  (insert " ")
  (ndu/org-screenshot filename-short filename)
  (org-display-inline-images))
;; https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
(defun ndu/org-screenshot (filename-short filename &optional anki)
  "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
  (interactive)
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))

  (setq spaces (make-string (current-column) ?\s))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (if anki filename-short
                       (concat "#+NAME: " filename-short "\n"
                               spaces "#+CAPTION: "
                               (format-time-string "%F" (current-time)) "\n"
                               spaces "#+ATTR_ORG: :width 400\n"
                               spaces "[[file:" filename "]]")))))
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

;; bug in org-drill - replace function
(defun ndu/org-drill-time-to-inactive-org-timestamp (time)
  "Convert TIME into org-mode timestamp."
  (format-time-string
    (concat "[" (cdr org-time-stamp-formats) "]")
    time))
(defun ndu/org-mode ()
  (load "~/.emacs.d/manuallyInstalled/vanish.el")
  (require 'vanish)
  (require 'org-drill)
  (require 'org-collector)
  ;; bug in org-drill
  (advice-add 'org-drill-time-to-inactive-org-timestamp :override #'ndu/org-drill-time-to-inactive-org-timestamp)
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
                     (org-mode-hook (visual-fill-column-mode))
                     (org-mode-hook (auto-fill-mode))
                     (org-mode-hook (vanish-mode))))
		;; https://orgmode.org/worg/org-contrib/org-drill.html#orgeb853d5
		(setq org-capture-templates
          `(("w" "note topic" plain (file+headline "~/org/misc-notes-items.org" "Topics")
             "%(ndu/insert-task-topic-item)\n   %?"
             :prepend t :empty-lines-before 0 :empty-lines-after 0)
            ("e" "note item" plain (file+headline "~/org/misc-notes-items.org" "Items")
             "%(ndu/insert-note-item)%(org-set-tags-command)\n   %?"
             :prepend t :empty-lines-before 0 :empty-lines-after 0)
            ("r" "note" plain (file+headline "~/org/gtd.org" "Notes")
             "  * %?" :prepend t :empty-lines-before 0 :empty-lines-after 0)
            ("a" "task topic" plain (file+headline "~/org/gtd.org" "Task topics")
             "%(ndu/insert-task-topic-item)\n   %?"
             :prepend t :empty-lines-before 0 :empty-lines-after 0)
            ("s" "task item" plain (file+headline "~/org/gtd.org" "Task items")
             "%(ndu/insert-task-topic-item)%(org-set-property \"CONFIDENCE\" \"5\")\n   %?"
             :prepend t :empty-lines-before 0 :empty-lines-after 0)
            ("d" "GTD topic" plain (file+headline "~/org/gtd.org" "Topics")
             "%(ndu/insert-task-topic-item)\n   %?"
             :prepend t :empty-lines-before 0 :empty-lines-after 0)
            ("f" "task blocks" plain (file+headline "~/org/gtd.org" "Task blocks")
             "%(ndu/insert-task-topic-item)\n   %?"
             :prepend t :empty-lines-before 0 :empty-lines-after 0)))
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
      org-drill-cram-hours 0
      org-drill-spaced-repetition-algorithm 'sm2
      org-drill-hide-item-headings-p t       ; so priorities not clozed
      org-drill-leech-method nil             ; for reading text
      org-drill-forgetting-index 100         ; for reading text
      org-drill-leech-failure-threshold nil  ; for reading text
      org-drill-scope 'file
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
                 "Face used for unfinished checkbox statistics.")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "j" #'org-match-sparse-tree)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "n" #'ndu/previous-match)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "m" #'ndu/next-match))
(defun ndu/latex ()
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
    '(html osx org git pdf ivy
      (shell :variables shell-default-shell 'eshell)
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
      (elfeed :variables elfeed-enable-goodies nil)
      (elfeed :variables rmh-elfeed-org-files (list "~/org/elfeed/feeds.org"))
	    (spell-checking :variables spell-checking-enable-by-default nil))
    ; List of additional packages that will be installed without being
    ;; wrapped in a layer. If you need some configuration for these
    ;; packages then consider to create a layer, you can also put the
    ;; configuration in `dotspacemacs/config'.helm-R
    dotspacemacs-additional-packages '(ansi-color anki-editor rg
                                       org-drill org-tidy
                                       evil-smartparens cdlatex
                                       latex-extra latex-math-preview
                                       hydra lsp-mode lsp-ui ;nov
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
  (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode)
  (ndu/set-leader
    'anki-editor
    '(("on" ndu/hide-tbmlfm)                 ("om" ndu/show-tbmlfm)
      ("oz" ndu/buffer-backlinks)            ("oZ" ndu/entry-backlinks)
      ("ob" ndu/insert-blocks-table)         ("oB" ndu/insert-blocks-task)
      ("ot" ndu/insert-topic-table)          ("oT" ndu/insert-item-table)
      ("oa" org-drill)                       ("os" org-drill-leitner)
      ("oc" ndu/add-cached-tag)              ("oC" ndu/remove-cached-tag)
      ("od" ndu/add-drill-tag)               ("oD" ndu/remove-drill-tag)
      ("ol" ndu/add-leitner-tag)             ("oL" ndu/remove-leitner-tag)
      ("ov" ndu/expand)                      ("oV" ndu/expand-all)
      ("og" ndu/align-tags)                  ("oG" vanish-mode)
      ("oi" ndu/update-tables)               ("oI" org-table-edit-formulas)
      ("op" ndu/shrink)                      ("oP" ndu/shrink-all)
      ("oY"  ndu/insert-last-stored-link)    ("oy" org-store-link)
      ("o\\" outline-cycle-buffer)           ("o|" org-set-property)
      ("o["  outline-hide-other)             ("o]" outline-show-subtree)
      ("o{"  ndu/set-startup-visibility)     ("o}" outline-hide-body)
      ("o,"  evil-numbers/dec-at-pt)         ("o." ndu/set-confidence)
      ("o<"  org-drill-resume)               ("o>" org-drill-again)
      ("oo" org-capture)))
  (setq-default
    org-clock-sound "~/.emacs.d/manuallyInstalled/bell.wav"
    org-timer-default-timer "0:25:00"
    org-tags-column -40
    dotspacemacs-whitespace-cleanup 'all
    dotspacemacs-check-for-update t
    spacemacs-yank-indent-threshold 0
    pixel-scroll-precision-mode t
    flycheck-python-pycompile-executable "python3"
    python-shell-interpreter "python3"
    auto-save-default t
    visual-fill-column-center-text t
    vterm-always-compile-module t
    org-use-property-inheritance t
    ; nov-text-width 60
    org-id-link-to-org-use-id 'create-if-interactive
    org-adapt-indentation t
    c-default-style "k&r"
    c-basic-offset 4
    org-hide-emphasis-markers t
    org-startup-with-inline-images t
    org-startup-folded 'showall
    org-image-actual-width nil                   ; images requre size attribute
    whitespace-line-column 80                    ; After 79 chars,
    whitespace-style '(face) ;'(face lines-tail) ; highlight columns.
    backup-directory-alist `(("." . "~/.saves")) ; file backups
    flycheck-highlighting-mode 'symbols
    rg-command-line-flags '("--before-context=1")
    flycheck-indication-mode 'left-fringe
    evil-use-y-for-yank t
    maximum-scroll-margin 0.5
    scroll-margin 20
    org-use-property-inheritance t
    org-highest-priority ?A
    org-lowest-priority  ?E
    org-default-priority org-lowest-priority
    git-magit-status-fullscreen t
    c-c++-lsp-enable-semantic-highlight t)
  (setq-default truncate-partial-width-windows nil)
  (global-set-key (kbd "M-q") 'fill-paragraph)
  (spacemacs/toggle-visual-line-navigation-globally-on)
  (mapc #'funcall
        #'(spacemacs/toggle-menu-bar-on ; ndu/latex ndu/ansi-color ndu/doxymacs
           spacemacs/toggle-highlight-current-line-globally-off
           global-whitespace-mode       ; global-flycheck-mode ndu/nov-mode
           ndu/org-mode                 ; ndu/clojure ndu/c-mode ndu/elfeed-mode
           ndu/emacs-lisp))
  ; (ndu/set-hooks '((c++-mode-hook (lsp))))
  ;; Suppress eshell warnings
  (custom-set-variables '(warning-suppress-types '((:warning))))
  (mapc (lambda (hook) (remove-hook 'find-file-hook hook))
        '(vc-refresh-state projectile-find-file-hook-function yas-global-mode-check-buffers
          global-flycheck-mode-check-buffers undo-tree-load-history-from-hook
          projectile-find-file-hook-function global-flycheck-mode-check-buffers
          yas-global-mode-check-buffers undo-tree-load-history-from-hook))
  (ndu/set-keys '(("C-<"    #'ndu/next-row-edit)
                  ("C->"    #'ndu/next-column-edit)
                  ("C-:"    #'ndu/edit-field)
                  ("C-;"    #'counsel-outline)) t)
  (find-file "~/org/misc-notes-items.org")
  (find-file "~/org/gtd.org")
  (vanish-set-hide 'tblfm t))
