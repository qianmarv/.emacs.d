;;; init-org.el --- Org-Mode Customizing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Show the clocked-in task - if any - in the header line
(defun my-org/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun my-org/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(defun my-org/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun my-org/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun my-org/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Different Apps to Be Called Under Different OS Platform
;;   Win: Powershell Tool https://github.com/Windos/BurntToast
(defun my-org/show-alarm (min-to-app new-time message)
  (cond ((string-equal system-type "windows-nt") (call-process "powershell"
                                                                nil
                                                                t
                                                                nil
                                                                (format " New-BurntToastNotification -Text '%s' -Sound 'Alarm2' -SnoozeAndDismiss" message)))
        ((string-equal system-type "gnu/linux") (call-process "notify-send"
                                                              nil
                                                              t
                                                              nil
                                                              (format "Emacs Alarm: '%s'" message)))))

(defmacro my-org/expand-template (name)
  "Expand template NAME to full path."
  (concat my-org/gtd-directory "/templates/" name ".tpl"))

(defun my-org/make-notebook (name)
  "Make notebook NAME to full path. TODO create the notebook if NOT exist."
  (concat my-org/gtd-directory "/" name ".org"))

;; The function defined to adapt same configuration for spacemacs inside package.el
(defun my-org/post-init-org ()
  (add-hook 'org-mode-hook (lambda ()
                             ;; (spacemacs/toggle-line-numbers-off)
                             ;; (spacemacs/toggle-auto-fill-mode-off)
                             (add-hook 'before-save-hook 'time-stamp)) 'append)

  (with-eval-after-load 'org
    (progn
      ;; (spacemacs|disable-company org-mode)
      ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode
      ;;                                           "," 'org-priority
      ;;                                           "ir" 'my-org/insert-src-block
      ;;                                           "iq" 'my-org/insert-quote
      ;;                                           "ip" 'my-org/insert-screenshot)
      (require 'org-compat)

      (require 'org)
      ;; (add-to-list 'org-modules "org-habit")
      ;; (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)

      (visual-line-mode)

      ;; Enable publish taskjuggler
      ;; https://orgmode.org/worg/exporters/taskjuggler/ox-taskjuggler.html
;;       (require 'ox-taskjuggler)

;;       (setq org-taskjuggler-reports-directory "~/Org/TaskJuggler/reports")
;;       ;;https://hugoideler.com/2018/09/org-mode-and-wide-taskjuggler-html-export/
;;       (setq org-taskjuggler-default-reports
;;             '("textreport report \"Plan\" {
;;   formats html
;;   header '== %title =='
;;   center -8<-
;;     [#Plan Plan] | [#Resource_Allocation Resource Allocation]
;;     ----
;;     === Plan ===
;;     <[report id=\"plan\"]>
;;     ----
;;     === Resource Allocation ===
;;     <[report id=\"resourceGraph\"]>
;;   ->8-
;; }
;; # A traditional Gantt chart with a project overview.
;; taskreport plan \"\" {
;;   headline \"Project Plan\"
;;   columns bsi, name, start, end, resources, complete, effort, effortdone, effortleft, chart { width 1000 }
;;   loadunit shortauto
;;   hideresource 1
;; }
;; # A graph showing resource allocation. It identifies whether each
;; # resource is under- or over-allocated for.
;; resourcereport resourceGraph \"\" {
;;   headline \"Resource Allocation Graph\"
;;   columns no, name, effort, weekly { width 1000 }
;;   loadunit shortauto
;;   hidetask ~(isleaf() & isleaf_())
;;   sorttasks plan.start.up
;; }"))

;;       (setq org-taskjuggler-valid-task-attributes '(account start note duration endbuffer endcredit end flags journalentry length limits maxend maxstart minend minstart period reference responsible scheduling startbuffer startcredit statusnote chargeset charge priority))

;;       (setq org-taskjuggler-valid-resource-attributes '(limits vacation shift booking efficiency journalentry rate workinghours flags leaves))
      ;; Resume Clocking Task On Clock-in If The Clock Is Open
      (setq org-clock-in-resume t)

      ;; Resume Clocking Task When Emacs Is Restarted
      (org-clock-persistence-insinuate)

      ;; Save The Running Clock And All Clock History When Exiting Emacs, Load It On Startup
      (setq org-clock-persist t)

      ;; Do Not Prompt To Resume An Active Clock
      (setq org-clock-persist-query-resume nil)

      ;; Set Agenda Span To Daily By Default
      (setq org-agenda-span 'day)

      ;; Removes Clocked Tasks With 0:00 Duration
      (setq org-clock-out-remove-zero-time-clocks t)

      ;; Set Org Clock, Change default leve from 2 to 3
      (setq org-clock-clocktable-default-properties '(:maxlevel 3 :scope file))

      ;; When Setting This Variable To nil,
      ;; 'a_b' Will Not Be Interpreted As A Subscript, But 'a_{b}' Will.
      ;; Default value is t
      (setq org-export-with-sub-superscripts nil)

      ;; Active Org-babel languages
      (org-babel-do-load-languages
       'org-babel-load-languages
       '(;; other Babel languages
         ;; Config plantuml
         ;; http://archive.3zso.com/archives/plantuml-quickstart.html
         (plantuml . t)
         (ditaa . t)
         (python . t)
         (perl . t)
         (ruby . t)
         (R . t)
         (shell . t)
         (gnuplot . t)
         (org . t)
         (latex . t)
         (java . t)
         (emacs-lisp . t)
         ;; (racket . t)
         (calc . t)
         (sql . t)
         (dot . t)
         ))
      ;; Config plantuml path
      (setq org-plantuml-jar-path
            (expand-file-name "~/.emacs.d/plugins/plantuml.jar"))

      (setq plantuml-jar-path
            (expand-file-name "~/.emacs.d/plugins/plantuml.jar"))
      ;; Config ditaa path
      (setq org-ditaa-jar-path
            (expand-file-name "~/.emacs.d/plugins/ditaa0_9.jar")) ;

      ;; Setup for GTD
      ;; Refer to http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html
      (setq org-todo-keywords
            (quote ((sequence "IDEA(i)" "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                    (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                    (sequence "STARTED(s)" "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
            org-todo-repeat-to-state "TODO")

      (setq org-todo-keyword-faces
            '(("IDEA" . (:foreground "GoldenRod" :weight bold))
              ("NEXT" . (:foreground "IndianRed1" :weight bold))   
              ("STARTED" . (:foreground "OrangeRed" :weight bold))
              ("WAITING" . (:foreground "coral" :weight bold)) 
              ("CANCELED" . (:foreground "LimeGreen" :weight bold))
              ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
              ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
              ))

      ;; (setq org-tag-persistent-alist
      ;;       '((:startgroup . nil)
      ;;         ("G@2019_EfficientWork" . ?e)
      ;;         ("G@2019_DevExpert" . ?d)
      ;;         ("G@2019_BizExpert" . ?b)
      ;;         (:endgroup . nil)
      ;;         ;; (:startgroup . nil)
      ;;         ;; ("EASY" . ?e)
      ;;         ;; ("MEDIUM" . ?m)
      ;;         ;; ("HARD" . ?a)
      ;;         ;; (:endgroup . nil)
      ;;         ;; ("URGENT" . ?u)
      ;;         ;; ("KEY" . ?k)
      ;;         ;; ("BONUS" . ?b)
      ;;         )
      ;;       )

      (setq org-tag-faces
            '(
              ("G@2019_EfficientWork" . (:foreground "GoldenRod" :weight bold))
              ("G@2019_DevExpert" . (:foreground "GoldenRod" :weight bold))
              ("G@2019_BizExpert" . (:foreground "GoldenRod" :weight bold))
              ("G@2019_MSE2020" . (:foreground "Red" :weight bold))
              ("G@2019_SelfMastery" . (:foreground "Red" :weight bold))
              ("@2019_Emacsen" . (:foreground "Red" :weight bold))
              ("@2019_Health" . (:foreground "OrangeRed" :weight bold))
              ("G@2019_Education" . (:foreground "OrangeRed" :weight bold))
              ("G@2019_Trip" . (:foreground "OrangeRed" :weight bold))
              ("G@2019_Decorate" . (:foreground "OrangeRed" :weight bold))
              ;; ("BONUS" . (:foreground "GoldenRod" :weight bold))
              )
            )


;;; Refiling

      (setq org-refile-use-cache nil)

      ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
      (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

      (add-to-list 'org-agenda-after-show-hook 'org-show-entry)

      ;; (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

      ;; Exclude DONE state tasks from refile targets

      (setq org-refile-target-verify-function 'my-org/verify-refile-target)

      ;; Targets start with the file name - allows creating level 1 tasks
      ;;(setq org-refile-use-outline-path (quote file))
      (setq org-refile-use-outline-path t)
      (setq org-outline-path-complete-in-steps nil)

      ;; Allow refile to create parent tasks with confirmation
      (setq org-refile-allow-creating-parent-nodes 'confirm)


      (setq org-enable-priority-commands nil)
      ;; --------------------------------------------------------------------
      ;; Encypting files or org
      ;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
      ;; --------------------------------------------------------------------
      ;; (require 'epa-file)
      (epa-file-enable)
      (require 'org-crypt)
      (org-crypt-use-before-save-magic)
      (setq org-tags-exclude-from-inheritance (quote ("crypt")))
      ;; GPG key to use for encryption
      ;; Either the Key ID or set to nil to use symmetric encryption.
      ;; 
      ;; (setq org-crypt-key "AC88F93004D199BC")
      (setq org-crypt-key nil)


      (setq my-org/gtd-directory "~/Org/GTD")

      (let* ((journal-book (my-org/make-notebook "Journal"))
             (inbox-book (my-org/make-notebook "Inbox"))
             (capture-book (my-org/make-notebook "Event"))
             (agenda-book (my-org/make-notebook "Agenda"))
             (project-book (my-org/make-notebook "Projects")))

        (setq org-capture-templates
              `(
                ("j" "Journals, Morning Write" entry
                 (file+olp+datetree ,journal-book) "* Morning Write\n%U\n%?" :tree-type week)
                ("b" "Break / Interrupt" entry
                 (file+headline ,capture-book "Other Interrupts") "* DONE %?\n%U %i\n" :clock-in t :clock-resume t)
                ("c" "Collect/Capture")
                ("cn" "Take Notes" entry
                 (file+headline ,capture-book "Notes") "* %^{Note Title}\nNote taken on %U \\\\\n%?\n%K")
                ("ci" "Capture Ideas/Mighty new todos" entry
                 (file+headline ,capture-book "Ideas")  "* TODO %?\n %i\n")
                ("cb" "Books want Read" entry
                 (file+headline ,capture-book "Books") (file ,(my-org/expand-template "book")))
                ("cm" "Movies want Watch" entry
                 (file+headline ,capture-book "Movies") (file ,(my-org/expand-template "movie")))
                ("r" "Review")
                ("rd" "Daily Review"  entry
                 (file+olp+datetree ,journal-book) (file ,(my-org/expand-template "daily_review")) :tree-type week :time-prompt t)
                ("rw" "Weekly Review"  entry
                 (file+olp+datetree ,journal-book) (file ,(my-org/expand-template "weekly_review")) :tree-type week :time-prompt t)
                ("rm" "Monthly Review"  entry
                 (file+olp+datetree ,journal-book) (file ,(my-org/expand-template "monthly_review")) :tree-type week :time-prompt t))))


      (add-hook 'org-clock-in-hook 'my-org/show-org-clock-in-header-line)
      (add-hook 'org-clock-out-hook 'my-org/hide-org-clock-from-header-line)
      (add-hook 'org-clock-cancel-hook 'my-org/hide-org-clock-from-header-line)

      ;; (after-load 'org-clock
      (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
      (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)
      ;; )


      ;; (setq org-agenda-files  `(,my-org/gtd-path))
      (setq org-agenda-compact-blocks nil)
      ;;      (spacemacs/set-leader-keys "'" 'my-org/insert-screenshot)
      ;; Re-align tags when window shape changes
      (add-hook 'org-agenda-mode-hook
                (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))
      ;; --------------------------------------------------------------------
      ;; Settings for Reminder - appt
      ;; Refer to: https://emacs.stackexchange.com/questions/3844/good-methods-for-setting-up-alarms-audio-visual-triggered-by-org-mode-events
      ;; --------------------------------------------------------------------

      (setq org-show-notification-handler
            (lambda (msg) (my-org/show-alarm nil nil msg)))


      ;; (setq appt-disp-window-function 'my-org/show-alarm)
      ;; (setq appt-message-warning-time 5) ; Show notification 5 minutes before event
      ;; (setq appt-display-interval appt-message-warning-time) ; Disable multiple reminders
      ;; (setq appt-display-mode-line nil)
      ;; ;; Add Desktop Notification
      ;; ;; Refer to https://gist.github.com/jstewart/7664823
      ;; (add-hook 'org-pomodoro-finished-hook
      ;;           (lambda ()
      ;;             (my-org/show-alarm 0 0 "Pomodoro completed! - Time for a break.")))

      ;; (add-hook 'org-pomodoro-break-finished-hook
      ;;           (lambda ()
      ;;             (my-org/show-alarm 0 0 "Pomodoro Short Break Finished - Ready for Another?")))

      ;; (add-hook 'org-pomodoro-long-break-finished-hook
      ;;           (lambda ()
      ;;             (my-org/show-alarm 0 0 "Pomodoro Long Break Finished - Ready for Another?")))

      ;; (add-hook 'org-pomodoro-killed-hook
      ;;           (lambda ()
      ;;             (my-org/show-alarm 0 0 "Pomodoro Killed - One does not simply kill a pomodoro!")))
      ;; Setup Publish
      (setq org-publish-project-alist
            `(
              ("Blog-Note"
               :base-directory "~/Org/Blog/"
               :recursive t
               :publishing-directory "~/Git/blog/source/_posts/"
               :publishing-function org-md-publish-to-md)
              ("Blog-Static"
               :base-directory "~/Org/Blog/"
               :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
               :publishing-directory "~/Git/blog/source/_posts/"
               :recursive t
               :publishing-function org-publish-attachment)
              ("Blog" :components ("Blog-Note" "Blog-Static"))))

      ;; Publish with
      ;; (org-publish-current-project) ;; While having a file in your project open
      ;; OR
      ;; M-x org-publish <RET> project-name <RET>

      (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))

      (setq org-agenda-include-diary t
            diary-file (concat my-org/gtd-directory "/diary.org")
            org-agenda-diary-file 'diary-file)
      )))

(my-org/post-init-org)

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:name "Log "
                 :log t)
          (:name "Current Focus "
                 :todo "STARTED")
          (:name "Schedule "
                 :time-grid t)
          (:name "Today "
                 :scheduled today)
          (:name "Habits "
                 :habit t)
          (:name "Due today "
                 :deadline today)
          (:name "Overdue "
                 :deadline past)
          (:name "Due soon "
                 :deadline future)
          (:name "Scheduled earlier "
                 :scheduled past)))
  (org-super-agenda-mode))

(provide 'init-org)
;;; init-org.el ends here
