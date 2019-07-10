;;; velox.el --- Simple and cool pomodoro timer -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See <https://www.gnu.org/licenses/> for more details.
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;; Velox is a simple and useful pomodoro-style timer. Velox is based on Pomidor
;; by TatriX, which is in turn based on Tomatinho by Konrad Scorciapino.
;;
;; USAGE
;;
;; 	M-x velox or bind to a useful key like <f12>.
;;
;; 	When velox opens, it automatically begins a work session. There is nothing
;; 	to do at this point except work!
;;
;; KEYBINDINGS
;;
;; 	| key   | binding             |
;; 	|-------+---------------------|
;; 	| S     | velox-stop          |
;; 	| B     | velox-break         |
;; 	| Q     | velox-quit          |
;; 	| R     | velox-reset         |
;; 	| q     | quit-window         |
;; 	| h     | describe-mode       |
;; 	| ?     | describe-mode       |
;; 	| g     | revert-buffer       |
;; 	| DEL   | scroll-down-command |
;;
;; CUSTOMIZATION
;;
;; 	To change timer duration:
;;
;; 		(setq velox-seconds (* 25 60))     ; 25 minutes for work period
;; 		(setq velox-break-seconds (*5 60)) ; 5 minutes for break period
;;
;;; Code:

(require 'cl-lib)


;;; customs

(defgroup velox nil
  "Customs for `velox'"
  :group 'productivity)

(defcustom velox-buffer-name "*velox*"
  "Name of the velox buffer."
  :type 'string :group 'velox)

(defcustom velox-seconds (* 25 60)
  "Time length of a Podomoro round."
  :type 'integer :group 'velox)

(defcustom velox-break-seconds (* 5 60)
  "Time length of a Podomoro break."
  :type 'integer :group 'velox)

(defcustom velox-update-interval 30
  "Interval in seconds when velox should run hooks."
  :type 'integer :group 'velox)

(defcustom velox-confirm-end-break t
  "If t ask for confirmation before ending a break and starting new a pomodoro."
  :type 'boolean :group 'velox)

(defcustom velox-time-format "%H:%M:%S"
  "Time format for podomoro clock."
  :type 'string :group 'velox)

(defcustom velox-duration-format "%H:%M:%S"
  "Time format for duration intervals."
  :type 'string :group 'velox)

(defun velox-message ()
  "Default velox alert message if any."
  (cond
   ((velox-overwork-p)
    (format "Take a break!\nOverwork: [%s]"
            (format-time-string "%H:%M:%S" (velox-overwork-duration) t)))
   ((velox-break-over-notify-p)
    (format "Go back to work!\nBreak: [%s]"
            (format-time-string "%H:%M:%S" (velox-break-duration) t)))))

(defun velox-alert ()
  "Default velox alert."
	(message (velox-message)))

(defvar velox-update-hook nil)


;;; faces

(defface velox-time-face
  '(( t (:height 4.0)))
  "velox face for time"
  :group 'velox)

(defface velox-timer-face
  '(( t (:height 5.0)))
  "velox face for timer"
  :group 'velox)

(defface velox-work-face
  '((t (:inherit 'success)))
  "velox face for work"
  :group 'velox)

(defface velox-overwork-face
  '((t (:inherit 'warning)))
  "velox face for overwork"
  :group 'velox)

(defface velox-break-face
  '((t (:inherit 'font-lock-keyword-face)))
  "velox face for break"
  :group 'velox)

(defface velox-skip-face
  '(( t (:inherit 'font-lock-comment-face)))
  "velox face for skip"
  :group 'velox)



;;; vars

(defvar velox-timer nil
  "Velox timer.")

(defvar velox-global-state nil
  "Velox global state.")

(defvar velox-graph-char ?█
  "Velox char for displaying tubes.")

(defvar velox-header-separator " — "
  "Velox string to separate time and duration in header.")


;;; private

(defun velox--current-state ()
  "Return current state."
  (car (last velox-global-state)))

(defun velox--reset ()
  "Delete current global state."
  (setq velox-global-state (list (velox--make-state))))

(defun velox--make-state ()
  "Make velox state."
  (list :started (current-time)
        :break nil
        :stopped nil
        :snooze nil))

(defun velox--started (state)
  "Return started time for STATE."
  (plist-get state :started))

(defun velox--break (state)
  "Return break time for STATE."
  (plist-get state :break))

(defun velox--stopped (state)
  "Return stopped time for STATE.
It's a time when user started a new timer after this one."
  (plist-get state :stopped))

(defun velox--ended (state)
  "Return ended time for STATE.
It's either stopped time or current time."
  (or (velox--stopped state) (current-time)))

(defun velox--work-duration (state)
  "Return work time for STATE."
  (let* ((started (velox--started state))
         (ended (or (velox--break state) (velox--ended state)))
         (work (time-subtract ended started))
         (max (seconds-to-time velox-seconds)))
    (if (time-less-p work max)
        work
      max)))

(defun velox--overwork-duration (state)
  "Return overwork time for STATE or nil."
  ;; (cur - started) - (cur - break) - max
  (let* ((started (velox--started state))
         (break (or (velox--break state) (velox--ended state)))
         (ended (velox--ended state))
         (max (seconds-to-time velox-seconds))
         (overwork (time-subtract (time-subtract (time-subtract ended started)
                                                 (time-subtract ended break))
                                  max)))
    (when (> (time-to-seconds overwork) 0)
      overwork)))

(defun velox-running-p ()
  "Return t if velox is running right now."
  (timerp velox-timer))

(defun velox-overwork-p ()
  "Return t if current state is overwork."
  (let* ((state (velox--current-state))
         (overwork (velox--overwork-duration state)))
    (and overwork (null (velox--break state)))))

(defun velox-break-over-notify-p ()
  "Return t if current break is over and user should be notified about it.
To snooze the notification use `velox-break'."
  (and (velox-break-over-p) (not (velox-snooze-p))))

(defun velox-break-over-p ()
  "Return t if current break is over."
  (let* ((state (velox--current-state))
         (break (velox--break-duration state)))
    (and break (> (time-to-seconds break) velox-break-seconds))))

(defun velox-snooze-p ()
  "Return t if user snooze end of break alarm."
  (plist-get (velox--current-state) :snooze))

(defun velox--total-duration (state)
  "Return total time for STATE."
  (time-subtract (velox--ended state) (velox--started state)))

(defun velox--break-duration (state)
  "Return break time for STATE."
  (let ((break (velox--break state)))
    (and break (time-subtract (velox--ended state) break))))

(defun velox--format-header (time face)
  "Return formated header for TIME with FACE."
  (concat (velox--with-face (concat (velox--format-time (current-time))
                                      velox-header-separator)
                              'velox-time-face)
          (propertize (velox--format-duration time)
                      'face `(:inherit (,face velox-timer-face)))))

(defun velox--header ()
  "Return header."
  (let* ((state (velox--current-state))
         (break (velox--break-duration state))
         (overwork (velox--overwork-duration state))
         (work (velox--work-duration state)))
    (cond
     (break (velox--format-header break 'velox-break-face))
     (overwork (velox--format-header overwork 'velox-overwork-face))
     (work (velox--format-header work 'velox-work-face)))))


(defun velox--format-time (time)
  "Format TIME as of `velox-time-format'."
  (format-time-string velox-time-format time))

(defun velox--format-duration (time)
  "Format TIME as of `velox-duration-format'.
TIME may be nil."
  (format-time-string velox-duration-format (or time (seconds-to-time 0)) t))

(defun velox--window-width ()
  "Return velox buffer width in chars."
  (window-total-width (get-buffer-window (velox--get-buffer-create))))

(defun velox--with-face (string face)
  "Retrun STRING with FACE."
  (propertize string 'font-lock-face face))

(defun velox--format-time-string (time face)
  "Format graph string for TIME with FACE."
  (velox--with-face (make-string (round (/ (time-to-seconds time)
                                     (/ (float velox-seconds) (/ (velox--window-width) 2))))
                                   velox-graph-char)
                      face))

(defun velox--graph (work overwork break)
  "Format graph based on WORK, OVERWORK and BREAK time."
  (concat
   (velox--format-time-string work 'velox-work-face)
   (let ((skip (- velox-seconds (time-to-seconds work))))
     (when (> skip 0)
       (velox--format-time-string (seconds-to-time skip) 'velox-skip-face)))
   (and overwork (velox--format-time-string overwork 'velox-overwork-face))
   (and break (velox--format-time-string break 'velox-break-face))))

(defun velox--update ()
  "Update velox state."
  (let* ((state (velox--current-state))
         (total (velox--total-duration state))
         (ellapsed (round (time-to-seconds total))))
    (when (zerop (mod ellapsed velox-update-interval))
			(velox-alert)
      (run-hooks 'velox-update-hook)))
  (velox--render))

(defun velox--render ()
  "Render velox state."
  (let ((buffer (velox--get-buffer-create)))
    (when (get-buffer-window buffer t)
      (with-current-buffer buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert (velox--header)
                "\n")
        (cl-loop
         for i from 1
         for state in velox-global-state

         as work = (velox--work-duration state)
         as overwork = (velox--overwork-duration state)
         as break = (velox--break-duration state)
         as total = (velox--total-duration state)

         with sum-work = (seconds-to-time 0)
         with sum-overwork = (seconds-to-time 0)
         with sum-break = (seconds-to-time 0)
         with sum-total = (seconds-to-time 0)

         do (progn
              (setq sum-work (time-add sum-work work)
                    sum-total (time-add sum-total total))
              (when overwork
                (setq sum-overwork (time-add sum-overwork overwork)))
              (when break
                (setq sum-break (time-add sum-break break)))
              (insert
               "\n     "
               (make-string 79 ?-)
               "\n"
               (format "%3d) [%s] | [%s] | [%s] | [%s]\t\t %s → %s"
                       i
                       (velox--with-face (velox--format-duration work) 'velox-work-face)
                       (velox--with-face (velox--format-duration overwork) 'velox-overwork-face)
                       (velox--with-face (velox--format-duration break) 'velox-break-face)
                       (velox--format-duration total)
                       (velox--format-time (velox--started state))
                       (velox--format-time (velox--ended state)))
               "\n     "
               (velox--graph work overwork break)))
         finally
         (insert "\n     "
                 (make-string 79 ?-)
                 "\n\n"
                 (format "     Work\t[%s]\n"
                         (velox--with-face (velox--format-duration sum-work) 'velox-work-face))
                 (format "     Overwork\t[%s]\n"
                         (velox--with-face (velox--format-duration sum-overwork) 'velox-overwork-face))
                 (format "     Break\t[%s]\n"
                         (velox--with-face (velox--format-duration sum-break) 'velox-break-face))
                 (format "     Total\t[%s]\n"
                         (velox--format-duration sum-total)))
         )
        (read-only-mode +1)))))

(defun velox--get-buffer-create ()
  "Return a velox buffer."
  (get-buffer-create velox-buffer-name))

(defun velox--cancel-timer ()
  "Cancel velox timer."
  (when (timerp velox-timer)
    (cancel-timer velox-timer)
    (setq velox-timer nil)))


;;; public

(defvar velox-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'velox-quit)
    (define-key map (kbd "R") #'velox-reset)
    (define-key map (kbd "S") #'velox-stop)
    (define-key map (kbd "B") #'velox-break)
    (suppress-keymap map)
    map))

(defun velox-work-duration ()
  "Return current work duration."
  (velox--work-duration (velox--current-state)))

(defun velox-overwork-duration ()
  "Return current overwork duration."
  (velox--overwork-duration (velox--current-state)))

(defun velox-break-duration ()
  "Return current break duration."
  (velox--break-duration (velox--current-state)))

(defun velox-total-duration ()
  "Return current total duration."
  (velox--total-duration (velox--current-state)))

(defun velox-quit ()
  "Turn off Velox."
  (interactive)
  (when (y-or-n-p "Are you sure you want to turn off velox? ")
    (kill-buffer (velox--get-buffer-create))))

(defun velox-break ()
  "Break current working velox."
  (interactive)
  (let ((state (velox--current-state)))
    (if (velox--break state)
        (progn
          (plist-put state :snooze t)
          (when (or (not velox-confirm-end-break)
                    (yes-or-no-p "Stop break and start new velox?"))
            (velox-stop)))
      (plist-put state :break (current-time)))))

(defun velox-reset ()
  "Delete current global state."
  (interactive)
  (when (y-or-n-p "Are you sure you want reset veloxs? ")
    (velox--reset)))

(defun velox-stop ()
  "Stop current working velox."
  (interactive)
  (let ((state (velox--current-state)))
    (plist-put state :stopped (current-time)))
  (nconc velox-global-state (list (velox--make-state))))


;;; package

(define-derived-mode velox-mode special-mode "velox"
  "Major mode for Velox.

\\{velox-mode-map}"
  (setq velox-timer (run-at-time nil 1 #'velox--update))
  (setq-local tab-width 8)
  (add-hook 'kill-buffer-hook #'velox--cancel-timer nil t)
  (velox--reset))

;;;###autoload
(defun velox ()
  "A simple and cool pomodoro technique timer."
  (interactive)
  (switch-to-buffer (velox--get-buffer-create))
  (unless (eq major-mode 'velox-mode)
    (velox-mode))
  (velox--update))

(provide 'velox)

;;; velox.el ends here
