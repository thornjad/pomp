;;; pomp.el --- Simple and cool pomodoro timer -*- lexical-binding: t; -*-
;;
;; © 2019-2019 Jade Michael Thornton
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
;; Pomp is a simple and useful pomodoro-style timer. Pomp is based on Pomidor
;; by TatriX, which is in turn based on Tomatinho by Konrad Scorciapino.
;;
;; USAGE
;;
;;  M-x pomp or bind to a useful key like <f12>.
;;
;;  When pomp opens, it automatically begins a work session. There is nothing
;;  to do at this point except work!
;;
;; KEYBINDINGS
;;
;;  | key   | binding             |
;;  |-------+---------------------|
;;  | W     | pomp-work           |
;;  | B     | pomp-break          |
;;  | Q     | pomp-quit           |
;;  | R     | pomp-reset          |
;;  | q     | quit-window         |
;;  | h     | describe-mode       |
;;  | ?     | describe-mode       |
;;  | g     | revert-buffer       |
;;  | DEL   | scroll-down-command |
;;  |-------+---------------------|
;;  | Deprecated                  |
;;  |-------+---------------------|
;;  | S     | pomp-stop           |
;;
;; CUSTOMIZATION
;;
;;  To change timer duration:
;;
;;    (setq pomp-seconds (* 25 60))     ; 25 minutes for work period
;;    (setq pomp-break-seconds (*5 60)) ; 5 minutes for break period
;;
;;; Code:

(require 'cl-lib)


;;; customs

(defgroup pomp nil
  "Customs for `pomp'"
  :group 'productivity)

(defcustom pomp-buffer-name "*pomp*"
  "Name of the pomp buffer."
  :type 'string :group 'pomp)

(defcustom pomp-seconds (* 25 60)
  "Time length of a Podomoro round."
  :type 'integer :group 'pomp)

(defcustom pomp-break-seconds (* 5 60)
  "Time length of a Podomoro break."
  :type 'integer :group 'pomp)

(defcustom pomp-update-interval 30
  "Interval in seconds when pomp should run hooks."
  :type 'integer :group 'pomp)

(defcustom pomp-confirm-end-break t
  "If t ask for confirmation before ending a break and starting new a pomodoro."
  :type 'boolean :group 'pomp)

(defcustom pomp-time-format "%H:%M:%S"
  "Time format for podomoro clock."
  :type 'string :group 'pomp)

(defcustom pomp-duration-format "%H:%M:%S"
  "Time format for duration intervals."
  :type 'string :group 'pomp)

(defun pomp-message ()
  "Default pomp alert message if any."
  (cond
   ((pomp-overwork-p)
    (format "Take a break!\nOverwork: [%s]"
            (format-time-string "%H:%M:%S" (pomp-overwork-duration) t)))
   ((pomp-break-over-notify-p)
    (format "Go back to work!\nBreak: [%s]"
            (format-time-string "%H:%M:%S" (pomp-break-duration) t)))))

(defun pomp-alert ()
  "Default pomp alert."
  (message (pomp-message)))

(defvar pomp-update-hook nil)


;;; faces

(defface pomp-time-face
  '(( t (:height 4.0)))
  "pomp face for time"
  :group 'pomp)

(defface pomp-timer-face
  '(( t (:height 5.0)))
  "pomp face for timer"
  :group 'pomp)

(defface pomp-work-face
  '((t (:inherit 'success)))
  "pomp face for work"
  :group 'pomp)

(defface pomp-overwork-face
  '((t (:inherit 'warning)))
  "pomp face for overwork"
  :group 'pomp)

(defface pomp-break-face
  '((t (:inherit 'font-lock-keyword-face)))
  "pomp face for break"
  :group 'pomp)

(defface pomp-skip-face
  '(( t (:inherit 'font-lock-comment-face)))
  "pomp face for skip"
  :group 'pomp)



;;; vars

(defvar pomp-timer nil
  "Pomp timer.")

(defvar pomp-global-state nil
  "Pomp global state.")

(defvar pomp-graph-char ?█
  "Pomp char for displaying tubes.")

(defvar pomp-header-separator " — "
  "Pomp string to separate time and duration in header.")


;;; private

(defun pomp--current-state ()
  "Return current state."
  (car (last pomp-global-state)))

(defun pomp--reset ()
  "Delete current global state."
  (setq pomp-global-state (list (pomp--make-state))))

(defun pomp--make-state ()
  "Make pomp state."
  (list :started (current-time)
        :break nil
        :stopped nil
        :snooze nil))

(defun pomp--started (state)
  "Return started time for STATE."
  (plist-get state :started))

(defun pomp--break (state)
  "Return break time for STATE."
  (plist-get state :break))

(defun pomp--stopped (state)
  "Return stopped time for STATE.
It's a time when user started a new timer after this one."
  (plist-get state :stopped))

(defun pomp--ended (state)
  "Return ended time for STATE.
It's either stopped time or current time."
  (or (pomp--stopped state) (current-time)))

(defun pomp--work-duration (state)
  "Return work time for STATE."
  (let* ((started (pomp--started state))
         (ended (or (pomp--break state) (pomp--ended state)))
         (work (time-subtract ended started))
         (max (seconds-to-time pomp-seconds)))
    (if (time-less-p work max)
        work
      max)))

(defun pomp--overwork-duration (state)
  "Return overwork time for STATE or nil."
  ;; (cur - started) - (cur - break) - max
  (let* ((started (pomp--started state))
         (break (or (pomp--break state) (pomp--ended state)))
         (ended (pomp--ended state))
         (max (seconds-to-time pomp-seconds))
         (overwork (time-subtract (time-subtract (time-subtract ended started)
                                                 (time-subtract ended break))
                                  max)))
    (when (> (time-to-seconds overwork) 0)
      overwork)))

(defun pomp-running-p ()
  "Return t if pomp is running right now."
  (timerp pomp-timer))

(defun pomp-overwork-p ()
  "Return t if current state is overwork."
  (let* ((state (pomp--current-state))
         (overwork (pomp--overwork-duration state)))
    (and overwork (null (pomp--break state)))))

(defun pomp-break-over-notify-p ()
  "Return t if current break is over and user should be notified about it.
To snooze the notification use `pomp-break'."
  (and (pomp-break-over-p) (not (pomp-snooze-p))))

(defun pomp-break-over-p ()
  "Return t if current break is over."
  (let* ((state (pomp--current-state))
         (break (pomp--break-duration state)))
    (and break (> (time-to-seconds break) pomp-break-seconds))))

(defun pomp-snooze-p ()
  "Return t if user snooze end of break alarm."
  (plist-get (pomp--current-state) :snooze))

(defun pomp--total-duration (state)
  "Return total time for STATE."
  (time-subtract (pomp--ended state) (pomp--started state)))

(defun pomp--break-duration (state)
  "Return break time for STATE."
  (let ((break (pomp--break state)))
    (and break (time-subtract (pomp--ended state) break))))

(defun pomp--format-header (time face)
  "Return formated header for TIME with FACE."
  (concat (pomp--with-face (concat (pomp--format-time (current-time))
                                      pomp-header-separator)
                              'pomp-time-face)
          (propertize (pomp--format-duration time)
                      'face `(:inherit (,face pomp-timer-face)))))

(defun pomp--header ()
  "Return header."
  (let* ((state (pomp--current-state))
         (break (pomp--break-duration state))
         (overwork (pomp--overwork-duration state))
         (work (pomp--work-duration state)))
    (cond
     (break (pomp--format-header break 'pomp-break-face))
     (overwork (pomp--format-header overwork 'pomp-overwork-face))
     (work (pomp--format-header work 'pomp-work-face)))))


(defun pomp--format-time (time)
  "Format TIME as of `pomp-time-format'."
  (format-time-string pomp-time-format time))

(defun pomp--format-duration (time)
  "Format TIME as of `pomp-duration-format'.
TIME may be nil."
  (format-time-string pomp-duration-format (or time (seconds-to-time 0)) t))

(defun pomp--window-width ()
  "Return pomp buffer width in chars."
  (window-total-width (get-buffer-window (pomp--get-buffer-create))))

(defun pomp--with-face (string face)
  "Retrun STRING with FACE."
  (propertize string 'font-lock-face face))

(defun pomp--format-time-string (time face)
  "Format graph string for TIME with FACE."
  (pomp--with-face (make-string (round (/ (time-to-seconds time)
                                     (/ (float pomp-seconds) (/ (pomp--window-width) 2))))
                                   pomp-graph-char)
                      face))

(defun pomp--graph (work overwork break)
  "Format graph based on WORK, OVERWORK and BREAK time."
  (concat
   (pomp--format-time-string work 'pomp-work-face)
   (let ((skip (- pomp-seconds (time-to-seconds work))))
     (when (> skip 0)
       (pomp--format-time-string (seconds-to-time skip) 'pomp-skip-face)))
   (and overwork (pomp--format-time-string overwork 'pomp-overwork-face))
   (and break (pomp--format-time-string break 'pomp-break-face))))

(defun pomp--update ()
  "Update pomp state."
  (let* ((state (pomp--current-state))
         (total (pomp--total-duration state))
         (ellapsed (round (time-to-seconds total))))
    (when (zerop (mod ellapsed pomp-update-interval))
      (pomp-alert)
      (run-hooks 'pomp-update-hook)))
  (pomp--render))

(defun pomp--render ()
  "Render pomp state."
  (let ((buffer (pomp--get-buffer-create)))
    (when (get-buffer-window buffer t)
      (with-current-buffer buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert (pomp--header)
                "\n")
        (cl-loop
         for i from 1
         for state in pomp-global-state

         as work = (pomp--work-duration state)
         as overwork = (pomp--overwork-duration state)
         as break = (pomp--break-duration state)
         as total = (pomp--total-duration state)

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
                       (pomp--with-face (pomp--format-duration work) 'pomp-work-face)
                       (pomp--with-face (pomp--format-duration overwork) 'pomp-overwork-face)
                       (pomp--with-face (pomp--format-duration break) 'pomp-break-face)
                       (pomp--format-duration total)
                       (pomp--format-time (pomp--started state))
                       (pomp--format-time (pomp--ended state)))
               "\n     "
               (pomp--graph work overwork break)))
         finally
         (insert "\n     "
                 (make-string 79 ?-)
                 "\n\n"
                 (format "     Work\t[%s]\n"
                         (pomp--with-face (pomp--format-duration sum-work) 'pomp-work-face))
                 (format "     Overwork\t[%s]\n"
                         (pomp--with-face (pomp--format-duration sum-overwork) 'pomp-overwork-face))
                 (format "     Break\t[%s]\n"
                         (pomp--with-face (pomp--format-duration sum-break) 'pomp-break-face))
                 (format "     Total\t[%s]\n"
                         (pomp--format-duration sum-total)))
         )
        (read-only-mode +1)))))

(defun pomp--get-buffer-create ()
  "Return a pomp buffer."
  (get-buffer-create pomp-buffer-name))

(defun pomp--cancel-timer ()
  "Cancel pomp timer."
  (when (timerp pomp-timer)
    (cancel-timer pomp-timer)
    (setq pomp-timer nil)))


;;; public

(defvar pomp-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'pomp-quit)
    (define-key map (kbd "R") #'pomp-reset)
    (define-key map (kbd "W") #'pomp-work)
    (define-key map (kbd "S") #'pomp-stop)
    (define-key map (kbd "B") #'pomp-break)
    (suppress-keymap map)
    map))

(defun pomp-work-duration ()
  "Return current work duration."
  (pomp--work-duration (pomp--current-state)))

(defun pomp-overwork-duration ()
  "Return current overwork duration."
  (pomp--overwork-duration (pomp--current-state)))

(defun pomp-break-duration ()
  "Return current break duration."
  (pomp--break-duration (pomp--current-state)))

(defun pomp-total-duration ()
  "Return current total duration."
  (pomp--total-duration (pomp--current-state)))

(defun pomp-quit ()
  "Turn off Pomp."
  (interactive)
  (when (y-or-n-p "Are you sure you want to turn off pomp? ")
    (kill-buffer (pomp--get-buffer-create))))

(defun pomp-break ()
  "Break current working pomp."
  (interactive)
  (let ((state (pomp--current-state)))
    (if (pomp--break state)
        (progn
          (plist-put state :snooze t)
          (when (or (not pomp-confirm-end-break)
                    (yes-or-no-p "Stop break and start new pomp?"))
            (pomp-work)))
      (plist-put state :break (current-time)))))

(defun pomp-reset ()
  "Delete current global state."
  (interactive)
  (when (y-or-n-p "Are you sure you want reset pomps? ")
    (pomp--reset)))

(defun pomp-work ()
  "Begin a new work pomp."
  (interactive)
  (let ((state (pomp--current-state)))
    (plist-put state :stopped (current-time)))
  (nconc pomp-global-state (list (pomp--make-state))))

(define-obsolete-function-alias 'pomp-stop #'pomp-work)


;;; package

(define-derived-mode pomp-mode special-mode "pomp"
  "Major mode for Pomp.

\\{pomp-mode-map}"
  (setq pomp-timer (run-at-time nil 1 #'pomp--update))
  (setq-local tab-width 8)
  (add-hook 'kill-buffer-hook #'pomp--cancel-timer nil t)
  (pomp--reset))

;;;###autoload
(defun pomp ()
  "A simple and cool pomodoro technique timer."
  (interactive)
  (switch-to-buffer (pomp--get-buffer-create))
  (unless (eq major-mode 'pomp-mode)
    (pomp-mode))
  (pomp--update))

(provide 'pomp)
;;; pomp.el ends here
