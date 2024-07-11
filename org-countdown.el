;;; org-countdown.el --- Countdown timers in Org Mode -*- lexical-binding: t -*-

;; Author: Ad <me@skissue.xyz>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (ts "0.3"))
;; Homepage: https://github.com/skissue/org-countdown
;; Keywords: calendar, text


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; (WIP) Countdown timers in Org Mode

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'ts)

(defgroup org-countdown nil
  "Countdown timers in Org Mode."
  :group 'org-time)

(defface org-countdown-overlay '((default :inherit (org-date default)
                                          :height 0.9))
  "Face for `org-countdown' countdown text.")

(defcustom org-countdown-link-elapsed-behavior 'zeros
  "How to show elapsed countdowns for links.

Valid choices:
- `zeros': show all zeros for the duration
- `none': do not style elapsed durations
- `negative': show the duration since the timestamp as a negative value

Note that this has no effect if not using the default
`org-countdown-format-duration-function'!"
  :type '(choice (const zeros)
                 (const none)
                 (const negative)))

(defcustom org-countdown-style-timestamp-types '(deadline)
  "List of timestamp types to add countdowns to.
Valid values are:
- `deadline': add countdowns to DEADLINE cookies.
- `scheduled': add countdowns to SCHEDULED cookies.
- `active': add countdowns to all active timestamps."
  :type '(repeat (choice (const deadline)
                         (const scheduled)
                         (const active))))

(defcustom org-countdown-format-duration-function #'org-countdown--format-duration
  "Function to use to format the duration left until a timestamp.
Takes a single argument, the target timestamp, as a `ts' timestamp
struct. Should return a string to display."
  :type 'function)

;; We make this variable global so that we only need one timer
(defvar org-countdown--overlays nil
  "List of all `org-countdown' overlays.")

(defvar org-countdown--timer nil
  "Timer that updates `org-countdown' overlays.")

(defun org-countdown--follow (_path _prefix)
  "Handle following a `countdown:' link."
  (org-countdown--style-link (org-element-context))
  (org-countdown--start-timer-maybe))

(defun org-countdown--complete (&optional _arg)
  "Complete a `countdown:' link using `org-read-date'."
  (let ((timestamp (org-read-date)))
    (concat "countdown:" timestamp)))

(defun org-countdown--register ()
  "Register `countdown:' link type with Org Mode."
  (org-link-set-parameters
   "countdown"
   :follow #'org-countdown--follow
   :complete #'org-countdown--complete))

(defun org-countdown--format-duration (timestamp)
  "Format duration until TIMESTAMP for display as a string."
  (let* ((diff (ts-diff timestamp (ts-now)))
         (text (cl-destructuring-bind
                   (&key years days hours minutes &allow-other-keys)
                   (ts-human-duration diff)
                 (format "⏳ %dd %dh %dm"
                         (+ (* 365 years) days)
                         hours minutes))))
    (if (> diff 0)
        text
      (cl-case org-countdown-link-elapsed-behavior
        (zeros "⏳ 0d 0h 0m")
        (none nil)
        (negative text)
        (t (user-error "Invalid value for `org-countdown-link-elapsed-behavior'"))))))

(defun org-countdown-remove-at-point ()
  "Remove countdown overlay at point."
  (interactive)
  (when-let ((ov (cl-find-if (lambda (ov)
                               (member ov org-countdown--overlays))
                             (overlays-at (point)))))
    ;; Will get removed from list during cleanup
    (delete-overlay ov)))

(defvar org-countdown-overlay-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'org-countdown-remove-at-point)
    map)
  "Keymap for `org-countdown' overlays.")

(defun org-countdown--style-link (link)
  "If applicable, add an overlay to the link element LINK."
  (when-let* ((type (org-element-property :type link))
              ((string= type "countdown"))
              (target (org-element-property :path link))
              (begin (org-element-begin link))
              (end (org-element-end link))
              (ov (make-overlay begin end))
              (ts (ts-parse target)))
    (overlay-put ov 'display (funcall org-countdown-format-duration-function ts))
    (overlay-put ov 'face 'org-countdown-overlay)
    (overlay-put ov 'keymap org-countdown-overlay-map)
    (overlay-put ov 'timestamp ts)
    (push ov org-countdown--overlays)))

(defun org-countdown--style-timestamp (timestamp)
  "Add an overlay to the timestamp element TIMESTAMP."
  (let* ((beg (org-element-begin timestamp))
         ;; We want the text to appear before the closing bracket.
         (end (1- (org-element-end timestamp)))
         (ov (make-overlay beg end))
         (ts (ts-parse-org-element timestamp))
         (text (funcall org-countdown-format-duration-function ts)))
    (overlay-put ov 'after-string
                 ;; Add a space so the text isn't jammed against the timestamp
                 (concat " " (propertize text 'face 'org-countdown-overlay)))
    (overlay-put ov 'keymap org-countdown-overlay-map)
    (overlay-put ov 'timestamp ts)
    (push ov org-countdown--overlays)))

(defun org-countdown--style-planning (planning)
  "Add appropriate overlays to planning element PLANNING.
Which elements overlays are added to is controlled by
`org-countdown-style-timestamp-types'."
  (when-let* (((memq 'deadline org-countdown-style-timestamp-types))
              (deadline (org-element-property :deadline planning)))
    (org-countdown--style-timestamp deadline))
  (when-let* (((memq 'scheduled org-countdown-style-timestamp-types))
              (scheduled (org-element-property :scheduled planning)))
    (org-countdown--style-timestamp scheduled)))

(defun org-countdown--update-overlays ()
  "Update all `org-countdown' overlays."
  ;; Remove dead overlays
  (setq org-countdown--overlays
        (cl-delete-if-not #'overlay-buffer org-countdown--overlays))
  (cl-loop for ov in org-countdown--overlays
           for timestamp = (overlay-get ov 'timestamp)
           for text = (org-countdown--format-duration timestamp)
           ;; We have to account for the possibility that `text' is nil because
           ;; the countdown has elapsed and `org-countdown-link-elapsed-behavior' is
           ;; `none'.
           when text do
           (overlay-put ov 'display text)
           else do
           (delete-overlay ov))
  (org-countdown--cancel-timer-maybe))

(defun org-countdown--start-timer-maybe ()
  "Start timer and store timer in `org-countdown--timer' if needed."
  (unless org-countdown--timer
    (setq org-countdown--timer
          (run-with-timer 60 60 #'org-countdown--update-overlays))))

(defun org-countdown--cancel-timer-maybe ()
  "Cancel `org-countdown--timer' if there are no overlays left."
  (when (and org-countdown--timer
             (not org-countdown--overlays))
    (cancel-timer org-countdown--timer)
    (setq org-countdown--timer nil)))

;;;###autoload
(defun org-countdown-style-links ()
  "Style all `countdown:' links in the buffer."
  (interactive)
  (org-countdown--register)
  (org-countdown-clear)
  (org-element-map (org-element-parse-buffer) 'link
    #'org-countdown--style-link))

;;;###autoload
(defun org-countdown-style-timestamps ()
  "Style all timestamps in the buffer.
Which timestamps are styled is controlled by
`org-countdown-style-timestamp-types'."
  (interactive)
  (org-countdown-clear)
  (org-element-map (org-element-parse-buffer) 'planning
    #'org-countdown--style-planning)
  (when (memq 'active org-countdown-style-timestamp-types)
    (org-element-map (org-element-parse-buffer) 'timestamp
      #'org-countdown--style-timestamp)))

(defun org-countdown-clear ()
  "Clear all countdown overlays in the buffer."
  (interactive)
  ;; List will get cleaned up in timer
  (dolist (ov org-countdown--overlays)
    (when (eq (overlay-buffer ov) (current-buffer))
      (delete-overlay ov))))

(provide 'org-countdown)

;;; org-countdown.el ends here
