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

(defcustom org-countdown-format-duration-function #'org-countdown--format-duration
  "Function to use to format the duration left until a timestamp.
Takes a single argument, the target timestamp, as a `ts' timestamp
struct. Should return a string to display."
  :type 'function)

(defvar-local org-countdown--overlays nil
  "List of `org-countdown' overlays in current buffer.")

(defvar-local org-countdown--timer nil
  "Timer that updates the current buffer's `org-countdown' overlays.")

(defun org-countdown--follow (_path _prefix)
  "Handle following a `countdown:' link."
  (org-countdown--style-link (org-element-context)))

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
  (cl-destructuring-bind
      (&key years days hours minutes &allow-other-keys)
      (ts-human-duration (ts-diff timestamp
                                  (ts-now)))
    (format "⏳ %dd %dh %dm"
            (+ (* 365 years) days)
            hours minutes)))

(defun org-countdown-remove-at-point ()
  "Remove countdown overlay at point."
  (interactive)
  (when-let ((ov (cl-find-if (lambda (ov)
                               (member ov org-countdown--overlays))
                             (overlays-at (point)))))
    (delete-overlay ov)
    (setq org-countdown--overlays (delete ov org-countdown--overlays))))

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
              (timestamp (ts-parse target))
              (text (funcall org-countdown-format-duration-function timestamp)))
    (overlay-put ov 'display text)
    (overlay-put ov 'face 'org-countdown-overlay)
    (overlay-put ov 'keymap org-countdown-overlay-map)
    (overlay-put ov 'timestamp timestamp)
    (push ov org-countdown--overlays)))

(defun org-countdown--update-overlays ()
  "Update overlays in the current buffer."
  (cl-loop for ov in org-countdown--overlays
           for timestamp = (overlay-get ov 'timestamp)
           for text = (org-countdown--format-duration timestamp)
           do
           (overlay-put ov 'display text)))

;;;###autoload
(defun org-countdown-enable ()
  "Style all `countdown:' links in the buffer."
  (interactive)
  (org-countdown--register)
  (org-countdown-clear)
  (org-element-map (org-element-parse-buffer) 'link
    #'org-countdown--style-link)
  (setq org-countdown--timer
        (run-with-timer 60 60 #'org-countdown--update-overlays)))

(defun org-countdown-clear ()
  "Clear all countdown overlays in the buffer."
  (interactive)
  (mapc #'delete-overlay org-countdown--overlays)
  (when org-countdown--timer
    (cancel-timer org-countdown--timer))
  (setq org-countdown--overlays nil
        org-countdown--timer nil))

(provide 'org-countdown)

;;; org-countdown.el ends here
