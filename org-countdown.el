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

(require 'org-element)
(require 'ts)

(defvar-local org-countdown--overlays nil)

(defun org-countdown--follow (_path _prefix)
  "Handle following a `countdown:' link."
  (org-countdown--style-link (org-element-context)))

(defun org-countdown--register ()
  "Register `countdown:' link type with Org Mode."
  (org-link-set-parameters
   "countdown"
   :follow #'org-countdown--follow))

(defun org-countdown--style-link (link)
  "If applicable, add an overlay to the link element LINK."
  (when-let* ((type (org-element-property :type link))
              ((string= type "countdown"))
              (target (org-element-property :path link))
              (begin (org-element-begin link))
              (end (org-element-end link))
              (ov (make-overlay begin end))
              (time-left (ts-human-format-duration
                          (ts-diff (ts-parse target)
                                   (ts-now))
                          :abbr))
              (text (format "⏳ %s" time-left)))
    (overlay-put ov 'display text)
    (overlay-put ov 'face '(org-date default))
    (push ov org-countdown--overlays)))

;;;###autoload
(defun org-countdown-enable ()
  "Style all `countdown:' links in the buffer."
  (interactive)
  (org-countdown--register)
  (org-countdown-clear)
  (org-element-map (org-element-parse-buffer) 'link
    #'org-countdown--style-link))

(defun org-countdown-clear ()
  "Clear all countdown overlays in the buffer."
  (interactive)
  (mapc #'delete-overlay org-countdown--overlays)
  (setq org-countdown--overlays nil))

(provide 'org-countdown)

;;; org-countdown.el ends here
