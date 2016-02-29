;;; dired-details-1.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2016  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Hide dired details separately.

;;; Code:


(defconst dired-details-s-parts '(perms links user group size time) "Definitions of detail parts.")

(defcustom dired-details-s-types
  `((size-time  . (size time))
    (all        . ,dired-details-s-parts)
    (no-details . ()))
  "Details combination list."
  :group 'dired
  :type 'file)

(defvar dired-details-s-type (caar dired-details-s-types) "Current type of details combination.")

(defvar dired-details-s-visible-parts (cdr (assq dired-details-s-type dired-details-s-types)) "Current visible parts list.")

(defconst dired-details-s-regexp
  (concat
   "\\(\\([^ ][-r][-w][^ ][-r][-w][^ ][-r][-w][^ ]\\) \\)" ;1,2:permissions
   "\\( *\\([0-9]+\\) +\\)" ;3,4:link count
   "\\(\\([^ ]+\\) +\\)" ;5,6:user
   "\\(\\([^ ]+\\) +\\)" ;7,8:group (7 including space before size)
   "\\(\\([0-9]+\\) \\)" ;9,10:size
   "\\(\\(.+[^ ]\\) +\\)")) ;11,12:time

(defun dired-details-s-foreach-filenames (beg end fun-at-filename)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (ignore-errors
        (when (dired-move-to-filename)
          (if (looking-back dired-details-s-regexp)
              (funcall fun-at-filename))))
      (forward-line 1))))

(defun dired-details-s-set-text-properties (beg end)
  (let ((group-width 0))
    ;; Calculate group name width
    (dired-details-s-foreach-filenames
     beg end
     #'(lambda ()
         (setq group-width (max group-width (length (match-string 8))))))

    ;; Set invisible text properties
    (dired-details-s-foreach-filenames
     beg end
     #'(lambda ()
         (let ((group-end (+ (match-beginning 7) group-width 1)))
           (put-text-property (match-beginning 1)  (match-end 1)  'invisible 'dired-details-s-perms)
           (put-text-property (match-beginning 3)  (match-end 3)  'invisible 'dired-details-s-links)
           (put-text-property (match-beginning 5)  (match-end 5)  'invisible 'dired-details-s-user)
           (put-text-property (match-beginning 7)  group-end      'invisible 'dired-details-s-group)
           (put-text-property group-end            (match-end 9)  'invisible 'dired-details-s-size)
           (put-text-property (match-beginning 11) (match-end 11) 'invisible 'dired-details-s-time))))))

(defun dired-details-s-update-invisibility-spec ()
  (mapc
   #'(lambda (part)
       (funcall
        (if (memq part dired-details-s-visible-parts) 'remove-from-invisibility-spec 'add-to-invisibility-spec)
        (intern (concat "dired-details-s-" (symbol-name part)))))
   dired-details-s-parts))

(defun dired-details-s-toggle-type ()
  (interactive)
  ;; Rotate type
  (let* ((curr-type-def
          (let ((types dired-details-s-types))
            (while (and types (not (eq (caar types) dired-details-s-type))) (setq types (cdr types)))
            types))
         (next-type-def (or (cadr curr-type-def) (car dired-details-s-types))))
    (setq dired-details-s-type (car next-type-def))
    (setq dired-details-s-visible-parts (cdr next-type-def))

    )
  ;; Update invisibility
  (dired-details-s-update-invisibility-spec)
  ;; Refresh buffer
  (revert-buffer))


;;
;; Setup
;;

(defun dired-details-s-set-text-properties-around (orig-fun beg end)
  (dired-details-s-set-text-properties beg end))

(defun dired-details-s-install ()
  (interactive)
  (advice-add 'dired-insert-set-properties :around 'dired-details-s-set-text-properties-around)
  (define-key dired-mode-map "(" 'dired-details-s-toggle-type)
  (add-hook 'dired-mode-hook 'dired-details-s-update-invisibility-spec))

(defun dired-details-s-uninstall ()
  (interactive)
  (advice-remove 'dired-insert-set-properties 'dired-details-s-set-text-properties-around)
  (define-key dired-mode-map "(" 'dired-hide-details-mode)
  (remove-hook 'dired-mode-hook 'dired-details-s-update-invisibility-spec))

(dired-details-s-install)


(provide 'dired-details-s)
;;; dired-details-s.el ends here
