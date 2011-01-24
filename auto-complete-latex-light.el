;;; auto-complete-latex-light.el --- An abbreviated version of the auto-complete-latex

;; Copyright (C) 2011 tequilasunset

;; Author: tequilasunset <tequilasunset.mac@gmail.com>
;; Keywords: latex, completion
(defconst ac-ll-version "0.0.2")

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

;; Last year, I wrote the auto-complete-latex for completing the
;; latex keywords. It had many useful functions, but in some situations,
;; it degraded PC's performance. So I rewrote a simple version of it.

;; This program provides the two sources for auto-complete-mode
;; under the modes for latex. Provided sources are below:
;;
;; `ac-source-latex-commands-in-same-mode-buffers',
;; `ac-source-latex-dictionary'.
;;
;; Note that this version is not compatible with auto-complete-latex.
;; If you want to use all functions, please try the original version
;; <https://bitbucket.org/tequilasunset/auto-complete-latex>.

;;; Requirements:

;; auto-complete-mode - <http://github.com/m2ym/auto-complete>

;;; Setup:

;; Put files to your load-path and add the following in your init file.
;;
;; (require 'auto-complete-latex-light)
;; (setq ac-ll-dict-directory "/path/to/ac-ll-dict")
;; (add-to-list 'ac-modes 'latex-mode)
;; (add-hook 'latex-mode-hook
;;           (lambda ()
;;             ;; Set the ac-sources you like.
;;             (setq ac-sources
;;                   '(ac-source-filename
;;                     ac-source-latex-dictionary
;;                     ac-source-latex-commands-in-same-mode-buffers
;;                     ac-source-abbrev
;;                     ac-source-dictionary
;;                     ac-source-yasnippet))))
;;
;; If needed, you have to also setup the auto-complete-mode.
;; See more details in <http://cx4a.org/software/auto-complete/>.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'auto-complete)

(defvar ac-ll-dict-directory "~/.emacs.d/ac-ll-dict"
  "*Directory path to store files for latex commands.
All the files in it will be used for `ac-source-latex-dictionary'.")

(defconst ac-ll-command-prefix "\\\\\\([[:alpha:]@]+\\)")
(defconst ac-ll-dictionary-cache (make-hash-table :test 'equal))
(defvar ac-ll-index nil)
(make-variable-buffer-local 'ac-ll-index)

(defun ac-ll-candidate ()
  (let ((i 0)
        (regexp (concat "\\\\\\(" (regexp-quote ac-prefix) "[[:alpha:]@]+\\)"))
        cand cands)
    (save-excursion
      ;; Search backward
      (goto-char ac-point)
      (while (and (or (not (integerp ac-limit)) (< i ac-limit))
                  (re-search-backward regexp nil t))
        (setq cand (match-string-no-properties 1))
        (unless (member cand cands)
          (push cand cands)
          (incf i)))
      ;; Search backward
      (goto-char (+ ac-point (length ac-prefix)))
      (while (and (or (not (integerp ac-limit)) (< i ac-limit))
                  (re-search-forward regexp nil t))
        (setq cand (match-string-no-properties 1))
        (unless (member cand cands)
          (push cand cands)
          (incf i))))
    (nreverse cands)))

(defun ac-ll-incremental-update-index ()
  (let ((ac-limit (or (and (integerp ac-limit) ac-limit) 10)))
    (when (null ac-ll-index)
      (setq ac-ll-index (cons nil nil)))
    ;; Mark incomplete
    (when (car ac-ll-index)
      (setcar ac-ll-index nil))
    (let ((list (cdr ac-ll-index))
          (words (ac-ll-candidate)))
      (dolist (word words)
        (unless (member word list)
          (push word list)
          (setcdr ac-ll-index list))))))

(defun ac-ll-update-index ()
  (dolist (buf (buffer-list))
    (when (and (eq major-mode (buffer-local-value 'major-mode buf))
               (or ac-fuzzy-enable
                   (not (eq buf (current-buffer)))))
      (with-current-buffer buf
        (when (and (not (car ac-ll-index))
                   (< (buffer-size) 1048576))
          ;; Complete index
          (setq ac-ll-index
                (cons t (let ((ac-point (point-min))
                              (ac-prefix "")
                              ac-limit)
                          (ac-ll-candidate)))))))))

(defun ac-ll-candidates ()
  (loop initially (unless ac-fuzzy-enable
                    (ac-ll-incremental-update-index))
        for buf in (buffer-list)
        if (and (or (not (integerp ac-limit)) (< (length cands) ac-limit))
                (derived-mode-p (buffer-local-value 'major-mode buf)))
        append (funcall ac-match-function ac-prefix
                        (cdr (buffer-local-value 'ac-ll-index buf)))
        into cands
        finally return cands))

(defun ac-ll-clear-dictionary-cache ()
  (interactive)
  (clrhash ac-ll-dictionary-cache))

(defun ac-ll-file-dictionary (filename)
  (let ((cache (gethash filename ac-ll-dictionary-cache 'none)))
    (if (and cache (not (eq cache 'none)))
        cache
      (let (result)
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents filename)
            (setq result (split-string (buffer-string) "\n"))))
        (puthash filename result ac-ll-dictionary-cache)
        result))))

(defun ac-ll-dictionary-candidates ()
  (apply #'append
         (mapcar #'ac-ll-file-dictionary
                 (directory-files ac-ll-dict-directory t "^[^.]"))))

;;; Sources

(defvar ac-source-latex-commands-in-same-mode-buffers
  `((prefix . ,ac-ll-command-prefix)
    (candidates . ac-ll-candidates)
    (init . ac-ll-update-index)))

(defvar ac-source-latex-dictionary
  `((prefix . ,ac-ll-command-prefix)
    (candidates . ac-ll-dictionary-candidates)
    (symbol . "d"))
  "Source for latex commands, which are collected from
all the files in `ac-ll-dict-directory'.")

(provide 'auto-complete-latex-light)
;;; auto-complete-latex-light.el ends here
