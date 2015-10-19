;;; linum-relative.el --- display relative line number in emacs.

;; Copyright (c) 2013 Yen-Chin, Lee.
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: converience
;; X-URL: http://github.com/coldnew/linum-relative
;; Version: 0.4

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; [![MELPA](http://melpa.org/packages/linum-relative-badge.svg)](http://melpa.org/#/linum-relative)
;; [![MELPA Stable](http://stable.melpa.org/packages/linum-relative-badge.svg)](http://stable.melpa.org/#/linum-relative)

;; ![Screenshot](https://github.com/coldnew/linum-relative/raw/master/screenshot/screenshot1.jpg)
;;
;; linum-relative lets you display relative line numbers for current buffer.
;;

;;; Installation:

;; If you have `melpa` and `emacs24` installed, simply type:
;;
;;     M-x package-install linum-relative
;;
;; And add the following to your .emacs
;;
;;     (require 'linum-relative)

;;; Setup & Tips:

;; The non-interactive function *linum-on* (which should already be built into recent GNU Emacs distributions), turns on side-bar line numbering:
;;
;;     (linum-on)
;;
;; and alternatively, by enabling `linum-mode`:
;;
;;     M-x linum-mode
;;
;; Relative line numbering should already be enabled by default (by installing this package), following *linum-on* or enabling *linum-mode*. One can also use the *linum-relative-toggle* interactive function to switch between relative and non-relative line numbering:
;;
;;     M-x linum-relative-toggle
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'linum)

(defgroup linum-relative nil
  "Show relative line numbers on fringe."
  :group 'convenience)

;;;; Faces
(defface linum-relative-current-face
  '((t :inherit linum :foreground "#CAE682" :background "#444444" :weight bold))
  "Face for displaying current line."
  :group 'linum-relative)

;;;; Customize Variables

(defcustom linum-relative-current-symbol "0"
  "The symbol you want to show on the current line, by default it is 0.
   You can use any string like \"->\". If this variable is empty string,
linum-releative will show the real line number at current line."
  :type 'string
  :group 'linum-relative)

(defcustom linum-relative-plusp-offset 0
  "Offset to use for positive relative line numbers."
  :type 'integer
  :group 'linum-relative)

(defcustom linum-relative-format "%3s"
  "Format for each line. Good for adding spaces/paddings like so: \" %3s \""
  :type 'string
  :group 'linum-relative)

(defcustom linum-relative-with-helm nil
  "Set t if you want to integrate with linum-relative and helm-mode."
  :type 'boolean
  :group 'linum-relative)

;;;; Internal Variables

(defvar linum-relative-last-pos 0
  "Store last position.")

(defvar linum-relative-user-format linum-format
  "Store the users linum-format")

;;;; helm support
(defvar helm-buffer)
(defvar helm-candidate-separator)
(defvar helm-alive-p)
(declare-function with-helm-buffer "ext:helm-lib.el" (&rest body))
(declare-function helm-candidate-number-at-point "ext:helm.el")
(declare-function helm-pos-header-line-p "ext:helm.el")

(defun linum-relative-enable-helm-support ()
  "Return t if user want to integrate linum-relative with helm."
  (and (bound-and-true-p helm-alive-p) linum-relative-with-helm))

(defun linum-relative-for-helm ()
  (when linum-relative-with-helm
    (with-helm-buffer
      (make-local-variable 'linum-relative-last-pos))
    (linum-update helm-buffer)))

(add-hook 'helm-move-selection-after-hook 'linum-relative-for-helm)

;;;; Advices
(defadvice linum-update (before relative-linum-update activate)
  "This advice get the last position of linum."
  (if (linum-relative-enable-helm-support)
      (setq linum-relative-last-pos (helm-candidate-number-at-point))
    (setq linum-relative-last-pos (line-number-at-pos))))

;;;; Functions
(defun linum-relative (line-number)
  (when (linum-relative-enable-helm-support)
    (with-helm-buffer
      (if (looking-at helm-candidate-separator)
          (setq line-number (save-excursion
                              (forward-line 1) (helm-candidate-number-at-point)))
        (setq line-number (helm-candidate-number-at-point)))))
  (let* ((diff1 (abs (- line-number linum-relative-last-pos)))
         (diff (if (minusp diff1)
                   diff1
                 (+ diff1 linum-relative-plusp-offset)))
         (current-p (= diff linum-relative-plusp-offset))
         (current-symbol (if (and linum-relative-current-symbol current-p)
                             (if (string= "" linum-relative-current-symbol)
                                 (number-to-string line-number)
                               linum-relative-current-symbol)
                           (number-to-string diff)))
         (face (if current-p 'linum-relative-current-face 'linum)))
    (if (and (linum-relative-enable-helm-support)
             (with-helm-buffer
               (or (looking-at helm-candidate-separator)
                   (eq (point-at-bol) (point-at-eol))
                   (helm-pos-header-line-p))))
        (propertize (format linum-relative-format current-symbol) 'invisible t)
      (propertize (format linum-relative-format current-symbol) 'face face))))


(defun linum-relative-on ()
  "Turn ON linum-relative."
  (unless (eq linum-format 'linum-relative)
    (setq linum-relative-user-format linum-format)
    (setq linum-format 'linum-relative)))

(defun linum-relative-off ()
  "Turn OFF linum-relative."
  (setq linum-format linum-relative-user-format))

(defun linum-relative-toggle ()
  "Toggle between linum-relative and linum."
  (interactive)
  (if (eq linum-format 'linum-relative)
      (linum-relative-off)
    (linum-relative-on)))

;;;###autoload
(define-minor-mode linum-relative-mode
  "Display relative line numbers for current buffer."
  :group 'linum-relative
  :global t
  :lighter " LR"
  (if linum-relative-mode
      (progn
        (linum-relative-on)
        (global-linum-mode 1))
    (linum-relative-off)
    (global-linum-mode -1)))


(provide 'linum-relative)
;;; linum-relative.el ends here.
