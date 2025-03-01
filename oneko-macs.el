;;; oneko-macs.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 
;;
;; Author:  <elle@etude>
;; Maintainer:  <elle@etude>
;; Created: March 01, 2025
;; Modified: March 01, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/elle/oneko-macs
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;;; Oneko


(defun oneko-macs--move-mouse-system (pos)
  "Handle mouse movement based on system type at position POS."
  (cl-case system-type
    (gnu/linux
     (cl-case (intern (or (getenv "XDG_SESSION_TYPE") "x11"))
       (wayland
        (user-error "Oneko-macs: Wayland is not supported"))
       (x11
        (if (executable-find "xdotool")
            (make-process
             :name "xdotool"
             :command (list "xdotool" "mousemove"
                            (number-to-string (car pos))
                            (number-to-string (cdr pos))))
          (user-error "Oneko-macs: Please install xdotool")))))
    ((windows-nt cygwin)
     (user-error "Oneko-macs: Windows is not supported"))
    ((darwin berkeley-unix)
     (if (executable-find "cliclick")
         (make-process
          :name "cliclick"
          :command (list "cliclick" (format "m:%d,%d" (car pos) (cdr pos))))
       (user-error "Oneko-macs: Please install cliclick")))))

(defun oneko-macs--move-mouse-to-cursor ()
  (interactive)
  (let ((pos (window-absolute-pixel-position)))
    (when pos
      (oneko-macs--move-mouse-system pos))))

(defun oneko-macs--move-mouse-to-cursor-minibuffer ()
  (let* ((pos (window-absolute-pixel-position))
         (display-height (display-pixel-height))
         (bottom-y (- display-height oneko-macs--mouse-minibuffer-offset)))
    (when pos
      (oneko-macs--move-mouse-system (cons (car pos) bottom-y)))))

(defvar oneko-macs--mouse-minibuffer-offset 50)

(defun oneko-macs--stop-oneko-follower ()
  "Stop oneko and cursor following"
  (interactive)
  (shell-command "pkill oneko")
  (remove-hook 'post-command-hook 'oneko-macs--move-mouse-to-minibuffer)
  (remove-hook 'post-command-hook 'oneko-macs--move-mouse-to-cursor))

(defun oneko-macs--choose-follower ()
  "Choose a friend to follow your cursor"
  (interactive)
  (let* ((options '(("Normal Cat" . ("oneko" "-time" "60000"))
                    ("Tora (Tiger)" . ("oneko" "-tora" "-time" "60000"))
                    ("Sakura (Cherry)" . ("oneko" "-sakura" "-time" "60000"))
                    ("Tomoyo" . ("oneko" "-tomoyo" "-time" "60000"))
                    ("Dog" . ("oneko" "-dog" "-time" "60000"))))
         (choice (completing-read "Choose your follower: " options nil t))
         (movement-type (completing-read "Choose movement type: " 
                                         '("Free" "Minibuffer") nil t))
         (cmd-args (cdr (assoc choice options)))
         (hook (cond ((string= movement-type "Free") 'oneko-macs--move-mouse-to-cursor)
                     ((string= movement-type "Minibuffer") 'oneko-macs--move-mouse-to-cursor-minibuffer))))
    (make-process :name "oneko"
                  :buffer nil
                  :command cmd-args
                  :noquery t)
    (add-hook 'post-command-hook hook)))

(provide 'oneko-macs)
;;; oneko-macs.el ends here
