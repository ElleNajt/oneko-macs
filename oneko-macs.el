;;; oneko-macs.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 
;;
;; Author: lnajt4@gmail.com
;; Maintainer:  lnajt4@gmail.com
;; Created: March 01, 2025
;; Modified: March 01, 2025
;; Version: 0.0.1
;; Keywords: games
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


;;;; System tools for moving mouse to cursor

(defun oneko-macs--move-mouse-system (pos)
  "Handle mouse movement based on system type at position POS."
  (cl-case system-type
    (gnu/linux
     (cl-case (intern (or (getenv "XDG_SESSION_TYPE") "x11"))
       (wayland
        (user-error "Oneko-macs: Wayland is not supported - PR welcome!"))
       (x11
        (if (executable-find "xdotool")
            (make-process
             :name "xdotool"
             :command (list "xdotool" "mousemove"
                            (number-to-string (car pos))
                            (number-to-string (cdr pos))))
          (user-error "Oneko-macs: Please install xdotool")))))
    ((windows-nt cygwin)
     (user-error "Oneko-macs: Windows is not supported - PR welcome!"))
    ((darwin berkeley-unix)
     (if (executable-find "cliclick")
         (make-process
          :name "cliclick"
          :command (list "cliclick" (format "m:%d,%d" (car pos) (cdr pos))))
       (user-error "Oneko-macs: Please install cliclick")))))


(defun oneko-macs--get-mouse-position ()
  "Get mouse position using xdotool."
  (with-temp-buffer
    (when (= 0 (call-process "xdotool" nil t nil "getmouselocation"))
      (let ((pos-string (buffer-string)))
        (when (string-match "x:\\([0-9]+\\).*y:\\([0-9]+\\)" pos-string)
          (cons (string-to-number (match-string 1 pos-string))
                (string-to-number (match-string 2 pos-string))))))))

;;;; Types of movement
;;;;; Following

(defun oneko-macs--move-mouse-to-cursor ()
  (let ((pos (window-absolute-pixel-position)))
    (when pos
      (oneko-macs--move-mouse-system pos))))

;;;;; Run away mode


(defun oneko-macs--get-furthest-point (pos)
  "Calculate the point furthest from cursor position POS."
  (let* ((display-width (display-pixel-width))
         (display-height (display-pixel-height))
         (cursor-x (car pos))
         (cursor-y (cdr pos))
         (corners (list (cons 0 0)
                        (cons 0 display-height)
                        (cons display-width 0)
                        (cons display-width display-height)))
         (max-dist 0)
         (furthest-point nil))
    (dolist (corner corners)
      (let ((dist (+ (expt (- (car corner) cursor-x) 2)
                     (expt (- (cdr corner) cursor-y) 2))))
        (when (> dist max-dist)
          (setq max-dist dist
                furthest-point corner))))
    furthest-point))

(defun oneko-macs--move-mouse-away-from-cursor ()
  "Move mouse to the point furthest from the cursor."
  (let ((pos (window-absolute-pixel-position)))
    (when pos
      (oneko-macs--move-mouse-system 
       (oneko-macs--get-furthest-point pos)))))

;;;;; Minibuffer mode

(defun oneko-macs--move-mouse-to-cursor-minibuffer ()
  (let* ((pos (window-absolute-pixel-position))
         (display-height (display-pixel-height))
         (bottom-y (- display-height oneko-macs--mouse-minibuffer-offset)))
    (when pos
      (oneko-macs--move-mouse-system (cons (car pos) bottom-y)))))

(defvar oneko-macs--mouse-minibuffer-offset 50)
;;;;; Random walk 

(defun oneko-macs--move-mouse-random ()
  "Move mouse to a random position on screen with given probability."
  (when (= (random 20) 0)  ; Only proceed 5% of the time
    (let* ((display-width (display-pixel-width))
           (display-height (display-pixel-height))
           (random-x (random display-width))
           (random-y (random display-height)))
      (message "Moving to random position: %s %s" random-x random-y)
      (oneko-macs--move-mouse-system (cons random-x random-y)))))

(defun oneko-macs--move-mouse-to-cursor-edges ()
  (let ((pos (window-absolute-pixel-position)))
    (when (and pos (car pos) (cdr pos)) ; Make sure we have valid coordinates
      (oneko-macs--move-mouse-along-edges pos))))

;;;;; Edges
;; not finished

(defun oneko-macs--move-mouse-along-edges (pos)
  "Move mouse to POS along screen edges."
  (let* ((current-pos (oneko-macs--get-mouse-position))
         (current-x (and current-pos (car current-pos)))
         (current-y (and current-pos (cdr current-pos)))
         (target-x (car pos))
         (target-y (cdr pos))
         (display-width (display-pixel-width))
         (display-height (display-pixel-height))
         (edge-points '()))
    

    (when (and current-x current-y) ; Only proceed if we have valid current position
      
      ;; Create path along edges
      (cond
       ;; If currently on left edge
       ((< current-x 30)
        (push (cons 0 target-y) edge-points)
        (push (cons target-x target-y) edge-points))
       ;; If currently on right edge
       ((> current-x (- display-width 30))
        (push (cons display-width target-y) edge-points)
        (push (cons target-x target-y) edge-points))
       ;; If currently on top/bottom
       (t
        (push (cons target-x current-y) edge-points)
        (push (cons target-x target-y) edge-points)))
      
      ;; Move through each point
      (dolist (point edge-points)
        (oneko-macs--move-mouse-system point)))))


;;;; Menu
(defun oneko-macs--choose-oneko ()
  "Choose a friend to follow your cursor"
  (interactive)
  (oneko-macs--stop-oneko)
  (let* ((options '(("Normal Cat" . ("oneko" "-time" "60000"))
                    ("Tora (Tiger)" . ("oneko" "-tora" "-time" "60000"))
                    ("Sakura (Cherry)" . ("oneko" "-sakura" "-time" "60000"))
                    ("Tomoyo" . ("oneko" "-tomoyo" "-time" "60000"))
                    ("Dog" . ("oneko" "-dog" "-time" "60000"))))
         (choice (completing-read "Choose your follower: " options nil t))
         (movement-type (completing-read "Choose movement type: " 
                                         '("Follow Closely" "Minibuffer" "Random" "Run Away") nil t))
         (cmd-args (cdr (assoc choice options)))
         (hook (cond ((string= movement-type "Follow Closely") 'oneko-macs--move-mouse-to-cursor)
                     ((string= movement-type "Minibuffer") 'oneko-macs--move-mouse-to-cursor-minibuffer)
                     ((string= movement-type "Random") 'oneko-macs--move-mouse-random)
                     ((string= movement-type "Run Away") 'oneko-macs--move-mouse-away-from-cursor))))
    (make-process :name "oneko"
                  :buffer nil
                  :command cmd-args
                  :noquery t)
    (add-hook 'post-command-hook hook)))

;;;; Cleanup

(defun oneko-macs--stop-oneko ()
  "Stop oneko and cursor following"
  (interactive)
  (shell-command "pkill oneko")
  (remove-hook 'post-command-hook 'oneko-macs--move-mouse-random)
  (remove-hook 'post-command-hook 'oneko-macs--move-mouse-away-from-cursor)
  (remove-hook 'post-command-hook 'oneko-macs--move-mouse-to-minibuffer)
  (remove-hook 'post-command-hook 'oneko-macs--move-mouse-to-cursor)
  (remove-hook 'post-command-hook 'oneko-macs--move-mouse-to-cursor-edges))

(provide 'oneko-macs)
;;; oneko-macs.el ends here
