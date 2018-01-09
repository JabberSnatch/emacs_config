
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(defconst modern-cc-mode-path (file-truename "~/.emacs.d/site-lisp/cc-mode-5.33"))
(byte-recompile-directory modern-cc-mode-path)
(add-to-list 'load-path modern-cc-mode-path)


(defun maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (if (eq system-type 'windows-nt)
      (w32-send-sys-command 61488)
    )
  )


;; TODO: "slide-line" function that emulates VS [M-up] [M-down] bindings

;; TODO: both of these should consider // as a blank line
(defun previous-blank-line ()
  "Moves to the previous empty line"
  (interactive)
  (forward-line -1)
  (forward-char)
  (if (search-backward-regexp ".\n[ \t]*\n" nil t)
      (forward-line)
    (goto-char (point-min))
    )
  )
(defun next-blank-line ()
  "Moves to the next empty line"
  (interactive)
  (forward-line)
  (if (search-forward-regexp "^[ \t]*\n." nil t)
      (forward-line -1)
    (goto-char (point-max))
    )
  )

;; TODO: Look into syntax table manual entry
;; any _t suffixed word should be treated as a single word
(defun safe-backward-kill-word ()
  "Kill previous word, just like backward-kill-word but treats newlines char as word beginnings and whitespace as words."
  (interactive "*")
  (let ((whitespace-delete-p) (end-previous-word nil) (end-previous-line nil))
    (save-excursion
      (backward-char)
      (let ((begin (point-marker)))
	(backward-word)
	(forward-word)
	(setq end-previous-word (point-marker))
	(setq whitespace-delete-p (< end-previous-word begin))
	)
      )
    (save-excursion
      (move-end-of-line 0)
      (setq end-previous-line (point-marker))
      )
    (if (> end-previous-line end-previous-word)
	(progn
	  (kill-region end-previous-line (point-marker))
	  (delete-horizontal-space)
	  )
      (if whitespace-delete-p
	  (kill-region end-previous-word (point-marker))
	(backward-kill-word 1)
	)
      )
    )
  )

(defun safe-forward-kill-word ()
  "Kill next word, just like forward-kill-word, but treats newlines char as word ends and whitespace as words."
  (interactive "*")
  (let ((whitespace-delete-p) (begin-next-word nil) (begin-next-line nil))
    (save-excursion
      (forward-char)
      (let ((begin (point-marker)))
	(forward-word)
	(backward-word)
	(setq begin-next-word (point-marker))
	(setq whitespace-delete-p (> begin-next-word begin))
	)
      )
    (save-excursion
      (move-beginning-of-line 2)
      (setq begin-next-line (point-marker))
      )
    (if (< begin-next-line begin-next-word)
	(progn
	  (kill-region (point-marker) begin-next-line)
	  (delete-horizontal-space)
	  )
      (if whitespace-delete-p
	  (kill-region (point-marker) begin-next-word)
	(kill-word 1)
	)
      )
    )
  )

(defun interactive-insert-tab ()
  "Inserts a tab character"
  (interactive "*")
  (insert-tab)
  )


(defalias 'list-buffers 'ibuffer)
  
(global-set-key (kbd "M-p") 'previous-blank-line)
(global-set-key (kbd "M-n") 'next-blank-line)
(global-set-key (kbd "C-'") 'whitespace-mode)
(global-set-key (kbd "<C-backspace>") 'safe-backward-kill-word)
(global-set-key (kbd "M-d") 'safe-forward-kill-word)
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)
(global-set-key (kbd "<C-tab>") 'interactive-insert-tab)

(put 'upcase-region 'disabled nil)



(setq c-echo-syntactic-information-p t)
(c-add-style "yolosquad"
	     '((c-basic-offset . 4)
	       (tab-width . 4)
	       (c-offsets-alist
		(innamespace . 0)
		(substatement-open . 0)
		(comment-intro . 0)
		(inlambda . 0)
		)))

(setq c-default-style
      '((java-mode . "java")
	(awk-mode . "awk")
	(c-mode . "yolosquad")
	(c++-mode . "yolosquad")
	(other . "gnu")))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;(add-hook 'c-mode-hook (lambda () (auto-fill-mode 1)))

(setq make-backup-files nil)
(setq scroll-step 3)

(add-to-list 'default-frame-alist '(font . "Liberation Mono-10.5"))
(set-face-attribute 'default t :font "Liberation Mono-10.5")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "OliveDrab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
;;(set-face-attribute 'font-lock-function-name-face nil :foreground "CadetBlue")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "Sienna")
(set-face-attribute 'font-lock-type-face nil :foreground "SeaGreen4")
;;(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
;;(set-face-attribute 'font-lock-variable-name-face nil :foreground "DodgerBlue3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")
(set-face-attribute 'escape-glyph nil :foreground "coral" :background "firebrick")
(set-foreground-color "burlywood3")
(set-background-color "#161616")
(set-cursor-color "#40FF40")


(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(column-number-mode 1)
;;(desktop-save-mode 1)
(setq desktop-save 'ask-if-new)
(setq desktop-dirname "~/")
(setq desktop-base-file-name ".emacs.desktop")
(setq inhibit-startup-screen 1)
(if (eq desktop-save-mode nil)
    (progn
      (switch-to-buffer "*scratch*")
      )
  )
;;(add-hook 'after-change-major-mode-hook
;;	  (lambda () (setq desktop-save-buffer t)))
;;(global-hl-line-mode 1)
(add-hook 'window-setup-hook
	  (lambda ()
	    (maximize-frame)
	    (split-window-horizontally)))


(if (equal system-type 'windows-nt)
    (progn
      (package-install 'msvc)
      (add-to-list 'load-path (expand-file-name "msvc/" "~/.emacs.d"))
      (require 'msvc)
      (setq w32-pipe-read-delay 0)
      (when (msvc-initialize)
	(msvc-flags-load-db :parsing-buffer-delete-p t)
	(add-hook 'c-mode-common-hook 'msvc-mode-on t)
	)
      (add-hook 'msvc-mode-hook
		(lambda ()
		  (local-unset-key (kbd "<C-f5>"))
		  (local-set-key (kbd "<C-f5>") 'msvc-mode-feature-build-project)
		  ))
      (defun msvc-load-solution (path version &optional target platform)
	(interactive)
	(if (eq target nil)
	    (setq target "Release")
	  )
	(if (eq platform nil)
	    (setq platform "x64")
	  )
	(msvc-activate-projects-after-parse :solution-file path
					    :platform platform
					    :configuration target
					    :version version)
	)
      (defun msvc-load-project (path version &optional target platform)
	(interactive)
	(if (eq target nil)
	    (setq target "Release")
	  )
	(if (eq platform nil)
	    (setq platform "x64")
	  )
	(msvc-activate-projects-after-parse :project-file path
					    :platform platform
					    :configuration target
					    :version version)
	)
      (defun msvc-unload-all ()
	(interactive)
	(kill-matching-buffers "\*MSVC.*\*")
	)
      (if (equal (getenv "HOME") nil)
	  (print "HOME variable is undefined")
	)
      (message "Setup windows environment succesfully")
      )
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (msvc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
