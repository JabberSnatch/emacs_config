
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun previous-blank-line ()
  "Moves to the previous empty line"
  (interactive)
  (forward-line -1)
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

(global-set-key (kbd "M-p") 'previous-blank-line)
(global-set-key (kbd "M-n") 'next-blank-line)
(global-set-key (kbd "C-'") 'whitespace-mode)

(setq c-echo-syntactic-information-p t)
(c-add-style "yolosquad"
	     '((c-basic-offset . 4)
	       (tab-width . 4)
	       (c-offsets-alist
		(innamespace . 0)
		(substatement-open . 0)
		)))

(setq c-default-style
      '((java-mode . "java")
	(awk-mode . "awk")
	(c-mode . "yolosquad")
	(c++-mode . "yolosquad")
	(other . "gnu")))

(setq make-backup-files nil)

(add-to-list 'default-frame-alist '(font . "Liberation Mono-10.5"))
(set-face-attribute 'default t :font "Liberation Mono-10.5")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "CadetBlue")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "OliveDrab")
(set-face-attribute 'font-lock-type-face nil :foreground "SeaGreen4")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "DodgerBlue3")
(set-face-attribute 'escape-glyph nil :foreground "coral" :background "firebrick")
(set-foreground-color "burlywood3")
(set-background-color "#161616")
(set-cursor-color "#40FF40")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(desktop-save-mode 1)
(setq desktop-save 'ask-if-new)
(setq desktop-dirname "~/")
(setq desktop-base-file-name ".emacs.desktop")
(add-hook 'after-change-major-mode-hook
	  (lambda () (setq desktop-save-buffer t)))
;(global-hl-line-mode 1)

(if (equal system-type 'windows-nt)
    (progn
      (if (equal (getenv "HOME") nil)
	  (print "HOME variable is undefined")
	)
      (if (equal (require 'cygwin-mount nil t) nil)
	  (print "cygwin-mount missing from load-path folder")
	)
      (cygwin-mount-activate)
      (setq explicit-shell-file-name "bash")
      (setq shell-file-name explicit-shell-file-name)
      (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
      (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)
      (print "Setup windows environment succesfully")
      )
  )
