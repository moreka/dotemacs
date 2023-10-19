;; -*- lexical-binding: t; -*-

;;; Put Emacs auto-save and backup files to /tmp
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   auto-save-list-file-prefix emacs-tmp-dir
   auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
   backup-directory-alist `((".*" . ,emacs-tmp-dir)))

(setq tab-width 2
      indent-tabs-mode nil
      compilation-scroll-output t)

(setq create-lockfiles nil)

(electric-pair-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(column-number-mode 1)
(global-auto-revert-mode t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'clipboard-yank)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq use-package-verbose nil)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

(use-package magit :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
	evil-want-C-u-scroll t)
  :config
  (evil-mode t)
  (setq evil-search-module 'evil-search)
  (evil-set-undo-system 'undo-tree))

(use-package evil-collection
  :ensure t
  :config
  (evil-collection-init
   (list 'magit 'dired)))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets/"))
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-keymap (kbd "C-k") 'yas-next-field-or-maybe-expand))

(setq ispell-dictionary "english")

(defun my-yas-try-expanding-auto-snippets ()
  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
	(let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
	  (yas-expand))))

(use-package latex
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :hook ((LaTeX-mode . flyspell-mode)
 	 (LaTeX-mode . LaTeX-math-mode)
	 (LaTeX-mode . TeX-source-correlate-mode)
	 (LaTeX-mode . reftex-mode)
	 (LaTeX-mode . (lambda ()
			 (setq fill-column 100)
			 (auto-fill-mode 1)))
	 (LaTeX-mode . (lambda ()
			 (push (list 'output-pdf "Zathura") TeX-view-program-selection)
			 (setq display-line-numbers-type 'relative)
			 (display-line-numbers-mode)))
	 (LaTeX-mode . (lambda ()
			 (use-package company-auctex :ensure t)
			 (use-package company-reftex :ensure t)
			 (make-local-variable 'company-backends)
			 (setq company-backends
			       '((company-capf company-reftex-labels company-reftex-citations company-dabbrev)))))
	 (LaTeX-mode . (lambda () (LaTeX-add-environments
				   '("theorem" LaTeX-env-label)
				   '("lemma" LaTeX-env-label))))
	 (LaTeX-mode . (lambda () (add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets))))

  :config
  (setq-default TeX-master nil
		TeX-PDF-mode t)
  (setq TeX-parse-self t
	TeX-auto-save t
	reftex-plug-into-AUCTeX t
	font-latex-fontify-script nil
	font-latex-fontify-sectioning 'color)
  (setq reftex-label-alist
	'(("theorem" ?h "thm:" "~\\cref{%s}" t ("theorem" "th.") -2)
	  ("lemma" ?h "lem:" "~\\cref{%s}" t ("lemma" "lem.") -3)
	  ("proposition" ?h "prop:" "~\\cref{%s}" t ("proposition" "prop.") -4)
	  ("assumption" ?h "asm:" "~\\cref{%s}" t ("assumption" "asm.") -5)
	  ("corollary" ?h "cor:" "~\\cref{%s}" t ("corollary" "cor.") -6)
	  AMSTeX))
  (setq reftex-ref-style-default-list '("Cleveref" "Default")))



(use-package smex :ensure t)
(use-package ido-completing-read+
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-ubiquitous-mode 1))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (global-company-mode t))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-directory "~/Research/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; (use-package zenburn-theme :ensure t)
;; (load-theme 'zenburn t)

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file t)
