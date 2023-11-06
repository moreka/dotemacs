;; -*- lexical-binding: t; -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Set default font
(set-face-attribute 'default nil
                    :family "CommitMono"
                    :height 110
                    :weight 'normal
                    :width 'normal)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      window-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      comp-async-report-warnings-errors nil)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
