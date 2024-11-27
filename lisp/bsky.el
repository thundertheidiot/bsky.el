;;; bsky.el --- Bluesky client inside Emacs -*- lexical-binding: t; -*-

;; Author: Thunder <thunder@disroot.org>
;; Version 0.1
;; Package-Requires ((plz "0.9") (dash "2.19.1"))
;; Keywords: bluesky

;;; Commentary:
;; A client for the Bluesky social network implemented in Emacs Lisp.

;;; Code:
(require 'plz)
(require 'dash)

(require 'bsky-api)
(require 'bsky-ui)

(provide 'bsky)
;;; bsky.el ends here
