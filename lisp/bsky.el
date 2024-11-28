;;; bsky.el --- Bluesky client inside Emacs -*- lexical-binding: t; -*-

;; Author: Thunder <thunder@disroot.org>
;; Version 0.1
;; Package-Requires ((plz "0.9") (dash "2.19.1"))
;; Keywords: bluesky

;;; Commentary:
;; A client for the Bluesky social network implemented in Emacs Lisp.

;;; Code:
(require 'bsky-api)
(require 'bsky-ui)

(defun bsky-search ()
  "Search bluesky."
  (interactive)
  (let ((term (completing-read "Search for: " '())))
    (bsky-ui--show-posts (cdar (bsky-api--search term)))))

(provide 'bsky)
;;; bsky.el ends here
