;;; bsky-api.el --- Backend API Module for bsky.el -*- lexical-binding: t; -*-
;; Author: Thunder <thunder@disroot.org>

;;; Commentary:
;; This module contains the backend code for interfacing with bluesky.
;; It mainly consists of GET and POST requests to the server, made using plz.el here.
(require 'dash)
(require 'plz)
(require 'url-util)

;;; Code:
(defcustom bsky-server "https://bsky.social"
  "Bluesky server to connect to.")
(defvar bsky-api--tokens nil
  "Bluesky authentication data returned by the server.")

(defun bsky-api--url (endpoint &optional server &rest args)
  "Helper function to generate urls for api calls to bluesky.
Outputs a url like https://bsky.social/xrpc/ENDPOINT?arg_one=1&arg_two=2.
SERVER optionally replaces the default bluesky server.
ARGS are used as the query parameters, like in the example url."
  (let ((base-url (format "%s/xrpc/%s" (or server bsky-server) endpoint)))
    (if args
	(format "%s?%s" base-url
		(mapconcat 'identity (-non-nil args) "&"))
      base-url)))

(defmacro bsky-api--error-handler (&rest handlers)
  "Macro to help with creating error handlers, see the function `bsky-api--base-errors' for an example on using HANDLERS."
  `(lambda (err)
     (let ((curl-err (plz-error-curl-error err))
	   (response (plz-error-response err)))
       (if response
	   (let ((status (plz-response-status response))
		 (body (plz-response-body response)))
	     (cond
	      ,@(mapcar (lambda (handler)
			  `((= status ,(car handler))
			    ,(cdr handler)))
			handlers)))
	 (signal 'plz-curl-error (list "Curl error" curl-err))))))

(defun bsky-api--base-errors ()
  "Basic error handler sufficient for most API calls."
  (bsky-api--error-handler
   (401 . (let ((response (json-read-from-string body)))
	    (message (format "401 Unauthorized (%s): %s" (alist-get 'error response) (alist-get 'message response)))))
   (400 . (let ((response (json-read-from-string body)))
	    (message (format "400 Bad Request (%s): %s" (alist-get 'error response) (alist-get 'message response)))))))

(defun bsky-api--check-authentication ()
  "Check that the user is authenticated, prompt for authentication if not."
  (unless bsky-api--tokens
    (bsky-authenticate)))

(defun bsky-api--auth-header ()
  "Return the authentication header that is passed on with every request to bluesky."
  `("Authorization" . ,(format "Bearer %s"
			       (alist-get 'accessJwt bsky-api--tokens))))

;; TODO 2 factor auth
;; https://docs.bsky.app/docs/api/com-atproto-server-create-session
(defun bsky-authenticate (identifier &optional password)
  "Auhenticate with bluesky using IDENTIFIER (can be your email or handle) and PASSWORD."
  (interactive "sHandle/Email: ")
  (let* ((password (or password
		       (read-passwd "Password: " '())))
	 (body (json-encode `((identifier . ,identifier)
			      (password . ,password)))))
    (plz 'post (bsky-api--url "com.atproto.server.createSession")
      :headers '(("Content-Type" . "application/json"))
      :body body
      :as #'json-read
      :then (lambda (response)
	      (setq bsky-api--tokens response)
	      (message (format "Successfully authenticated with bluesky as %s" (alist-get 'handle response))))
      :else (bsky-api--base-errors))))

(defun bsky-refresh-authentication ()
  (interactive)
  (bsky-api--check-authentication)
  (let ((refresh-token (alist-get 'refreshJwt bsky-api--tokens)))
    (plz 'post (bsky-api--url "com.atproto.server.refreshSession")
      :headers `(("Content-Type" . "application/json")
		 ,(bsky-api--auth-header))
      :as #'json-read
      :then (lambda (response)
	      (setq bsky-api--tokens response))
      :else (bsky-api--base-errors))))

(defun bsky-helper--iso8601-date ()
  "Return current date in iso8601 format used by bluesky."
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z"))))

(defun bsky-api--post (text &optional extra-record-content)
  "Post TEXT using the createRecord endpoint.
Append EXTRA-RECORD-CONTENT to the record data."
  (bsky-api--check-authentication)
  (let* ((auth (alist-get 'accessJwt bsky-api--tokens))
	 (post (append `((type . "app.bsky.feed.post")
			 (text . ,text)
			 (createdAt . ,(bsky-helper--iso8601-date))
			 (langs . ("en")))
		       extra-record-content)) ;; TODO customize langs
	 (body (json-encode `((repo . ,(alist-get 'did bsky-api--tokens))
			      (collection . "app.bsky.feed.post")
			      (record . ,post)))))
    (plz 'post (bsky-api--url "com.atproto.repo.createRecord")
      :headers `(("Content-Type" . "application/json")
		 ,(bsky-api--auth-header))
      :as #'json-read
      :body body
      :else (bsky-api--base-errors))))

(defun bsky-api--reply-to-post (text root parent &optional extra-record-content)
  "Reply TEXT to PARENT, appending EXTRA-RECORD-CONTENT to the record content.
ROOT is the first post of the thread."
  (bsky-api--check-authentication)
  (let ((root-uri (alist-get 'uri root))
	(root-cid (alist-get 'cid root))
	(parent-uri (alist-get 'uri parent))
	(parent-cid (alist-get 'cid parent)))
    (bsky-api--post text
		    (append `((reply . ((root . ((uri . ,root-uri)
						 (cid . ,root-cid)))
					(parent . ((uri . ,parent-uri)
						   (cid . ,parent-cid))))))
			    extra-record-content)
		    )))

(defun bsky-api--upload-blob (blob mime-type)
  "Upload BLOB with MIME-TYPE to the bluesky server."
  (bsky-api--check-authentication)
  (plz 'post (bsky-api--url "com.atproto.repo.uploadBlob")
    :headers `(,(bsky-api--auth-header)
	       ("Content-Type" . ,mime-type))
    :body-type 'binary
    :body blob
    :as #'json-read
    :else (bsky-api--base-errors)))

(defun bsky-api--get-blob (then did cid)
  "Get blob from the bluesky server, DID is the author did, CID is the blob cid."
  (plz 'get (bsky-api--url "com.atproto.sync.getBlob" nil
			   (format "did=%s" did)
			   (format "cid=%s" cid))
    :headers `(,(bsky-api--auth-header))
    :as 'binary
    :then then))

(defun bsky-api--insert-image-blob (did cid &optional scale)
  "Insert retrieved blob as image"
  (bsky-api--get-blob
   (lambda (data) (interactive)
     (insert-image (create-image data nil t :scale 0.5)))
   did cid))

(defun bsky-api--upload-file (file)
  "Upload FILE contents with the FILE mime-type to the bluesky server.
Convenience wrapper around bsky-api--upload-blob."
  (let ((blob (with-temp-buffer
		(insert-file-contents-literally file nil nil nil t)
		(buffer-substring-no-properties (point-min) (point-max))))
	(mime-type (mailcap-file-name-to-mime-type file)))
    (bsky-api--upload-blob blob mime-type)))


(defun bsky-api--search (term &rest args)
  "Search for TERM, extra params in ARGS."
  (bsky-api--check-authentication)
  (let ((auth (alist-get 'accessJwt bsky-api--tokens))
	(sort (or (plist-get args :sort) nil))
	(since (or (plist-get args :since) nil))
	(until (or (plist-get args :until) nil))
	(mentions (or (plist-get args :mentions) nil))
	(author (or (plist-get args :author) nil))
	(lang (or (plist-get args :lang) nil))
	(domain (or (plist-get args :domain) nil))
	(url (or (plist-get args :url) nil))
	(tag (or (plist-get args :tag) nil))
	(limit (or (plist-get args :limit) nil))
	(cursor (or (plist-get args :cursor) nil)))
    (plz 'get (bsky-api--url "app.bsky.feed.searchPosts" nil
			     ;; TODO url hexify all needed ones
			     ;; For example limit cannot be hexified
			     (format "q=%s" (url-hexify-string term))
			     (if sort (format "sort=%s" sort) nil)
			     (if since (format "since=%s" since) nil)
			     (if until (format "until=%s" until) nil)
			     (if mentions (format "mentions=%s" mentions) nil)
			     (if author (format "author=%s" author) nil)
			     (if lang (format "lang=%s" lang) nil)
			     (if domain (format "domain=%s" domain) nil)
			     (if url (format "url=%s" url) nil)
			     (if tag (format "tag=%s" tag) nil)
			     (if limit (format "limit=%s" limit) nil)
			     (if cursor (format "cursor=%s" cursor) nil))
      :headers `(,(bsky-api--auth-header))
      :as #'json-read)))

(defun bsky-api--get-post-thread (uri &rest args)
  (bsky-api--check-authentication)
  (let ((depth (or (plist-get args :depth) nil))
	(parent-height (or (plist-get args :parent-height) nil)))
    (plz 'get (bsky-api--url "app.bsky.feed.getPostThread" nil
			     (format "uri=%s" uri)
			     (when depth (format "depth=%s" depth))
			     (when parent-height (format "parentHeight=%s" parent-height)))
      :headers `(,(bsky-api--auth-header))
      :as #'json-read)))

(provide 'bsky-api)
;;; bsky-api.el ends here
