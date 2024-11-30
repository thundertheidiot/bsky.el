;;; bsky-ui.el --- UI Module for bsky.el -*- lexical-binding: t; -*-
;; Author: Thunder <thunder@disroot.org>

;;; Commentary:
;; bsky.el leverages org-mode features for its user interface

;;; Code:
(require 'dash)

(defvar bsky-use-all-the-icons nil
  "Should bsky.el use all-the-icons in the post renderer.")

(defun bsky-ui--format-post-stats (like reply repost quote)
  "Insert LIKE, REPLY, REPOST and QUOTE counts."
  (if bsky-use-all-the-icons
      (concat
       (format "%s %s " (all-the-icons-faicon "reply" :face 'all-the-icons-blue) reply)
       (format "%s %s " (all-the-icons-faicon "recycle" :face 'all-the-icons-blue) repost)
       (format "%s %s " (all-the-icons-faicon "heart" :face 'all-the-icons-red) like)
       (format "%s %s\n" (all-the-icons-faicon "quote-right" :face 'all-the-icons-blue) quote))
    (concat
     (format "REPLIES: %s " reply)
     (format "REPOSTS: %s " repost)
     (format "LIKES: %s " like)
     (format "QUOTES: %s\n" quote))))

(defun bsky-ui--post-to-element (post &optional header-level buf)
  "Create a visual representation of a POST element in buffer BUF returned from bluesky."
  (set-buffer-file-coding-system 'utf-8) ;; https://docs.bsky.app/docs/advanced-guides/post-richtext
  (let ((author (alist-get 'author post))
	(record (alist-get 'record post))
	(post-embed (ignore-errors (alist-get 'embed post))))
    (let ((buf (or buf (current-buffer)))
	  (uri (alist-get 'uri post))
	  (cid (alist-get 'cid post))
	  (author-did (alist-get 'did author))
	  (author-handle (alist-get 'handle author))
	  (author-display-name (alist-get 'displayName author))
	  (text (alist-get 'text record))
	  (reply (alist-get 'reply record))
	  (reply-count (alist-get 'replyCount post))
	  (repost-count (alist-get 'repostCount post))
	  (like-count (alist-get 'likeCount post))
	  (quote-count (alist-get 'quoteCount post))
	  (embed (alist-get 'embed record))
	  (quote (string= (ignore-errors (alist-get '$type record-embed)) "app.bsky.embed.record")))
      (with-current-buffer buf
	(insert
	 ;; Header
	 (format "%s %s\n"
		 (make-string
		  (or header-level 1)
		  ?*)
		 (or (format "%s <@%s>" author-display-name author-handle) author-handle))
	 ":PROPERTIES:\n"
	 (format ":uri: %s\n" uri)
	 (format ":cid: %s\n" cid)
	 (format ":author_did: %s\n" author-did)
	 (format ":author_handle: %s\n" author-handle))
	(when reply (let ((parent (alist-get 'parent reply))
			  (root (alist-get 'root reply)))
		      (insert (format ":reply: t\n")
			      (format ":parent_uri: %s\n" (alist-get 'uri parent))
			      (format ":parent_cid: %s\n" (alist-get 'cid parent))
			      (format ":root_uri: %s\n" (alist-get 'uri root))
			      (format ":root_cid: %s\n" (alist-get 'cid root)))))
	(when quote (let ((quote-uri (alist-get 'uri (alist-get 'record embed))))
		      (insert
		       (format ":quote_uri: %s\n" quote-uri))))
	(insert ":END:\n\n"
		(format "%s\n\n" text))

	;; TODO links 

	(when embed
	  (cond
	   ;; TODO the order is not enforced
	   ((string= (alist-get '$type embed) "app.bsky.embed.images")
	    (mapc (lambda (img)
		    (bsky-api--insert-blob-as-image (alist-get 'did author) (alist-get '$link (alist-get 'ref (alist-get 'image img))) (point) buf (alist-get 'alt img)))
		  (alist-get 'images embed)))
	   ((string= (alist-get '$type embed) "app.bsky.embed.record") ;; quote post
	    ;; TODO embeds in quoted post?
	    (let* ((post (alist-get 'record post-embed))
		   (author (alist-get 'author post)))
	      (insert (format "#+begin_quote %s\n" (if (alist-get 'displayName author)
						       (format "%s <@%s>"
							       (alist-get 'displayName author)
							       (alist-get 'handle author))
						     (format "@%s" (alist-get 'handle author))))
		      (alist-get 'text (alist-get 'value post))
		      "\n"
		      (bsky-ui--format-post-stats
		       (alist-get 'likeCount post)
		       (alist-get 'replyCount post)
		       (alist-get 'repostCount post)
		       (alist-get 'quoteCount post))
		      "#+end_quote\n\n")))))

	(insert (bsky-ui--format-post-stats like-count reply-count repost-count quote-count)
		"\n")))))

(defun bsky-ui--show-posts (posts &optional header-level buf)
  "Call post-to-element on each element of the POSTS list/vector in the buffer BUF.
A new buffer is created if BUF is nil.
HEADER-LEVEL represents the reply depth of the post."
  (let ((buf (or buf (generate-new-buffer "*bluesky view*")))
	(header-level (or header-level 1)))
    (with-current-buffer buf
      (org-mode)
      (cond
       ;; reply thread
       ((string= (ignore-errors (alist-get '$type posts)) "app.bsky.feed.defs#threadViewPost")
	(bsky-ui--post-to-element (alist-get 'post posts))
	(bsky-ui--show-posts (alist-get 'replies posts) (+ header-level 1) buf))
       ;; list of posts e.g. search
       ;; TODO make actual condition (or (listp) (vectorp))?
       (t
	(mapc (lambda (post)
		(cond
		 ((alist-get '$type post)
		  (let ((innerpost (alist-get 'post post)))
		    (bsky-ui--post-to-element innerpost header-level buf)
		    (bsky-ui--show-posts (alist-get 'replies post) (+ header-level 1) buf)))
		 (t
		  (bsky-ui--post-to-element post header-level buf))))
	      posts)))
      (org--hide-drawers (point-min) (point-max)))
    (switch-to-buffer buf)))

(defun bsky-ui--element-to-post ()
  "Convert currently \"focused\" post to a post element that can be used as a post."
  `((uri . ,(org-entry-get nil "uri"))
    (cid . ,(org-entry-get nil "cid"))
    (author . ((did . ,(org-entry-get nil "author_did"))
	       (handle . ,(org-entry-get nil "author_handle"))))))

(defun bsky-view-post-thread (&optional from-here depth)
  "View the thread of the current post.
If FROM-HERE is set, fetch only this reply thread instead of the root post.
DEPTH specifies the chain depth."
  (interactive)
  (bsky-api--check-authentication)
  (let ((uri (if from-here
		 (org-entry-get nil "uri")
	       (or (org-entry-get nil "root_uri")
		   (org-entry-get nil "uri")))))
    ;; TODO make this async
    (bsky-ui--show-posts (bsky-api--get-post-thread uri
						    :depth (or depth 100)))))

(define-minor-mode bsky-post-mode
  "Minor mode that must be enabled for you to post to bluesky.
This should help prevent mistakes."
  :global nil
  :interactive nil
  :lighter " Bluesky")

(defun bsky-create-post (&rest properties)
  "Create a buffer to post.  Add PROPERTIES to the org property drawer."
  (interactive)
  (set-buffer-file-coding-system 'utf-8) ;; https://docs.bsky.app/docs/advanced-guides/post-richtext
  (let ((buf (generate-new-buffer "*bluesky post*")))
    (with-current-buffer buf
      (org-mode)
      (bsky-post-mode)
      (insert ":PROPERTIES:\n")
      (when properties
	(mapc (lambda (prop)
		(insert (format ":%s: %s\n" (car prop) (cdr prop))))
	      properties))
      (insert ":END:\n")
      (insert "# Write your post below\n")
      (org--hide-drawers (point-min) (point-max)))
    (switch-to-buffer buf)))

(defun bsky-reply-to-current-post ()
  "Create post buffer for replying to a post."
  (interactive)
  (let ((reply (org-entry-get nil "reply"))
	(parent_uri (org-entry-get nil "uri"))
	(parent_cid (org-entry-get nil "cid"))
	(root_uri nil)
	(root_cid nil))
    (if reply
	(progn
	  (setq root_uri (org-entry-get nil "root_uri"))
	  (setq root_cid (org-entry-get nil "root_cid")))
      (progn
	(setq root_uri (org-entry-get nil "uri"))
	(setq root_cid (org-entry-get nil "cid"))))
    (bsky-create-post
     '(reply . t)
     `(parent_uri . ,parent_uri)
     `(parent_cid . ,parent_cid)
     `(root_uri . ,root_uri)
     `(root_cid . ,root_cid))))

(defun bsky-ui--text-parser (content)
  (let ((content content)
	(images '())
	(rich-text '())

	(image-regex (rx
		      "[["
		      "file:"
		      (group-n 1 ;; filename
			(+? nonl))
		      "]"
		      (or
		       "]"
		       (and
			"["
			(group-n 2 ;; alt text
			  (+? nonl))
			"]]"))))

	(tag-regex (rx
		    (not (any "\\"))
		    "#"
		    (group-n 1
		      (+? nonl))
		    (or space line-end)))

	(mention-regex (rx
			(not (any "\\"))
			"@"
			(group-n 1
			  (+? nonl))
			(or space line-end)))

	(url-regex (rx
		    "[["
		    (group-n 1 ;; link
		      "http"
		      (opt "s")
		      "://"
		      (+? nonl))
		    "]"
		    (or
		     "]"
		     (and
		      "["
		      (group-n 2 ;; text
			(+? nonl))
		      "]]")))))

    (while (string-match image-regex content)
      (push (list (match-string 1 content) (match-string 2 content))
	    images)
      (setq content (concat
		     (substring content 0 (match-beginning 0))
		     (substring content (match-end 0)))))

    (while (string-match url-regex content)
      (let* ((url (match-string 1 content))
	     (description (or (match-string 2 content) url)))
	(setq content (concat
		       (substring content 0 (match-beginning 0))
		       description
		       (substring content (match-end 0))))
	(push `((index . ((byteStart . ,(match-beginning 0))
			  (byteEnd . ,(+ (match-beginning 0) (length description)))))
		;; features: [{}]
		(features . ((($type . "app.bsky.richtext.facet#link")
			      (uri . ,url)))))
	      rich-text)))

    (setq bskydebug content)

    (let ((start 0))
      (while (string-match tag-regex content start)
	(push `((index . ((byteStart . ,(- (match-beginning 1) 1))
			  (byteEnd . ,(match-end 1))))
		(features . ((($type . "app.bsky.richtext.facet#tag")
			      (tag . ,(match-string 1 content))))))
	      rich-text)
	(setq start (match-end 1))))

    (let ((start 0))
      (while (string-match mention-regex content start)
	(push `((index . ((byteStart . ,(- (match-beginning 1) 1))
			  (byteEnd . ,(match-end 1))))
		(features . ((($type . "app.bsky.richtext.facet#mention")
			      ;; TODO be smart here
			      (did . ,(alist-get 'did (bsky-api--get-profile (match-string 1 content))))))))
	      rich-text)
	(setq start (match-end 1))))

    ;; TODO figure out link embed

    (list
     content
     (nreverse images)
     (nreverse rich-text))))

(defun bsky-post-current-post (&optional override)
  "Post the currently focused buffer, this function is also used for replies.
OVERRIDE will override the check for a valid post buffer."
  (interactive)
  (unless (or override bsky-post-mode)
    (warn "This is not a bluesky post buffer."))
  (when (and (or override bsky-post-mode) (y-or-n-p "Are you sure you want to post this? "))
    (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
	   (lines (s-lines content))
	   ;; do some parsing to remove unnecessary lines
	   (lines (apply #'append (cdr (-split-on ":END:" lines))))
	   (lines (-filter (lambda (line)
			     (not (or
				   (string-prefix-p "# " line)
				   (string-prefix-p "#+" line))))
			   lines))
	   (content (mapconcat #'identity lines "\n"))
	   (processed (bsky-ui--text-parser content))
	   (text (car processed))
	   (images (nth 1 processed))
	   (rich-text (nth 2 processed)))
      (let ((record nil))
	(when (org-entry-get nil "reply")
	  (let ((root-uri (org-entry-get nil "root_uri"))
		(root-cid (org-entry-get nil "root_cid"))
		(parent-uri (org-entry-get nil "parent_uri"))
		(parent-cid (org-entry-get nil "parent_cid")))
	    (setq record `((reply . ((root . ((uri . ,root-uri)
					      (cid . ,root-cid)))
				     (parent . ((uri . ,parent-uri)
						(cid . ,parent-cid)))))))))
	(when images
	  (when (< 4 (length images))
	    (warn "Bluesky only supports embedding up to four images per post, only the first four will be used.")
	    (setq images (-take 4 images)))
	  (setq images `((embed . (($type . "app.bsky.embed.images")
				   (images . ,(mapcar (lambda (image)
							;; TODO alt (cdr image)
							`((alt . "")
							  (image . ,(alist-get 'blob
									       (bsky-api--upload-file (car image))))))
						      images)))))))

	(when rich-text
	  (setq rich-text `((facets . ,rich-text))))

	(setq record (append record images rich-text))

	(bsky-post-mode -1)
	(let ((response (bsky-api--post text record)))
	  (let ((uri (alist-get 'uri response))
		(cid (alist-get 'cid response)))
	    (org-set-property "uri" uri)
	    (org-set-property "cid" cid)))))))

(provide 'bsky-ui)
;;; bsky-ui.el ends here
