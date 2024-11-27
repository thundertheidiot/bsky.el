;; -*- lexical-binding: t; -*-
(require 'dash)

(defvar bsky-use-all-the-icons nil 
  "Should bsky.el use all-the-icons in the post renderer.")

(defun bsky-ui--post-to-element (post &optional header-level buf)
  "Create a visual representation of a POST element in buffer BUF returned from bluesky."
  (let ((author (alist-get 'author post))
	(record (alist-get 'record post)))
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
	  )
      (with-current-buffer buf
	(insert (format "*%s %s\n"
			(make-string
			 (or header-level 0)
			 ?*)
			(or author-display-name author-handle)))
	(insert ":PROPERTIES:\n")
	(insert (format ":uri: %s\n" uri))
	(insert (format ":cid: %s\n" cid))
	(insert (format ":author_did: %s\n" author-did))
	(insert (format ":author_handle: %s\n" author-handle))
	(when reply (let ((parent (alist-get 'parent reply))
			  (root (alist-get 'root reply)))
		      (insert (format ":reply: t\n"))
		      (insert (format ":parent_uri: %s\n" (alist-get 'uri parent)))
		      (insert (format ":parent_cid: %s\n" (alist-get 'cid parent)))
		      (insert (format ":root_uri: %s\n" (alist-get 'uri root)))
		      (insert (format ":root_cid: %s\n" (alist-get 'cid root)))
		      ))
	(insert ":END:\n\n")

	(insert (format "%s\n\n" text))

	(if bsky-use-all-the-icons
	    (progn
	      (insert (format "%s %s " (all-the-icons-faicon "reply" :face 'all-the-icons-blue) reply-count))
	      (insert (format "%s %s " (all-the-icons-faicon "recycle" :face 'all-the-icons-blue) repost-count))
	      (insert (format "%s %s " (all-the-icons-faicon "heart" :face 'all-the-icons-red) like-count))
	      (insert (format "%s %s\n\n" (all-the-icons-faicon "quote-right" :face 'all-the-icons-blue) quote-count)))
	  (progn
	    (insert (format "REPLIES: %s " reply-count))
	    (insert (format "REPOSTS: %s " repost-count))
	    (insert (format "LIKES: %s " like-count))
	    (insert (format "QUOTES: %s\n\n" quote-count))))
	))))

(defun bsky-ui--show-posts (posts &optional header-level buf)
  "Call post-to-element on each element of the POSTS list/vector in the buffer BUF.
A new buffer is created if BUF is nil.
HEADER-LEVEL represents the reply depth of the post."
  (let ((buf (or buf (generate-new-buffer "*bluesky view*")))
	(header-level (or header-level 0)))
    (with-current-buffer buf
      (org-mode)
      (cond
       ;; reply thread
       ((string= (ignore-errors (alist-get '$type posts)) "app.bsky.feed.defs#threadViewPost")
	(bsky-ui--post-to-element (alist-get 'post posts))
	(bsky-ui--show-posts (alist-get 'replies posts) (+ header-level 1) buf))
       ;; list of posts e.g. search
       ;; TODO make actual condition
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
If FROM-HERE is set, fetch only this reply thread, not the thread of the root post.
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

(defun bsky-ui--create-post-buffer (&rest properties)
  "Create a buffer to post."
  (let ((buf (generate-new-buffer "*bluesky post*")))
    (with-current-buffer buf
      (org-mode)
      (insert ":PROPERTIES:\n")
      (when properties
	(mapc (lambda (prop)
		(insert (format ":%s: %s\n" (car prop) (cdr prop))))
	      properties))
      (insert ":END:\n")
      (insert "# Write your post below\n")
      (org--hide-drawers (point-min) (point-max)))
    (switch-to-buffer buf)))

(defun bsky-ui--create-reply ()
  "Create post buffer for replying to a post."
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
    (bsky-ui--create-post-buffer
     '(reply . t)
     `(parent_uri . ,parent_uri)
     `(parent_cid . ,parent_cid)
     `(root_uri . ,root_uri)
     `(root_cid . ,root_cid))))

(defun bsky-ui--post-buffer-parse-links (lines)
  "Parse urls and images from the list of LINES."
  (let ((processed-lines '())
	(images '())
	(link-embeds '()))
    (dolist (line lines)
      (cond
       ((string-match "\\[\\[file:\\(.+\\)\\]\\]" line)
	(push (match-string 1 line) images))
       ((string-match "\\[\\[\\(http[|s]://.+\\)\\]\\[\\(.+\\)\\]\\]" line)
	(push `(,(match-string 1 line) ,(match-string 2 line)) link-embeds))
       (t (push line processed-lines))))
    (list (nreverse processed-lines)
	  (nreverse images)
	  (nreverse link-embeds))))

(defun bsky-ui--post-bsky-post-buffer ()
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
	 (lines (s-lines content))
	 ;; remove everything before the first appearance of :END:
	 (lines (apply #'append (cdr (-split-on ":END:" lines))))
	 (content (-filter (lambda (line)
			     (not (string-prefix-p "#" line)))
			   lines))
	 (processed (bsky-ui--post-buffer-parse-links content))
	 (text (mapconcat 'identity (nth 0 processed) "\n"))
	 (images (nth 1 processed))
	 (link-embeds (nth 2 processed)))
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
      (cond
       (images
	(when (> 4 (length images))
	  (warn "Bluesky only supports up to four images per post, only the first four will be used.")
	  (setq images (-take 4 images)))
	(setq record (append record `((embed . (($type . "app.bsky.embed.images")
						(images . ,(mapcar (lambda (image)
								     `((alt . "")
								       (image . ,(alist-get 'blob
											    (bsky-api--upload-file image)))))
								   images))))))))
       (link-embeds
	(when (> 1 (length link-embeds))
	  (warn "Bluesky only supports one link embed, only the first one will be used."))
	(let* ((link (nth 0 link-embeds))
	       (uri (nth 0 link))
	       ;; Title:::Description
	       ;; Both Title and Description at the same time
	       (split (split-string (nth 1 link) ":::"))
	       (description (or (cadr split) (nth 1 link)))
	       (title (car split)))
	  (setq sillyness `(,description ,title))
	  (setq record (append record `((embed . (($type . "app.bsky.embed.external")
						  (external . ((uri . ,uri)
							       (title . ,title)
							       (description . ,description)))))))))
	)
       )
      ;; TODO handle errors
      (let ((response (bsky-api--post text record)))
	(let ((uri (alist-get 'uri response))
	      (cid (alist-get 'cid response)))
	  (org-set-property "uri" uri)
	  (org-set-property "cid" cid)))
      )
    )
  )
