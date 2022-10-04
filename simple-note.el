;;; simple-note.el --- simple configuration for taking notes  -*- lexical-binding:t -*-

(defcustom simple-note-directory nil
  "The directory for notes."
  :group 'simple-note
  :type 'file)

(defcustom simple-note-cache-file (convert-standard-filename
				   (concat (file-name-directory user-emacs-directory) ".simple-note-cache"))
  "The file for simple-note cache."
  :group 'simple-note
  :type 'file)

(defcustom simple-note-enable-load-cache-from-file t
  "Enable simple-note to load cache from file."
  :group 'simple-note
  :type 'boolean)

(defcustom simple-note-enable-save-cache-befor-exit-emacs t
  "Enable simple-note to save cache before exiting emacs."
  :group 'simple-note
  :type 'boolean)

(defvar simple-note-cache nil
  "The cache containing org id and relevant information for simple note.")

(defvar simple-note-file-reference nil
  "The cache for file reference.")

(defvar simple-note-edit-mode-map (make-sparse-keymap)
  "`simple-note-edit-mode' keymap")

(define-minor-mode simple-note-edit-mode
  "A minor mode for simple-note. It is mainly used for creating
a new node by `simple-note-create-new-node'.
  "
  :global nil :group 'simple-note simple-note-edit-mode-map)

;;;###autoload
(defun simple-note-completing--read (prompt collection &optional predicate require-match
					    initial-input hist def inherit-input-method)
  "The function returns the cons cell object to which the selected candidate maps.

The return value is (choice . mapped-value), where choice is the selected candidate and
the mapped-value is the matching id value.
  "
  (when (hash-table-p collection)
    (let* ((choice (completing-read prompt
				    (lambda (str pred action)
				      (if (eq action 'metadata)
					  `(metadata
					    (annotation-function . ,(lambda (str)
								      (let ((value (gethash str collection)))
									(if (length> value 1)
									    (format " multiple values")
									  (let* ((id (caar value))
										 (data (cdar value))
										 (file (nth 1 data))
										 (tags (nth 2 data)))
									    (format " id: %s, file: \"%s\", tags: %s" id file tags)))))))
					(complete-with-action action collection str pred)))
				    predicate require-match initial-input hist def inherit-input-method))
	   (mapped-value (gethash choice collection)))
      (cond
       ((not mapped-value) (cons choice nil))
       ((length= mapped-value 1) (cons choice (car mapped-value)))
       (t (let ((second-choice (completing-read (format "[Second choice for %s] %s" choice prompt)
						(lambda (str pred action)
						  (if (eq action 'metadata)
						      `(metadata
							(annotation-function . ,(lambda (str)
										  (let* ((value (assoc str mapped-value))
											 (id (car value))
											 (data (cdr value))
											 (file (nth 1 data))
											 (tags (nth 2 data)))
										    (format " id: %s, file: \"%s\", tags: %s" id file tags)))))
						    (complete-with-action action mapped-value str pred)))
						predicate require-match initial-input hist def inherit-input-method)))
	    (cons choice (assoc second-choice mapped-value))))))))

(defvar simple-note-completing-read-function #'simple-note-completing--read
  "The function of `simple-note-completing--read'.")

;;;###autoload
(defun simple-note--search ()
  "The main procedure that is invoked by `simple-note-search'."
  (let ((selection (if (use-region-p)
		       (buffer-substring-no-properties (region-beginning) (region-end)))))
    (grep (read-shell-command "Search in directory: "
			      (format "grep -rnH --color=always --include=*.org '%s' \"%s\""
				      (or selection "") (or simple-note-directory ""))))))

(defvar simple-note-search-function #'simple-note--search
  "The function of `'simple-note--search'.")

;;;###autoload
(defun simple-note-search ()
  "Search notes in `simple-note-directory'."
  (interactive)
  (funcall simple-note-search-function))

;;;###autoload
(defun simple-note-query-all--nodes ()
  "The main procedure that is invoked by `simple-note-query-all-nodes'."
  (grep (read-shell-command "Query command: "
			    (format "grep -rnH --include=\"*.org\" --color=always -C 5 '^\s*\\:ID\\:\\(.*\\-\\)\\{4\\}[a-zA-Z0-9]*' \"%s\""
				    (or simple-note-directory "")))))

(defvar simple-note-query-all-node-function #'simple-note-query-all--nodes
  "The function of `simple-note-query-all--nodes'.")

;;;###autoload
(defun simple-note-query-all-nodes ()
  "Query all nodes recursively in `simple-note-directory'."
  (interactive)
  (funcall simple-note-query-all-node-function))

;;;###autoload
(defun simple-note-query-all-node--reference ()
  "The main procedure that is invoked by `simple-note-query-all-node-reference'."
  (grep (read-shell-command "Query command: "
			    (format "grep -rnH --include=\"*.org\" --color=always -C 5 '\\:\\?[iI][dD]\\:\\(.*\\-\\)\\{4\\}[a-zA-Z0-9]*' \"%s\""
				    (or simple-note-directory "")))))

(defvar simple-note-query-all-node-reference-function #'simple-note-query-all-node--reference
  "The function of `simple-note-query-all-node--reference'.")

;;;###autoload
(defun simple-note-query-all-node-reference ()
  "Query all node reference in `simple-note-directory'."
  (interactive)
  (funcall simple-note-query-all-node-reference-function))

;;;###autoload
(defun simple-note-query-node--reference ()
  "The main procedure that is invoked by `simple-note-query-node-reference'."
  (require 'org)
  (let* ((id (cadr (funcall simple-note-completing-read-function "Select a node: "
			    (funcall simple-note-build-info-function simple-note-cache) nil t))))
    (grep (read-shell-command "Query command: "
			      (format "grep -rnH --color=always --include=\"*.org\" -C 5 '%s' \"%s\"" (or id "")
				      simple-note-directory)))))

(defvar simple-note-query-node-reference-function #'simple-note-query-node--reference
  "The function of `simple-note-query-node--reference'.")

;;;###autoload
(defun simple-note-query-node-reference ()
  "Query node reference in `simple-note-directory'."
  (interactive)
  (funcall simple-note-query-node-reference-function))

;;;###autoload
(defun simple-note-load-org-id--locations (&optional force-rescan)
  "Force rescan notes in `simple-note-directory' and then call `org-id-update-id-locations'
to update `org-id-locations'."
  (require 'org-id)
  (if (not force-rescan)
      (unless org-id-files
	(if (file-exists-p org-id-locations-file)
	    (org-id-locations-load)
	  (if simple-note-directory (org-id-update-id-locations (directory-files-recursively simple-note-directory "\\.org$")))))
    (if simple-note-directory (org-id-update-id-locations (directory-files-recursively simple-note-directory "\\.org$")))))

(defvar simple-note-load-org-id-locations-function #'simple-note-load-org-id--locations
  "The function of `simple-note-load-org-id--locations'.")

;;;###autoload
(defun simple-note-load-org-id-locations (&optional arg)
  "Load `org-id-locations-file'.

With a prefix argument provided or set a non-nil value, the function will
direct invoke `org-id-update-id-locations' to update `org-id-locations' if `simple-note-directory'
is not nil, otherwise direct use `org-id-locations', and invoke `org-id-update-id-locations' if it's nil.
  "
  (interactive "P")
  (funcall simple-note-load-org-id-locations-function arg))

;;;###autoload
(defun simple-note-compose-mapped--value (id file)
  "Compose the mapped-value for the id."
  (let* ((heading (org-entry-get nil "ITEM"))
	 (tags (org-entry-get (point) "TAGS")))
    (list heading file tags)))

(defvar simple-note-compose-mapped-value-function #'simple-note-compose-mapped--value
  "The function of `simple-note-compose-mapped--value'.")

;;;###autoload
(defun simple-note-iterate-entry-in--file (cache id file)
  "The function is invoked when each org id is parsed.

cache is the hash table into which to be put our data.
The id value and the file are used for extracting useful information that is
to be put into cache.
"
  (puthash id (funcall simple-note-compose-mapped-value-function id file) cache))

(defvar simple-note-iterate-entry-in-file-function #'simple-note-iterate-entry-in--file
  "The function of `simple-note-iterate-entry-in--file'.")

;;;###autoload
(defun simple-note-genetate-file--reference ()
  "Generate a file-reference cache, which contains id lists, each file associlates with an id list."
  (when (file-exists-p org-id-locations-file)
    (let ((file-ref (make-hash-table :test 'equal))
	  (tmp-org-id-locations nil))
      (with-temp-buffer
	(condition-case nil
	    (progn
	      (insert-file-contents org-id-locations-file)
	      (setq tmp-org-id-locations (read (current-buffer)))
	      (let ((loc (file-name-directory org-id-locations-file)))
		(mapc (lambda (item)
			(unless (file-name-absolute-p (car item))
			  (setf (car item) (expand-file-name (car item) loc))))
		      tmp-org-id-locations)))
	  (error
	   (message "Could not read `org-id-values' from %s, setting it to nil"
		    org-id-locations-file))))
      (dolist (value tmp-org-id-locations)
	(puthash (car value) (cdr value) file-ref))
      file-ref)))

(defvar simple-note-genetate-file-reference-function #'simple-note-genetate-file--reference
  "The function of `simple-note-genetate-file--reference'.")

;;;###autoload
(defun simple-note-generate-file--list ()
  (when (file-exists-p org-id-locations-file)
    (let ((tmp-org-id-locations nil))
      (with-temp-buffer
	(condition-case nil
	    (progn
	      (insert-file-contents org-id-locations-file)
	      (setq tmp-org-id-locations (read (current-buffer)))
	      (let ((loc (file-name-directory org-id-locations-file)))
		(mapc (lambda (item)
			(unless (file-name-absolute-p (car item))
			  (setf (car item) (expand-file-name (car item) loc))))
		      tmp-org-id-locations)))
	  (error
	   (message "Could not read `org-id-values' from %s, setting it to nil"
		    org-id-locations-file))))
      (mapcar 'car tmp-org-id-locations))))

(defvar simple-note-generate-file-list-function #'simple-note-generate-file--list
  "The function of `simple-note-generate-file--list'.")

;;;###autoload
(defun simple-note-generate--cache (file-list)
  "Generate a cache, which is a hash table consisting of org id and relevant information, from file-list."
  (when file-list
    (let* ((cache (make-hash-table :test 'equal))
	   (id-regexp (rx (seq bol (0+ (any "\t ")) ":ID:" (1+ " ") (not (any " "))))))
      (with-temp-buffer
	(org-mode)
	(dolist (file file-list)
	  (when (file-exists-p file)
	    (insert-file-contents file nil nil nil 'replace)
	    (let ((case-fold-search t))
	      (org-with-point-at 1
		(while (re-search-forward id-regexp nil t)
		  (when (org-at-property-p)
		    (funcall simple-note-iterate-entry-in-file-function cache (org-entry-get nil "ID") file))))))))
      cache)))

(defvar simple-note-generate-cache-function #'simple-note-generate--cache
  "The function builds `simple-note-cache'.")

;;;###autoload
(defun simple-note-build--info (cache)
  "Build info for completing-read candidates from the given cache."
  (when (hash-table-p cache)
    (let ((info-hash (make-hash-table :test 'equal)))
      (maphash (lambda (key value)
		 (let* ((heading (nth 0 value))
			(file (nth 1 value))
			(info (or heading (file-name-nondirectory file)))
			(mapped-value (cons key value)))
		   ;; Assume the key is not unique in normal scenario, consing its values for second choices.
		   (puthash info (cons mapped-value (gethash info info-hash)) info-hash)))
	       cache)
      info-hash)))

(defvar simple-note-build-info-function #'simple-note-build--info
  "The function of `simple-note-build--info'.")

;;;###autoload
(defun simple-note--insert ()
  "The main procedure that is invoked by `simple-note-insert-node'."
  (require 'org)
  (let* ((result (or (funcall simple-note-completing-read-function
			      "Select a node: " (funcall simple-note-build-info-function simple-note-cache) nil (use-region-p))))
	 (choice (car result))
	 (id (if (cdr result) (cadr result))))
    (if id ;; The node exists.
	(let* ((description choice)
	       (beg nil)
	       (end nil))
	  (when (use-region-p)
	    (setq beg (region-beginning)
		  end (region-end)
		  description (buffer-substring-no-properties beg end)))
	  (let ((node (read-string "Insert: " (org-link-make-string (concat "id:" id) description))))
	    (if (and beg end)
		(delete-region beg end))
	    (insert node)))
      (let ((description choice)) ;; The node doesn't exist, create a new one.
	(setq id (org-id-uuid))
	(insert (org-link-make-string (concat "id:" id) description))
	(funcall simple-note-create-new-node-function id)))))

(defvar simple-note-insert-function #'simple-note--insert
  "The function of `simple-note--insert'.")

;;;###autoload
(defun simple-note-insert-node ()
  "Insert a simple-note node."
  (interactive)
  (funcall simple-note-insert-function))

;;;###autoload
(defun simple-note-update--cache ()
  "The main procedure that is invoked by `simple-note-update-cache'."
  (setq simple-note-file-reference
	(funcall simple-note-genetate-file-reference-function)
	simple-note-cache
	(if (and simple-note-enable-load-cache-from-file (file-exists-p simple-note-cache-file))
	    (funcall simple-note-load-cache-from-file-function simple-note-cache-file)
	  (funcall simple-note-generate-cache-function (funcall simple-note-generate-file-list-function)))))

(defvar simple-note-update-cache-function #'simple-note-update--cache
  "The function of `simple-note-update--cache'.")

;;;###autoload
(defun simple-note-update-cache ()
  "Update the cache used for simple-note."
  (interactive)
  (funcall simple-note-update-cache-function))

;;;###autoload
(defun simple-note-on--update (arg)
  "The main procedure that is invoked by `simple-note-update'.

With a prefix arg, meaning force simple-note to re-scan org files and update the relevant cache.
"
  (funcall simple-note-load-org-id-locations-function arg)
  (if arg
      (progn
	(delete-file simple-note-cache-file)
	(funcall simple-note-update-cache-function)
	(funcall simple-note-save-cache-to-file-function simple-note-cache simple-note-cache-file)))
  (funcall simple-note-update-cache-function))

(defvar simple-note-on-update-function #'simple-note-on--update
  "The function of `simple-note-on--update'.")

;;;###autoload
(defun simple-note-update(&optional arg)
  "Initialize simple-note by creating a cache for later use.

With a prefix argument, it will force simple-note to update cache.
"
  (interactive "P")
  (funcall simple-note-on-update-function arg))

;;;###autoload
(defun simple-note--initialize ()
  "Initialize simple-note."
  (require 'org-id)
  (unless org-id-track-globally
    (error "Please turn on `org-id-track-globally' if you want to track IDs"))
  (funcall simple-note-load-org-id-locations-function)
  (funcall simple-note-update-cache-function)
  (funcall simple-note-enable-auto-update-cache-function)
  (if simple-note-enable-save-cache-befor-exit-emacs
      (add-hook 'kill-emacs-hook 'simple-note-save-cache-to-file)))

(defvar simple-note-initialize-function #'simple-note--initialize
  "The function of `simple-note--initialize'.")

;;;###autoload
(defun simple-note-on-org-id-added (id file)
  "The function is designed for being invoked when `org-id-add-location' is invoked.

It should be a function being passed to `advice-add'.
  "
  (funcall simple-note-add-to-node-cache-function id file nil))

;;;###autoload
(defun simple-note-enable-auto-update--cache (&optional disable)
  "The main procedure that is invoked by `simple-note-enable-auto-update-cache'."
  (if (not disable)
      (if (not (advice-member-p #'simple-note-on-org-id-added 'org-id-add-location))
	  (advice-add 'org-id-add-location :after #'simple-note-on-org-id-added))
    (advice-remove 'org-id-add-location #'simple-note-on-org-id-added)))

(defvar simple-note-enable-auto-update-cache-function #'simple-note-enable-auto-update--cache
  "The function of `simple-note-enable-auto-update--cache'.")

;;;###autoload
(defun simple-note-enable-auto-update-cache (&optional arg)
  "Enable update the cache of simple-note, whose information originates from `org-id-locations'.

In this function, `org-id-add-location' is hooked by `advice-add', and `simple-note-on-org-id-added'
is passed to `advice-add'. After the invocation of `org-id-add-location', `simple-note-on-org-id-added'
is invoked.

The cache of simple-note will be updated automatically if arg is nil or omitted, otherwise won't.
  "
  (interactive "P")
  (funcall simple-note-enable-auto-update-cache-function arg))

;;;###autoload
(defun simple-note--goto ()
  "The main procedure that is inboked by `simple-note-goto-node'."
  (require 'org)
  (let* ((id (cadr (funcall simple-note-completing-read-function "Select a node: "
			    (funcall simple-note-build-info-function simple-note-cache) nil t))))
    (org-id-goto id)))

(defvar simple-note-goto-function #'simple-note--goto
  "The function of `simple-note-goto'.")

;;;###autoload
(defun simple-note-goto-node ()
  "Goto a simple-note node."
  (interactive)
  (funcall simple-note-goto-function))

;;;###autoload
(defun simple-note-get-or-create-org-id ()
  "If an org id exists under an org entry, return it, oterwise create a new one and return it."
  (or (org-entry-get nil "ID") (org-id-uuid)))

;;;###autoload
(defun simple-note-add-to-node--cache(id file maintain-built-in)
  "Add an id in the given file to the maintained node cache.

If maintain-built-in is not nil, both `org-id-locations' and `org-id-files' are maintained.
  "
  (when maintain-built-in
    (puthash id file org-id-locations)
    (unless (member file org-id-files)
      (add-to-list 'org-id-files file)))

  (when (not (gethash id simple-note-cache))
    (let ((id-list (gethash file simple-note-file-reference)))
      (if id-list
	  (if (not (member id id-list))
	      (progn
		(push id id-list)
		(puthash file id-list simple-note-file-reference)))
	(progn
	  (push id id-list)
	  (puthash file id-list simple-note-file-reference))))

    (funcall simple-note-iterate-entry-in-file-function simple-note-cache id file)))

(defvar simple-note-add-to-node-cache-function #'simple-note-add-to-node--cache
  "The function of `simple-note-add-to-node--cache'.")

;;;###autoload
(defun simple-note-on-add-entry-to-node--cache ()
  "The main procedure that is invoked by `simple-note-add-entry-to-node-cache'."
  (when (equal major-mode 'org-mode)
    (let* ((id (simple-note-get-or-create-org-id))
	   (afile (abbreviate-file-name (buffer-file-name))))
      (if (not (org-entry-get nil "ID"))
	  (org-entry-put nil "ID" id))
      (when (not simple-note-edit-mode)
	(funcall simple-note-add-to-node-cache-function id afile t)))))

(defvar simple-note-on-add-entry-to-node-cache-function #'simple-note-on-add-entry-to-node--cache
  "The function of `simple-note-on-add-entry-to-node-cache'.")

;;;###autoload
(defun simple-note-add-entry-to-node-cache ()
  "Add an org entry to the node cache maintained by simple note."
  (interactive)
  (funcall simple-note-on-add-entry-to-node-cache-function))

;;;###autoload
(defun simple-note-remove-from-node--cache (id file)
  "Remove an id in the given file from the maintained node cache."
  (let ((id-list (gethash file simple-note-file-reference)))
    (if id-list
	(if (member id id-list)
	    (setq id-list (delete id id-list))))

    (if id-list
	(puthash file id-list simple-note-file-reference)
      (progn
	(setq org-id-files (delete file org-id-files))
	(remhash file simple-note-file-reference)))

    (remhash id org-id-locations)
    (remhash id simple-note-cache)))

(defvar simple-note-remove-from-node-cache-function #'simple-note-remove-from-node--cache
  "The function of `simple-note-remove-from-node--cache'.")

;;;###autoload
(defun simple-note-on-remove-entry-from-node--cache ()
  "The main procedure that is invoked by `simple-note-remove-entry-from-node-cache'."
  (when (equal major-mode 'org-mode)
    (let ((id (org-entry-get nil "ID")))
      (when id
	(let* ((afile (abbreviate-file-name (buffer-file-name))))
	  (org-entry-delete nil "ID")
	  (when (not simple-note-edit-mode)
	    (funcall simple-note-remove-from-node-cache-function id afile)))))))

(defvar simple-note-on-remove-entry-from-node-cache-function #'simple-note-on-remove-entry-from-node--cache
  "The function of `simple-note-on-remove-entry-from-node--cache'.")

;;;###autoload
(defun simple-note-remove-entry-from-node-cache ()
  "Remove an org entry from the node cache maintained by simple note."
  (interactive)
  (funcall simple-note-on-remove-entry-from-node-cache-function))

;;;###autoload
(defun simple-note-save-org-id--locations ()
  "The main procedure that is invoked by `simple-note-save-org-id-locations'."
  (require 'org-id)
  (org-id-locations-save))

(defvar simple-note-save-org-id-locations-function #'simple-note-save-org-id--locations
  "The function of `simple-note-save-org-id--locations'.")

;;;###autoload
(defun simple-note-save-org-id-locations ()
  "Save `org-id-locations' by `org-id-locations-save'."
  (interactive)
  (funcall simple-note-save-org-id-locations-function))

;;;###autoload
(defun simple-note-create-new--node (&optional id)
  "The main procedure that is invoked by `simple-note-create-new-node'."
  (let* ((file (read-file-name "The filename of the new node: "
			       (file-name-as-directory simple-note-directory) nil nil))
	 (buffer (generate-new-buffer (file-name-nondirectory file))))
    (if (file-exists-p file)
	(message "File '%s' already exists!" file)
      (progn
	(split-window-right)
	(other-window 1)
	(switch-to-buffer buffer)

	(org-mode)
	(setq simple-note-edit-mode t
	      buffer-file-name file)
	(setq-local header-line-format
		    (substitute-command-keys
		     "Use \\[simple-note-commit-changes] to commit, \\[simple-note-abort-changes] to abort."))
	(org-entry-put nil "ID" (or id (org-id-uuid)))))))

(defvar simple-note-create-new-node-function #'simple-note-create-new--node
  "The function of `simple-note-create-new--node'.")

;;;###autoload
(defun simple-note-create-new-node ()
  "Create a new node maintained by simple-note."
  (interactive)
  (funcall simple-note-create-new-node-function))

;;;###autoload
(defun simple-note-commit--changes ()
  "The main procedure that is invoked by `simple-note-commit-changes'."
  (when (and simple-note-edit-mode (equal major-mode 'org-mode))
    (save-buffer)
    (let ((file buffer-file-name))
      (with-temp-buffer
	(org-mode)
	(insert-file-contents file nil nil nil 'replace)
	(let ((case-fold-search t)
	      (id-regexp (rx (seq bol (0+ (any "\t ")) ":ID:" (1+ " ") (not (any " "))))))
	  (org-with-point-at 1
	    (while (re-search-forward id-regexp nil t)
	      (when (org-at-property-p)
		(funcall simple-note-add-to-node-cache-function (org-entry-get nil "ID") file t)))))))
    (setq-local header-line-format nil)
    (setq simple-note-edit-mode nil)))

;;;###autoload
(defvar simple-note-commit-changes-function #'simple-note-commit--changes
  "The function of `simple-note-commit--changes'.")

;;;###autoload
(defun simple-note-commit-changes ()
  "Commit the changes in the buffer created from `simple-note-create-new-node'."
  (interactive)
  (funcall simple-note-commit-changes-function))

;;;###autoload
(defun simple-note-abort--changes ()
  "The main procedure that is invoked by `simple-note-abort-changes'."
  (when (and simple-note-edit-mode (equal major-mode 'org-mode))
    (setq buffer-file-name nil)
    (kill-buffer)))

(defvar simple-note-abort-changes-function #'simple-note-abort--changes
  "The function of `simple-note-abort--changes'.")

;;;###autoload
(defun simple-note-abort-changes ()
  "Abort the changes in the buffer created from `simple-note-create-new-node'."
  (interactive)
  (funcall simple-note-abort-changes-function))

;;;###autoload
(defun simple-note-update--file (file)
  "Update cache of the given file."
  (when (and (not simple-note-edit-mode) (equal major-mode 'org-mode))
    (let ((id-list (gethash file simple-note-file-reference)))
      ;; Delete relevant id values and files
      (when id-list
	(setq org-id-files (delete file org-id-files))
	(dolist (id id-list)
	  (remhash id org-id-locations)
	  (remhash id simple-note-cache))
	(remhash file simple-note-file-reference)))

    (with-temp-buffer
      (org-mode)
      (insert-file-contents file nil nil nil 'replace)
      (let ((case-fold-search t)
	    (id-regexp (rx (seq bol (0+ (any "\t ")) ":ID:" (1+ " ") (not (any " "))))))
	(org-with-point-at 1
	  (while (re-search-forward id-regexp nil t)
	    (when (org-at-property-p)
	      (let* ((id (org-entry-get nil "ID")))
		(funcall simple-note-add-to-node-cache-function id file t)))))))))

(defvar simple-note-update-file-function #'simple-note-update--file
  "The function of `simple-note-update--file'.")

;;;###autoload
(defun simple-note-save--buffer ()
  "The main procedure that is invoked by `simple-note-save-buffer'."
  (when (and (not simple-note-edit-mode) (buffer-modified-p))
    (save-buffer)
    (funcall simple-note-update-file-function buffer-file-name)))

(defvar simple-note-save-buffer-function #'simple-note-save--buffer
  "The function of `simple-note-save--buffer'.")

;;;###autoload
(defun simple-note-save-buffer ()
  "Save current buffer and update information of entries."
  (interactive)
  (funcall simple-note-save-buffer-function))

;;;###autoload
(defun simple-note-save-cache-to--file (cache cache-file)
  "Save cache to the given file path."
  (with-temp-file cache-file
    (let ((print-level nil)
	  (print-length nil))
      (print cache (current-buffer)))))

(defvar simple-note-save-cache-to-file-function #'simple-note-save-cache-to--file
  "The function of `simple-note-save-cache-to--file'.")

;;;###autoload
(defun simple-note-load-cache-from--file (cache-file)
  "Load cache from the given file path."
  (when (file-exists-p cache-file)
    (let ((cache nil))
      (with-temp-buffer
	(condition-case nil
	    (progn
	      (insert-file-contents cache-file)
	      (setq cache (read (current-buffer))))
	  (error
	   (message "Could not read `cache-values' from %s, setting it to nil"
		    cache-file))))
      cache)))

(defvar simple-note-load-cache-from-file-function #'simple-note-load-cache-from--file
  "The function of `simple-note-load-cache-from--file'.")

;;;###autoload
(defun simple-note-save-cache-to-file ()
  "Save `simple-note-cache' to `simple-note-cache-file'."
  (funcall simple-note-save-cache-to-file-function simple-note-cache simple-note-cache-file))

(provide 'simple-note)
