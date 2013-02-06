;; Mode for Merlin



(defvar merlin-command "ocamlmerlin"
  "Merlin's command")

(defvar merlin-process nil
  "The process associated to the current buffer")
(defvar merlin-continuation nil
  "The callback to be called with the result of the next command")
(defvar merlin-point nil
  "Stores the point of last completion")

(defvar merlin-debug t)
(defvar merlin-type-buffer (get-buffer-create "*merlin types*"))

(defun merlin-make-buffer-name ()
  "The buffer name of the process for merlin"
  (concat "*"
	  (buffer-file-name nil)
	  " merlin *"))

(defun merlin-make-process-name ()
  "The process name of a buffer"
  (concat "merlin-" (buffer-file-name nil)))

(defun merlin-get-process ()
  merlin-process)
;  (get-process (merlin-make-process-name)))

(defun merlin-filter (process output)
  "The filter on merlin's output"
  (if merlin-debug (message output))
  (if merlin-continuation
      (let ((a (ignore-errors (json-read-from-string output))))
	(if a (funcall merlin-continuation a)))))

(defun merlin-wait-for-answer ()
  "Waits for merlin to answer"
  (if (not (accept-process-output (merlin-get-process) 0.1 nil nil))
      (merlin-wait-for-answer)))

(defun merlin-start-process ()
  "Start the merlin process"
  (let ((p (start-process (merlin-make-process-name)
			 (merlin-make-buffer-name)
			 merlin-command)))
    (set (make-local-variable 'merlin-process) p)
    (set-process-filter p 'merlin-filter)))


(defun merlin-send-command (name args callback)
  "Send a command to merlin. Callback is called with the sexp result of the command"
  (let ((string
	 (concat 
	  (json-encode 
	   (if args (append (list name) args) (list name)))
	  "\n")))
    (if merlin-debug (message string))
    (set (make-local-variable 'merlin-continuation) callback)
    (process-send-string (merlin-get-process) string)
    (merlin-wait-for-answer)
))
	
(defun merlin-rewind ()
  "Rewind the knowledge of merlin of the current buffer to zero"
  (merlin-send-command "reset" nil nil))

(defun merlin-make-point (line col)
  "Creates a point from a couple line / col"
  (save-excursion
    (beginning-of-line)
    (goto-line line)
    (forward-char col)
    (point)))
(defun merlin-dump-env ()
  "Dump the environment"
  (merlin-send-command "dump" '("env")
		       '(lambda (sexp) 
			  (message "Env: %s" (append (elt sexp 1) nil)))))


(defun merlin-tell-string (mode string)
  "Tell a string to merlin using `mode'"
  (merlin-send-command "tell" (list "struct" string) nil))

(defun merlin-flush-tell ()
  "Flush merlin teller"
  (merlin-send-command "tell" '("struct" nil) nil))

(defun merlin-tell-piece (mode start end)
  "Tell part of the current buffer to merlin using `mode'"
  (merlin-tell-string mode 
		      (buffer-substring start end)))


(defvar merlin-cache nil "Merlin cache for completion")
(defvar merlin-prefix nil "Merlin prefix")
(defvar merlin-name nil "Merlin name")
(defvar merlin-overlay nil "Merlin overlay used for errors")

(defun merlin-remove-overlay ()
  (delete-overlay merlin-overlay))

(defun merlin-highlight (msg start end)
  (setq merlin-overlay
	(make-overlay start end))
  (overlay-put merlin-overlay 'face 'next-error)
  (run-at-time "5 sec" nil 'merlin-remove-overlay)
  )
(defun merlin-handle-errors (errors)
  (let ((message (cdr (assoc 'message errors)))
	(lbeginning (cdr (assoc 'line (cdr (assoc 'start errors)))))
	(cbeginning (cdr (assoc 'col (cdr (assoc 'start errors)))))
	(lend (cdr (assoc 'line (cdr (assoc 'end errors)))))
	(cend (cdr (assoc 'col (cdr (assoc 'end errors)))))
	(type (cdr (assoc 'type errors))))
    (goto-char (merlin-make-point lbeginning cbeginning))
    (merlin-highlight message (merlin-make-point lbeginning cbeginning)
		      (merlin-make-point lend cend))
    (message "%s: %s" type message)
))

(defun merlin-view-errors ()
  (merlin-send-command "errors" nil
		       '(lambda (output)
			  (if (> (length (elt output 1)) 0)
			      (merlin-handle-errors (elt (elt output 1) 0))
			    (message "ok")))))

(defun merlin-tell-previous-lines ()
  (interactive)
  (merlin-rewind)
  (save-excursion
    (move-beginning-of-line nil)
    (merlin-tell-piece "struct" (point-min) (point)))
  (merlin-flush-tell)
  (merlin-view-errors)
)


(defun merlin-extract-complete (l)
  (mapcar '(lambda (c) 
	     (concat merlin-prefix 
		     (cdr (assoc 'name c)) 
		     ": " 
		     (cdr (assoc 'desc c))
		     ))
	  (append l nil)))
;(cdr (assoc 'name c))

(defun merlin-source-action ()
  (save-excursion
    (let ((endpoint (point)))
      (goto-char merlin-point)
      (search-forward ":")
      (backward-char 1)
      (delete-region (point) endpoint)))
)

(defun merlin-compute-prefix (ident)
  (let ((l (butlast (split-string ident "\\."))))
    (let ((s (mapconcat 'identity l ".")))
      (if (string-equal s "") s (concat s ".")))))
	
    
(defun merlin-complete-identifier (ident)
  (setq merlin-prefix (merlin-compute-prefix ident))
  (merlin-send-command "complete" (list "prefix" ident) 
		       '(lambda (output)
			  (setq merlin-cache 
				(merlin-extract-complete (elt output 1)))
			  )))

(defun merlin-source-init ()
  (setq merlin-cache nil)
  (setq merlin-point ac-point)
  (merlin-complete-identifier ac-prefix)
  (sleep-for 0.05)
  merlin-cache)
  

(defvar merlin-ac-source
  '((init . merlin-source-init)
    (candidates . (lambda () merlin-cache))
    (action . merlin-source-action)
    (requires . 3)
    ))

(ac-define-source "merlin" merlin-ac-source)

(defun merlin-setup ()
  "Sets up a buffer for use with merlin"
  (interactive)
  (merlin-start-process)
  (auto-complete-mode)
  (setq ac-sources '(merlin-ac-source))
  (with-current-buffer merlin-type-buffer
    (tuareg-mode))
)
(defun merlin-ident-under-point ()
  (let ((x (bounds-of-thing-at-point 'symbol)))
    (buffer-substring-no-properties (car x) (cdr x))))

(defun merlin-get-type () 
  (interactive)
  (merlin-send-command "complete" (list "prefix" (merlin-ident-under-point))
		       '(lambda (sexp)
			  (if (= (length (elt sexp 1)) 0) 
			      (message "nothing found for %s" (merlin-ident-under-point))
			    (let ((ans (elt (elt sexp 1) 0)))
			      (if (string-equal (cdr (assoc 'kind ans)) "Value")
				  (message "%s" (cdr (assoc 'desc ans)))
				(progn
				  (display-buffer merlin-type-buffer)
				  (with-current-buffer merlin-type-buffer
				    (erase-buffer)
				    (insert (cdr (assoc 'info ans)))))
))))))
(defun merlin-view-type ()
  (interactive)
  (merlin-complete-identifier
;; Mode definition
(define-minor-mode merlin-mode
  "Mode to use merlin tool inside OCaml tools."
  nil
  " merlin"
  '(("\C-i" . merlin-tell-previous-lines)
    ("\C-o" . merlin-get-type))
  :after-hook 'merlin-setup)
