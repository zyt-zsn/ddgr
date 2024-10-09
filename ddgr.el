(require 'cl-lib)
(defvar ddgr-process nil)

(defun zyt/get-marked-text()
  (save-excursion
	(cond
	 ((eq major-mode 'pdf-view-mode)
	  (if (pdf-view-active-region-p)
		  (nth 0 (pdf-view-active-region-text))))
	 (t
	  (and (region-active-p)
		   (buffer-substring-no-properties (region-beginning) (region-end)))
	  )
	 )
	)
  )

(defun ddgr-output-filter(process output)
  (with-current-buffer ddgr-output-buffer
	(progn
	  (setq output (replace-regexp-in-string "\n\\([^[:space:]]\\)" "\n     \\1" output))
	  (let ((buffer-read-only nil))
		(if (> (length output) 200)
			(erase-buffer)
		  (end-of-buffer)
		  )
		(insert output)
		)
	  )
	)
  )
(defvar-local ddgr-page-num 0)
(defun ddgr (keywords)
  (interactive
   (list (or (zyt/get-marked-text)
			 (read-from-minibuffer "Text to search: " nil nil nil  
								   'minibuffer-history)))
   )
  ;; (message (format "keywords-->%s\n" keywords)) 
  (setq ddgr-output-buffer (get-buffer-create "ddgr-output"))
  (let* (
		 (cur-buf (current-buffer))
		 (coding-system-for-read 'utf-8-dos)
		 (coding-system-for-write 'utf-8-dos)
		 (direction 'right)
		 (window
		  (or (get-buffer-window ddgr-output-buffer t)
			  (split-window (selected-window) nil direction nil)))
		 )
	(if (process-live-p ddgr-process)
		(process-send-string ddgr-process (format "*%s\n" keywords))	 
	  (setq ddgr-process
			(start-process
			 "ddgr"
			 ddgr-output-buffer
			 "c:/windows/System32/ddgr.exe"
			 "--proxy" "http://127.0.0.1:10809"
			 "-n" "9"
			 "-x"
			 keywords))
	  (set-process-filter (get-buffer-process "ddgr-output") #'ddgr-output-filter)
	  )
	(window--display-buffer ddgr-output-buffer
							window
							'resuse
							'(display-buffer-same-window . ()))
	(pop-to-buffer ddgr-output-buffer)
	(ddgr-mode)
	(setq-local ddgr-page-num 0)
	;; (sleep-for 1)
	;; (pop-to-buffer cur-buf '(display-buffer-in-previous-window . ()) t)
	)
  )
(defun ddgr-goto-bottom()
  (end-of-buffer)
  (sleep-for 0.5)
  (recenter -1 t)
  )
(defun ddgr-first-set()
  (interactive)
  (process-send-string ddgr-process "f\n")
  (setf ddgr-page-num 0)
  (ddgr-goto-bottom)
  )
(defun ddgr-next-set()
  (interactive)
  (process-send-string ddgr-process "n\n")
  (cl-incf ddgr-page-num)
  (ddgr-goto-bottom)
  )
(defun ddgr-prev-set()
  (interactive)
  (process-send-string ddgr-process "p\n")
  (if (> ddgr-page-num 0)
	  (cl-decf ddgr-page-num))
  (ddgr-goto-bottom)
  )
(function-put 'ddgr-first-set 'menu-enable '(> ddgr-page-num 0))
(function-put 'ddgr-prev-set 'menu-enable '(> ddgr-page-num 0))
(defun ddgr-help()
  (interactive)
  (process-send-string ddgr-process "?\n")
  (ddgr-goto-bottom)
  )
(defun ddgr-toggle-url()
  (interactive)
  (process-send-string ddgr-process "x\n")
  (ddgr-goto-bottom)
  )

(defun ddgr-quit()
  (interactive)
  (bury-buffer)
  )
(defun ddg-self-insert-command()
  (interactive)
  (let* (
		 (basic-event (event-basic-type last-input-event))
		 (ch
		  (if (symbolp basic-event)
			  (get basic-event 'ascii-character)
			basic-event
			)
		  )
		 )
	(if (and (>= ch ?1) (<= ch ?9))
		;; (process-send-string ddgr-process (concat (format "%c" ch) "\n"))
		(w3m (progn
			   (process-send-string ddgr-process (concat (format "c %c" ch) "\n"))
			   (sleep-for 0.5)
			   (current-kill 0)
			   )
			 )
	  (message (format "last input event %s" ch))
	  )
	)
  )
(defvar-keymap ddgr-mode-map
  :doc "Keymap for ddgr/duckduckgo search commands"
  "n" #'ddgr-next-set
  "p" #'ddgr-prev-set
  "f" #'ddgr-first-set
  "x" #'ddgr-toggle-url
  "*" #'ddgr
  "?" #'ddgr-help
  "q" #'ddgr-quit
  "RET" #'org-open-at-point
  "<remap> <self-insert-command>" #'ddg-self-insert-command
  )

(define-key ddgr-mode-map [menu-bar ddgr] (cons "Duckduckgo" (make-sparse-keymap "ddgr-menu")))

(define-key ddgr-mode-map [menu-bar ddgr visit] '(menu-item
												 "1-10 open-url" nil
												 :enable nil
												 :help "press index number to open url accordingly"
												 ))

(define-key ddgr-mode-map [menu-bar ddgr utl-toggle] '(menu-item
												  "toggle-url" ddgr-toggle-url
												  :help "toggle url display"
												  :keys "\\[ddgr-toggle-url]"
												  ))

(define-key ddgr-mode-map [menu-bar ddgr prev] '(menu-item
												 "prev-set" ddgr-prev-set
												 :help "previous search page"
												 :keys "\\[ddgr-prev-set]"
												 :enable (> ddgr-page-num 0)))

(define-key ddgr-mode-map [menu-bar ddgr next] '(menu-item
												 "next-set" ddgr-next-set
												 :help "next search page"))

(define-key ddgr-mode-map [menu-bar ddgr first] '(menu-item
												  "first-set" ddgr-first-set
												  :help "first search page"
												  :keys "\\[ddgr-first-set]"
												  :enable (> ddgr-page-num 0)))

(defvar ddgr-output-buffer nil)
(define-derived-mode ddgr-mode org-mode "ddgr"
  "duckduckgo search mode."
  :keymap ddgr-mode-map
  (setq ddgr-output-buffer (get-buffer-create "ddgr-output"))
  (with-current-buffer ddgr-output-buffer
	(setenv "BROWSER" "w3m")
	(setenv "PYTHONIOENCODING" "utf-8")
	(setq-local buffer-read-only t)
	)
  )
(provide 'ddgr)
