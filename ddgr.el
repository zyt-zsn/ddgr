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
			 "-n" "10"
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
  (ddgr-goto-bottom)
  )
(defun ddgr-next-set()
  (interactive)
  (process-send-string ddgr-process "n\n")
  (ddgr-goto-bottom)
  )
(defun ddgr-prev-set()
  (interactive)
  (process-send-string ddgr-process "p\n")
  (ddgr-goto-bottom)
  )
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
(defvar-keymap ddgr-mode-map
  :doc "Keymap for ddgr/duckduckgo search commands"
  "n" #'ddgr-next-set
  "p" #'ddgr-prev-set
  "f" #'ddgr-first-set
  "x" #'ddgr-toggle-url
  "*" #'ddgr
  "?" #'ddgr-help
  "q" #'ddgr-quit
  )
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
