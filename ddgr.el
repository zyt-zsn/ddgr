(require 'cl-lib)
(require 'w3m)
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
(defvar ddgr-in-progress nil)
(defun ddgr-output-filter(process output)
  (setq ddgr-in-progress nil)
  (with-current-buffer ddgr-output-buffer
	(if (> (length output) 200)
		(let ((buffer-read-only nil))
		  ;; `*` is specially treated in org mode
		  (setq output (replace-regexp-in-string "\n\\([[:space:]]*\\)\\*\\([[:space:]]*other inputs are considered as new search keywords\\)" "\n\\1~*~\\2" output))
		  ;; Indent search result abstract
		  (setq output (replace-regexp-in-string "\n\\([^[:space:]]\\)" "\n     \\1" output))
		  ;; Eliminate unnecessary prompt
		  (setq output (replace-regexp-in-string "\n +ddgr (\\? for help): " "" output))

		  (if (string-match "omniprompt keys:" output)
			  (setq output (replace-regexp-in-string "\n\\([[:space:]]*\\)\\(omniprompt keys:\\)\\([[:space:]]*\\)" "\n\* \\2\\3" output))
			(setq output (format "* ~%s~ Search results(Page: %d)\n%s" ddgr-search-keys ddgr-page-num output))
			(setq output (concat output (ddgr-help)))
			)
		  (let (
				(line (line-number-at-pos))
				(pos (point))
				)
			(erase-buffer)
			(insert output)
			(goto-line line)
			;; (goto-char pos)
			)
		  )
	  (when (eq ddgr-prev-request 'open-url)
		(setq-local ddgr-prev-request nil)
		(let* (
			   (direction 'right)
			   (window
				(cond
				 ((and (car (w3m-list-buffers)) (get-buffer-window (car (w3m-list-buffers)))))
				 ((window-in-direction direction))
				 (t
				  (split-window (selected-window) nil direction nil))
				 ))
			   )
		  (with-selected-window window
			(w3m (current-kill 0))
			)
		  )
		)
	  )
	;; (message output)
	)
  )
(defun ddgr--signal-process(process args)
  (if ddgr-in-progress
	  (progn
		(message "正在处理前次请求，请稍后尝试...")
		nil)
	(setq ddgr-in-progress t)
	(process-send-string process args)
	t
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
	(with-current-buffer ddgr-output-buffer
	  (if (process-live-p ddgr-process)
		  (and
		   (ddgr--signal-process ddgr-process (format "*%s\n" keywords))
		   (setq-local ddgr-search-keys keywords)
		   )
		;; call ddgr-mode before start-process to make sure
		(ddgr-mode)
		(setenv "BROWSER" "w3m")
		(setenv "PYTHONIOENCODING" "utf-8")
		(setq ddgr-process
			  (start-process
			   "ddgr"
			   ddgr-output-buffer
			   (executable-find "ddgr")
			   "--proxy" (concat "http://"  (car (nth 0 (w32reg-get-ie-proxy-config))))
			   ;; 考虑 0/9 被config.org 配置为 evil-end-of-visual-line/evil-beginning-of-visual-line, 故只将 1-8 作为快速url跳转键
			   "-n" "8"
			   "-x"
			   keywords))
		(setq-local ddgr-search-keys keywords)
		(set-process-filter (get-buffer-process "ddgr-output") #'ddgr-output-filter)
		)
	  )
	(window--display-buffer ddgr-output-buffer
							window
							'resuse
							'(display-buffer-same-window . ()))
	(pop-to-buffer ddgr-output-buffer)
	(setq-local ddgr-page-num 0)
	;; (sleep-for 1)
	;; (pop-to-buffer cur-buf '(display-buffer-in-previous-window . ()) t)
	)
  )
(defun ddgr-goto-bottom()
  ;; (end-of-buffer)
  ;; (sleep-for 0.5)
  ;; "recenter" may cause screen flashing
  ;; (recenter -1 t)
  )
(defun ddgr-first-set()
  (interactive)
  (if (= 0 ddgr-page-num)
	  (progn
		(message "Already at the first page.")
		(beep)
		)
	(if (ddgr--signal-process ddgr-process "f\n")
		(setf ddgr-page-num 0))
	)
  )
(defun ddgr-next-set()
  (interactive)
  (if (ddgr--signal-process ddgr-process "n\n")
	  (cl-incf ddgr-page-num))
  )
(defun ddgr-prev-set()
  (interactive)
  (if (= 0 ddgr-page-num)
	  (progn
		(message "Already at the first page.")
		(beep)
		)
	(if (ddgr--signal-process ddgr-process "p\n")
		(cl-decf ddgr-page-num))
	)
  )
(function-put 'ddgr-first-set 'menu-enable '(> ddgr-page-num 0))
(function-put 'ddgr-prev-set 'menu-enable '(> ddgr-page-num 0))
(defun ddgr-help()
  ;; (interactive)
  ;; (ddgr--signal-process ddgr-process "?\n")
  "* omniprompt keys:
  n, p, f               fetch the next, prev or first set of search results
  index                 open the result corresponding to index in browser
  o [index|range|a ...] open space-separated result indices, ranges or all
  O [index|range|a ...] like key 'o', but try to open in a GUI browser
  d keywords            new DDG search for 'keywords' with original options
                        should be used to search omniprompt keys and indices
  x                     toggle url expansion
  c index               copy url to clipboard
  q, ^D, double Enter   exit ddgr
  ?                     show omniprompt help
  ~*~                     other inputs are considered as new search keywords"
  )
(defun ddgr-toggle-url()
  (interactive)
  (ddgr--signal-process ddgr-process "x\n")
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
		(progn
		  (setq-local ddgr-prev-request 'open-url)
		  (ddgr--signal-process ddgr-process (concat (format "c %c" ch) "\n"))
		  )
	  
	  (message "Read-only, '?' to check supported cmds")
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
  "<remap> <digit-argument>" #'ddg-self-insert-command
  ;; 不同于 evil-normal-state-map, evil-emacs-state-map 将数字小键盘的数字输入识别为 self-insert-command
  ;; 故不能只处理 digit-argument, 仍需对 self-insert-command 进行处理
  "<remap> <self-insert-command>" #'ddg-self-insert-command
  )

(define-key ddgr-mode-map [menu-bar ddgr] (cons "Duckduckgo" (make-sparse-keymap "ddgr-menu")))

(define-key ddgr-mode-map [menu-bar ddgr visit] '(menu-item
												  ;; 考虑 0/9 被config.org 配置为 evil-end-of-visual-line/evil-beginning-of-visual-line, 故只将 1-8 作为快速url跳转键
												  "1-8 open-url" nil
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
(defvar-local ddgr-search-keys nil)
(defvar-local ddgr-prev-request nil)
(define-derived-mode ddgr-mode org-mode "ddgr"
  "duckduckgo search mode."
  :keymap ddgr-mode-map
  (setq ddgr-output-buffer (get-buffer-create "ddgr-output"))
  (with-current-buffer ddgr-output-buffer
	(setq-local buffer-read-only t)
	(setq evil-normal-state-local-map ddgr-mode-map)
	)
  )
(provide 'ddgr)
