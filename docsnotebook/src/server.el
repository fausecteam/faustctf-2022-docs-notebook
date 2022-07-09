#!/usr/bin/env -S emacs --fg-daemon -l
(message "Starting docsnotebook...")
(package-initialize)
(add-to-list 'load-path "/srv/emacs-web-server")
(require 'web-server)
(require 'emacsql)
(require 'emacsql-sqlite)

;; Dear Emacsql devs, why isn't this considered an error? >:(
;; I just want to catch it °_°
(define-error 'emacsql-constraint "SQL Constraint Violated")

(defvar db (emacsql-sqlite "files/auth.db"))

(ignore-errors
  (emacsql db [:create-table users ([(name :primary-key) password token])]))

(defun serve-static (request filename)
  (with-slots (process headers) request
	(ws-send-file process filename)))

(defun auth/generate-token ()
  (with-temp-buffer
	(set-buffer-multibyte nil)
	(call-process "head" "/dev/urandom" t nil "-c" "30")
	(base64-encode-string (buffer-string) t)))

(defun auth/get-user (headers)
  (let* ((cookies (cdr (assoc :COOKIE headers)))
		 (cookielist (mapcar (lambda (x) (split-string x "=")) (split-string (or cookies "") "; ")))
		 (token (car (cdr (assoc "sessionId" cookielist #'string-equal)))))
	(car (car (emacsql db [:select [name]
						   :from users
						   :where (== token $s1)]
					   token)))))

(defun auth/check-password (username password)
  (string-equal (car (car (emacsql db [:select [password]
									   :from users
									   :where (= name $s1)]
								   username)))
				(secure-hash 'sha256 password)))

(defun auth/do-login (process username)
  (let* ((token (auth/generate-token))
		 (session (format "sessionId=%s" token)))
	(emacsql db [:update users
				 :set (= token $s2)
				 :where (= name $s1)]
			 username token)
	(ws-response-header process 303 '("Location" . "/editor") `("Set-Cookie" . ,session))))

(defun response/message (process code message)
  (ws-response-header process code '("Content-Type" . "text/plain; charset=utf-8"))
  (process-send-string process message))

(defun handler/register (request)
  (with-slots (process headers) request
	(let ((username (cdr (assoc "username" headers)))
		  (password (cdr (assoc "password" headers))))
	  (if (string-match-p "^[[:alnum:]]\\{,64\\}$" username)
		  (if (ignore-errors
				(emacsql db [:insert :into users
							 :values ([$s1 $s2 nil])]
						 username (secure-hash 'sha256 password))
				t)
			  (auth/do-login process username)
			(response/message process 400 "Already exists"))
	    (response/message process 400 "Invalid user")))))

(defun handler/login (request)
  (with-slots (process headers) request
	(let ((username (cdr (assoc "username" headers)))
		  (password (cdr (assoc "password" headers))))
	  (if (auth/check-password username password)
		  (auth/do-login process username)
		(response/message process 403 "Invalid login")))))

(defun handler/logout (request)
  (with-slots (process headers) request
    (ws-response-header process 303 '("Location" . "/login") '("Set-Cookie" . "sessionId=; expires=Thu, 01 Jan 1970 00:00:00 GMT"))))

(defun handler/list (request)
  (with-slots (process headers) request
	(let* ((user (auth/get-user headers))
		   (path (format "files/%s" user)))
	  (if user
		  (if (file-exists-p path)
			  (progn
				(ws-response-header process 200 '("Content-Type" . "application/json"))
				(process-send-string process (format "[%s]" (mapconcat (lambda (x) (format "\"%s\"" (substring x 0 -4))) (directory-files path nil "\\.org$") ","))))
			(ws-response-header process 200 '("Content-Type" . "application/json"))
			(process-send-string process "[]"))
		(response/message process 400 "Not logged in")))))

(defun handler/read (request)
  (with-slots (process headers) request
	(let* ((path (cdr (assoc :GET headers)))
		   (components (split-string path "/"))
		   (user (auth/get-user headers))
		   (file (nth 2 components))
		   (path (format "files/%s/%s.org" user file)))
	  (if user
		  (if (file-exists-p path)
			  (ws-send-file process path "text/plain; charset=utf-8")
			(ws-response-header process 200 '("Content-Type" . "text/plain; charset=utf-8")))
		(response/message process 400 "Not logged in")))))

(defun handler/save (request)
  (with-slots (process headers body) request
	(let* ((path (cdr (assoc :POST headers)))
		   (components (split-string path "/"))
		   (user (auth/get-user headers))
		   (file (nth 2 components))
		   (path (format "files/%s/%s.org" user file)))
	  (if user
		  (progn
			(make-directory (file-name-directory path) t)
			(chmod (file-name-directory path) #o777)
			(with-temp-file path
			  (set-buffer-multibyte nil)
			  (insert body))
		    (response/message process 200 "Success"))
		(response/message process 400 "Not logged in")))))

(defun handler/get-export (request)
  (with-slots (process headers) request
	(let* ((path (cdr (assoc :GET headers)))
		   (components (split-string path "/"))
		   (user (auth/get-user headers))
		   (file (nth 2 components))
		   (path (format "files/%s/%s.html" user file)))
	  (if user
		  (if (file-exists-p path)
			  (ws-send-file process path)
		    (response/message process 404 ""))
		(response/message process 400 "Not logged in")))))

(defun handler/export (request)
  (with-slots (process headers) request
	(let* ((path (cdr (assoc :POST headers)))
		   (components (split-string path "/"))
		   (user (auth/get-user headers))
		   (file (nth 2 components))
		   (path (format "files/%s/%s.org" user file)))
	  (if user
		  (if (file-exists-p path)
			  (progn
			    (start-file-process "export" nil (expand-file-name "./export") path)
				(response/message process 200 "Success"))
			(response/message process 404 "Not Found"))
		(response/message process 400 "Not logged in")))))

(let ((server (process
			   (ws-start
				'(((:GET . "^/$") . (lambda (request)
									  (with-slots (process headers) request
										(if (auth/get-user headers)
											(ws-response-header process 303 '("Location" . "/editor"))
										  (serve-static request "static/index.html")))))
				  ((:GET . "^/register$") . (lambda (request) (serve-static request "static/register.html")))
				  ((:GET . "^/login$") . (lambda (request) (serve-static request "static/login.html")))
				  ((:GET . "^/logout$") . handler/logout)
				  ((:GET . "^/load.svg$") . (lambda (request) (serve-static request "static/load.svg")))
				  ((:GET . "^/editor$") . (lambda (request)
											(with-slots (process headers) request
											  (if (auth/get-user headers)
												  (serve-static request "static/editor.html")
												(ws-response-header process 303 '("Location" . "/login"))))))
				  ((:POST . "^/register$") . handler/register)
				  ((:POST . "^/login$") . handler/login)
				  ((:GET . "^/list$") . handler/list)
				  ((:GET . "^/edit/[[:alnum:]]*$") . handler/read)
				  ((:POST . "^/edit/[[:alnum:]]*$") . handler/save)
				  ((:GET . "^/export/[[:alnum:]]*$") . handler/get-export)
				  ((:POST . "^/export/[[:alnum:]]*$") . handler/export)
				  ((lambda (_) t) . (lambda (request)
								 (with-slots (process headers) request
								   (ws-send-404 process)))))
				9000 "*server-log*" :host "::"))))
  (message "webserver started..."))
