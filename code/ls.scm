#! /usr/local/bin/guile -s
!#

;; A solution to Guile 100 Problem #2 `ls'
;; Contributed by Jez Ng.

(use-modules (srfi srfi-1) ; fold, map etc
	     (srfi srfi-26) ; cut (partial application)
	     (srfi srfi-37) ; args-fold
	     (ice-9 ftw)
	     (ice-9 format)
	     (ice-9 i18n))

(define perror (cut format (current-error-port) <...>))

(define (default-printer path st . rest)
  (format #t "~a~%" (basename path)))

(define* (long-printer path st #:optional
		       (max-nlinks 0) (max-size 0)
		       (max-uname-length 0) (max-groupname-length 0))
  (let*
      ((bits-set?
	(lambda (bits . masks)
	  (let ((mask (apply logior masks)))
	    (= mask (logand bits mask)))))
       (permission-string
	(lambda (perms)
	  (let* ((setuid-bit #o4000)
		 (setgid-bit #o2000)
		 (sticky-bit #o1000)
		 (owner-read-bit #o400)
		 (owner-write-bit #o200)
		 (owner-exec-bit #o100)
		 (group-read-bit #o40)
		 (group-write-bit #o20)
		 (group-exec-bit #o10)
		 (other-read-bit #o4)
		 (other-write-bit #o2)
		 (other-exec-bit #o1)
		 (rwx-letter (lambda (bit letter)
			       (if (bits-set? perms bit) letter #\-)))
		 (setid-letter (lambda (exec-bit setid-bit letter)
				 (cond ((bits-set? perms exec-bit setid-bit) letter)
				       ((bits-set? perms setid-bit)
					(char-downcase letter))
				       (else (rwx-letter exec-bit #\x))))))
	    (string (rwx-letter owner-read-bit #\r)
		    (rwx-letter owner-write-bit #\w)
		    (setid-letter owner-exec-bit setuid-bit #\S)
		    (rwx-letter group-read-bit #\r)
		    (rwx-letter group-write-bit #\w)
		    (setid-letter group-exec-bit setgid-bit #\S)
		    (rwx-letter other-read-bit #\r)
		    (rwx-letter other-write-bit #\w)
		    (setid-letter other-exec-bit sticky-bit #\T)))))
       (format-time
	(lambda (time)
	  (if (and (<= time (current-time))
		   (< (- (current-time) time) (* 3600 24 30 6)))
	      (strftime "%b %e %H:%M" (localtime time))
	      (strftime "%b %e %_5Y" (localtime time)))))
       (type (case (stat:type st)
	       ((directory) #\d)
	       ((regular) #\-)
	       ((symlink) #\l)
	       ((block-special) #\b)
	       ((char-special) #\c)
	       ((fifo) #\p)
	       (else #\?)))
       (digits (lambda (n) (if (= n 0) 1 (1+ (inexact->exact (ceiling (log10 n))))))))
    (format #t "~a~a ~vd ~va ~va ~vd ~a ~a\n"
	    type
	    (permission-string (stat:perms st))
	    (digits max-nlinks) (stat:nlink st)
	    max-uname-length (passwd:name (getpwuid (stat:uid st)))
	    max-groupname-length (group:name (getgrgid (stat:gid st)))
	    (digits max-size) (stat:size st)
	    (format-time (stat:mtime st))
	    (if (char=? type #\l)
		(format #f "~a -> ~a" path (readlink path))
		(basename path)))))

(define (ls-dir dir-name dir-stat recursive? all? print-header? printer)
  (let* ((not-hidden? (lambda (name) (not (string-prefix? "." name))))
	 (enter? (lambda (path st)
		   (or (and (or all? (not-hidden? (basename path))) recursive?)
		       (= (stat:ino st) (stat:ino dir-stat))))))
    (let recurse ((tree (file-system-tree dir-name enter?))
		  (parent-path `(,(dirname dir-name)))
		  (top-level? #t))
      ;; `file-system-tree' returns a structure of the form
      ;; (string basename, object stat, tree children)
      (let* ((path (cons (car tree) parent-path))
	     (path-string (string-join (reverse path) file-name-separator-string))
	     (children
	      (filter
	       (lambda (tree) (or all? (not-hidden? (car tree))))
	       (sort (let ((current-dir-path (in-vicinity path-string "."))
			   (parent-dir-path (in-vicinity path-string "..")))
		       (cons (list current-dir-path (lstat current-dir-path))
			     (cons (list parent-dir-path (lstat parent-dir-path))
				   (cddr tree))))
		     (lambda (a b) (string-locale-ci<? (car a) (car b))))))
	     ;; `max' throws an error if called without arguments;
	     ;; `max-above-0' just returns 0
	     (max-above-0 (lambda args (apply max (cons 0 args))))
	     (stats (map cadr children))
	     (max-nlinks (apply max-above-0 (map stat:nlink stats)))
	     (max-size (apply max-above-0 (map stat:size stats)))
	     (max-uname-length
	      (apply max-above-0 (map (compose string-length passwd:name
					       getpwuid stat:uid) stats)))
	     (max-groupname-length
	      (apply max-above-0 (map (compose string-length group:name
					       getgrgid stat:gid) stats))))
	(if (or (not top-level?) print-header?) (format #t "~a:~%" path-string))
	(for-each (lambda (child)
		    (printer
		     (in-vicinity path-string (car child))
		     (cadr child)
		     max-nlinks max-size max-uname-length max-groupname-length))
		  children)
	(if recursive?
	    (for-each (lambda (child)
			(if (and (eq? (stat:type (cadr child)) 'directory)
				 (not (or (equal? (basename (car child)) ".")
					  (equal? (basename (car child)) ".."))))
			    (recurse child path #f)))
		      children))))))

(let* ((program-name (car (program-arguments)))
       (make-bool-option
	(lambda (opt-name flag)
	  (option `(,flag) #f #f (lambda (opt name arg result)
				   (acons opt-name #t result)))))
       ;; `getopt-long' requires the long option name to be provided,
       ;; but the real `ls' does not use long names. srfi-37 does not
       ;; have this restriction, so we use it instead.
       (args (args-fold
	      (cdr (program-arguments))
	      (map make-bool-option '(all? recursive? long?) '(#\a #\R #\l))
	      (lambda (opt name arg result)
		(perror "~a: illegal option -- ~a~%" program-name name)
		(perror "usage: ~a [-alR] [file ...]~%" program-name)
		(exit 1))
	      (lambda (opt result) (assq-set! result
					      'paths
					      (cons opt (assq-ref result 'paths))))
	      '((paths))))
       (paths (if (null? (assq-ref args 'paths)) '(".") (assq-ref args 'paths)))
       (printer (if (assq-ref args 'long?) long-printer default-printer))
       (ls-dir-cut (cut ls-dir <> <>
			(assq-ref args 'recursive?) (assq-ref args 'all?)
			(> (length paths) 1)
			printer))
       (exit-code 0))
  (for-each
   (lambda (path)
     (catch 'system-error
	    (lambda ()
	      (let ((st (lstat path)))
		(case (stat:type st)
		  ((directory) (ls-dir-cut path st))
		  ((symlink) (if (assq-ref args 'long?)
				 (printer path st)
				 (ls-dir-cut
				  (let ((linked-path (readlink path)))
				    (if (absolute-file-name? linked-path)
					linked-path
					(in-vicinity (dirname path)
						     linked-path)))
				  (stat path))))
		  (else (printer path st)))))
	    (lambda args
	      (perror "~a: ~a: ~a~%"
		      program-name path (strerror (system-error-errno args)))
	      (set! exit-code 1)))) paths)
  (exit exit-code))
