#! /usr/bin/env guile \
-e main -s
!#

(use-modules (rnrs bytevectors)
	     (rnrs io ports)
	     (srfi srfi-1) ; map, reduce
	     (srfi srfi-26) ; cut, cute
	     (ice-9 format))

(define write-bytevector (cut put-bytevector (current-output-port) <...>))

(define block-size 512)

(define (cat)
  (define bv (make-bytevector block-size 0))
  (let ((read-count (get-bytevector-n! (current-input-port) bv 0 block-size)))
    (unless (eof-object? read-count)
      (write-bytevector bv)
      (unless (< read-count block-size) (cat)))))

(define (valid-rustar-char? c)
  (or (char<=? #\x20 c #\x22)
      (char<=? #\x25 c #\x3F)
      (char<=? #\x41 c #\x5A)
      (char=?  #\x5F c)
      (char<=? #\x61 c #\x7A)))

(define (make-fixed-string length string)
  (let ((bv (make-bytevector length 0)))
    (string-for-each-index
     (lambda (i)
       (let ((c (string-ref string i)))
	 (unless (valid-rustar-char? c)
	   (throw 'ustar-error "encountered invalid character"))
	 (bytevector-u8-set! bv i (char->integer c))))
     string)
    bv))

(define (make-rustar-string length string)
  (if (<= (string-length string) length)
    (make-fixed-string length string)
    (throw 'ustar-error "'~a' is too long for tar header" string)))

(define (make-rustar-0string length string)
  (if (< (string-length string) length)
    (make-fixed-string length string)
    (throw 'ustar-error "'~a' is too long for tar header" string)))

(define (make-rustar-number length number)
  (let* ((num (number->string number 8))
	 (padding (- length (string-length num) 1)))
    (if (>= padding 0)
	(make-fixed-string length (string-append (make-string padding #\0) num))
	(throw 'ustar-error "~a is too large for tar header" num))))

(define (write-file-header filename)
  (define st (lstat filename))
  (unless (eq? (stat:type st) 'regular)
      (throw 'ustar-error "Only regular files are supported"))
  (let* ((uid (stat:uid st))
	 (gid (stat:gid st))
	 ; We only really need an a-list for the purposes of modifying
	 ; checksum in-place. The other keys are not used. However, they do
	 ; serve as documentation.
	 (header
	  `((filename . ,(make-rustar-string 100 (basename filename)))
	    (mode . ,(make-rustar-number 8 (stat:perms st)))
	    (uid . ,(make-rustar-number 8 uid))
	    (gid . ,(make-rustar-number 8 gid))
	    (size . ,(make-rustar-number 12 (stat:size st)))
	    (mtime . ,(make-rustar-number 12 (stat:mtime st)))
	    (checksum . ,(make-bytevector 8 (char->integer #\space)))
	    (typeflag . ,(make-rustar-string 1 "0"))
	    (link-name . ,(make-rustar-string 100 ""))
	    (magic . ,(make-rustar-0string 6 "ustar"))
	    (version . ,(make-rustar-string 2 "00"))
	    (uname . ,(make-rustar-0string 32 (passwd:name (getpwuid uid))))
	    (gname . ,(make-rustar-0string 32 (group:name (getgrgid gid))))
	    (dev-major . ,(make-rustar-number 8 0))
	    (dev-minor . ,(make-rustar-number 8 0))
	    (path . ,(make-rustar-string 155 (dirname filename)))
	    (padding . ,(make-rustar-0string 12 ""))))
	 (sum (cut reduce + 0 <>))
	 (checksum (sum (map (compose sum bytevector->u8-list cdr) header))))
    (set! header (assq-set! header 'checksum (make-rustar-number 8 checksum)))
    (for-each (compose write-bytevector cdr) header)))

(define (tar archive filenames)
  (with-output-to-file archive
    (lambda ()
      (for-each (lambda (filename)
		  (write-file-header filename)
		  (with-input-from-file filename cat #:binary #t))
		filenames)
      (write-bytevector (make-bytevector (* block-size 2) 0)))
    #:binary #t))

(define (main args)
  (define perror (cut format (current-error-port) <...>))
  (define (system-error-handler . args)
    (perror "error: ~a~%" (strerror (system-error-errno args)))
    (exit 1))
  (define (ustar-error-handler . args)
    (perror "error: ")
    (apply perror (cdr args))
    (perror "~%")
    (exit 1))

  (catch 'ustar-error
    (lambda ()
      (catch 'system-error
        (cute tar (cadr args) (cddr args))
	system-error-handler))
    ustar-error-handler))
