#!/usr/bin/guile \
-e main -s
!#
;;; Copyright (C) 2013 Mark H Weaver <mhw@netris.org>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (srfi srfi-1)
	     (ice-9 match)
	     (ice-9 receive)
	     (rnrs bytevectors)
	     (rnrs io ports))

;; 'file-name-separator-string' and 'file-name-separator?' are
;; included in Guile 2.0.9 and later.
(define file-name-separator-string "/")
(define (file-name-separator? c) (char=? c #\/))


(define (fmt-error fmt . args)
  (error (apply format #f fmt args)))

;; Like 'string-pad-right', but for bytevectors.  However, unlike
;; 'string-pad-right', truncation is not allowed here.
(define* (bytevector-pad
          bv len #:optional (byte 0) (start 0) (end (bytevector-length bv)))
  (when (< len (- end start))
	(fmt-error
	 "bytevector-pad: truncation would occur: len ~a, start ~a, end ~a, bv ~s"
	 len start end bv))
  (let ((result (make-bytevector len byte)))
    (bytevector-copy! bv start result 0 (- end start))
    result))

(define (bytevector-append . bvs)
  (let* ((lengths (map bytevector-length bvs))
	 (total (fold + 0 lengths))
	 (result (make-bytevector total)))
    (fold (lambda (bv len pos)
            (bytevector-copy! bv 0 result pos len)
            (+ pos len))
          0 bvs lengths)
    result))

(define ustar-charset
  #;
  (char-set-union (ucs-range->char-set #x20 #x23)
  (ucs-range->char-set #x25 #x40)
  (ucs-range->char-set #x41 #x5B)
  (ucs-range->char-set #x5F #x60)
  (ucs-range->char-set #x61 #x7B))
  char-set:ascii)

(define (valid-ustar-char? c)
  (char-set-contains? ustar-charset c))

(define (ustar-string n str name)
  (unless (>= n (string-length str))
	  (fmt-error "~a is too long (max ~a): ~a" name n str))
  (unless (string-every valid-ustar-char? str)
	  (fmt-error "~a contains unsupported character(s): ~s in ~s"
		     name
		     (string-filter (negate valid-ustar-char?) str)
		     str))
  (bytevector-pad (string->utf8 str) n))

(define (ustar-0string n str name)
  (bytevector-pad (ustar-string (- n 1) str name)
                  n))

(define (ustar-number n num name)
  (unless (and (integer? num)
	       (exact? num)
	       (not (negative? num)))
	  (fmt-error "~a is not a non-negative exact integer: ~a" name num))
  (unless (< num (expt 8 (- n 1)))
	  (fmt-error "~a is too large (max ~a): ~a" name (expt 8 (- n 1)) num))
  (bytevector-pad (string->utf8 (string-pad (number->string num 8)
                                            (- n 1)
                                            #\0))
                  n))

(define (checksum-bv bv)
  (let ((len (bytevector-length bv)))
    (let loop ((i 0) (sum 0))
      (if (< i len)
          (loop (+ i 1) (+ sum (bytevector-u8-ref bv i)))
          sum))))

(define (checksum . bvs)
  (fold + 0 (map checksum-bv bvs)))

(define nuls (make-bytevector 512 0))

;; write a ustar record of exactly 512 bytes, starting with the
;; segment of BV between START (inclusive) and END (exclusive), and
;; padded at the end with nuls as needed.
(define* (write-ustar-record
          port bv #:optional (start 0) (end (bytevector-length bv)))
  (when (< 512 (- end start))
	(fmt-error "write-ustar-record: record too long: start ~s, end ~s, bv ~s"
		   start end bv))
  ;; We could have used 'bytevector-pad' here,
  ;; but instead use a method that avoids allocation.
  (put-bytevector port bv start end)
  (put-bytevector port nuls 0 (- 512 (- end start))))

;; write 1024 zero bytes, which indicates the end of a ustar archive.
(define (write-ustar-footer port)
  (put-bytevector port nuls)
  (put-bytevector port nuls))

(define (compose-path-name dir name)
  (if (or (string-null? dir)
          (file-name-separator? (string-ref dir (- (string-length dir) 1))))
      (string-append dir name)
      (string-append dir "/" name)))

;; Like 'call-with-port', but also closes PORT if an error occurs.
(define (call-with-port* port proc)
  (dynamic-wind
      (lambda () #f)
      (lambda () (proc port))
      (lambda () (close port))))

(define (call-with-dirstream* dirstream proc)
  (dynamic-wind
      (lambda () #f)
      (lambda () (proc dirstream))
      (lambda () (closedir dirstream))))

(define (files-in-directory dir)
  (call-with-dirstream* (opendir dir)
    (lambda (dirstream)
      (let loop ((files '()))
        (let ((name (readdir dirstream)))
          (cond ((eof-object? name)
		 (reverse files))
                ((member name '("." ".."))
		 (loop files))
                (else
		 (loop (cons (compose-path-name dir name) files)))))))))

;; split the path into prefix and name fields for purposes of the
;; ustar header.  If the entire path fits in the name field (100 chars
;; max), then leave the prefix empty.  Otherwise, try to put the last
;; component into the name field and everything else into the prefix
;; field (155 chars max).  If that fails, put as much as possible into
;; the prefix and the rest into the name field.  This follows the
;; behavior of GNU tar when creating a ustar archive.
(define (ustar-path-name-split path orig-path)
  (define (too-long)
    (fmt-error "~a: file name too long" orig-path))
  (let ((len (string-length path)))
    (cond ((<= len 100) (values "" path))
          ((> len 256) (too-long))
          ((string-rindex path
                          file-name-separator?
                          (- len 101)
                          (min (- len 1) 156))
	   => (lambda (i)
                (values (substring path 0 i)
                        (substring path (+ i 1) len))))
          (else (too-long)))))

(define (write-ustar-header port path st)
  (let* ((type  (stat:type st))
	 (perms  (stat:perms st))
	 (mtime  (stat:mtime st))
	 (uid    (stat:uid st))
	 (gid    (stat:gid st))
	 (uname  (or (false-if-exception (passwd:name (getpwuid uid)))
		     ""))
	 (gname  (or (false-if-exception (group:name (getgrgid gid)))
		     ""))

	 (size  (case type
                  ((regular) (stat:size st))
                  (else 0)))

	 (type-flag (case type
                      ((regular)      "0")
                      ((symlink)      "2")
                      ((char-special)  "3")
                      ((block-special) "4")
                      ((directory)    "5")
                      ((fifo)          "6")
                      (else (fmt-error "~a: unsupported file type ~a"
				       path type))))

	 (link-name (case type
                      ((symlink) (readlink path))
                      (else "")))

	 (dev-major (case type
                      ((char-special block-special)
		       (quotient (stat:rdev st) 256))
                      (else 0)))
	 (dev-minor (case type
                      ((char-special block-special)
		       (remainder (stat:rdev st) 256))
                      (else 0)))

	 ;; Convert file name separators to slashes.
	 (slash-path (string-map (lambda (c)
				   (if (file-name-separator? c) #\/ c))
				 path))

	 ;; Make the path name relative.
	 ;; TODO: handle drive letters on windows.
	 (relative-path (if (string-every #\/ slash-path)
                            "."
                            (string-trim slash-path #\/)))

	 ;; If it's a directory, add a trailing slash,
	 ;; otherwise remove trailing slashes.
	 (full-path (case type
                      ((directory) (string-append relative-path "/"))
                      (else (string-trim-right relative-path #\/)))))

    (receive (prefix name) (ustar-path-name-split full-path path)

	     (let* ((%name      (ustar-string  100 name      "file name"))
		    (%mode      (ustar-number    8 perms    "file mode"))
		    (%uid      (ustar-number    8 uid      "user id"))
		    (%gid      (ustar-number    8 gid      "group id"))
		    (%size      (ustar-number  12 size      "file size"))
		    (%mtime    (ustar-number  12 mtime    "modification time"))
		    (%type-flag (ustar-string    1 type-flag "type flag"))
		    (%link-name (ustar-string  100 link-name "link name"))
		    (%magic    (ustar-0string  6 "ustar"  "magic field"))
		    (%version  (ustar-string    2 "00"      "version number"))
		    (%uname    (ustar-0string  32 uname    "user name"))
		    (%gname    (ustar-0string  32 gname    "group name"))
		    (%dev-major (ustar-number    8 dev-major "dev major"))
		    (%dev-minor (ustar-number    8 dev-minor "dev minor"))
		    (%prefix    (ustar-string  155 prefix    "directory name"))

		    (%dummy-checksum (string->utf8 "        "))

		    (%checksum
		     (bytevector-append
		      (ustar-number 7 (checksum %name %mode %uid %gid %size %mtime
						%dummy-checksum
						%type-flag %link-name %magic %version
						%uname %gname %dev-major %dev-minor
						%prefix)
				    "checksum")
		      (string->utf8 " "))))

	       (write-ustar-record port
				   (bytevector-append
				    %name %mode %uid %gid %size %mtime
				    %checksum
				    %type-flag %link-name %magic %version
				    %uname %gname %dev-major %dev-minor
				    %prefix))))))

(define (write-ustar-path port path)
  (let* ((path (if (string-every file-name-separator? path)
		   file-name-separator-string
		   (string-trim-right path file-name-separator?)))
	 (st  (lstat path))
	 (type (stat:type st))
	 (size (stat:size st)))
    (write-ustar-header port path st)
    (case type
      ((regular)
       (call-with-port* (open-file path "rb")
	 (lambda (in)
	   (let ((buf (make-bytevector 512)))
	     (let loop ((left size))
	       (when (positive? left)
		     (let* ((asked (min left 512))
			    (obtained (get-bytevector-n! in buf 0 asked)))
		       (when (or (eof-object? obtained)
				 (< obtained asked))
			     (fmt-error "~a: file appears to have shrunk" path))
		       (write-ustar-record port buf 0 obtained)
		       (loop (- left obtained)))))))))
      ((directory)
       (for-each (lambda (path) (write-ustar-path port path))
		 (files-in-directory path))))))

(define (write-ustar-archive output-path paths)
  (catch #t
	 (lambda ()
	   (call-with-port* (open-file output-path "wb")
	     (lambda (out)
	       (for-each (lambda (path)
			   (write-ustar-path out path))
			 paths)
	       (write-ustar-footer out))))
	 (lambda (key subr message args . rest)
	   (false-if-exception (delete-file output-path))
	   (format (current-error-port) "ERROR: ~a\n"
		   (apply format #f message args))
	   (exit 1))))

(define (main args)
  (match args
	 ((program output-path paths ...)
	  (write-ustar-archive output-path paths))
	 (_ (display "Usage: ustar <archive> <file> ...\n" (current-error-port))
	    (exit 1))))

;;; Local Variables:
;;; mode: scheme
;;; eval: (put 'call-with-port* 'scheme-indent-function 1)
;;; eval: (put 'call-with-dirstream* 'scheme-indent-function 1)
;;; End:
