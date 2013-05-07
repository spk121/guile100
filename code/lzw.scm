;; Copyright (C) 2013 Daniel Hartwig <mandyke@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (lzw)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (lzw-compress
            lzw-uncompress
            %lzw-compress
            %lzw-uncompress))

;; This procedure adapted from an example in the Guile Reference
;; Manual.
(define (make-serial-number-generator start end)
  (let ((current-serial-number (- start 1)))
    (lambda ()
      (and (< current-serial-number end)
           (set! current-serial-number (+ current-serial-number 1))
           current-serial-number))))

(define (put-u16 port k)
  ;; Little endian.
  (put-u8 port (logand k #xFF))
  (put-u8 port (logand (ash k -8) #xFF)))

(define (get-u16 port)
  ;; Little endian.  Order of evaluation is important, use 'let*'.
  (let* ((a (get-u8 port))
         (b (get-u8 port)))
    (if (any eof-object? (list a b))
        (eof-object)
        (logior a (ash b 8)))))

(define (%lzw-compress in out done? table-size)
  (let ((codes (make-hash-table table-size))
        (next-code (make-serial-number-generator 0 table-size))
        (universe (iota 256))
        (eof-code #f))
    ;; Populate the initial dictionary with all one-element strings
    ;; from the universe.
    (for-each (lambda (obj)
                (hash-set! codes (list obj) (next-code)))
              universe)
    (set! eof-code (next-code))
    (let loop ((cs '()))
      (let ((c (in)))
        (cond ((done? c)
               (unless (null? cs)
                 (out (hash-ref codes cs)))
               (out eof-code)
               (values codes))
              ((hash-ref codes (cons c cs))
               (loop (cons c cs)))
              (else
               (and=> (next-code)
                      (cut hash-set! codes (cons c cs) <>))
               (out (hash-ref codes cs))
               (loop (cons c '()))))))))

(define (ensure-bv-input-port bv-or-port)
  (cond ((port? bv-or-port)
         bv-or-port)
        ((bytevector? bv-or-port)
         (open-bytevector-input-port bv-or-port))
        (else
         (scm-error 'wrong-type-arg "ensure-bv-input-port"
                    "Wrong type argument in position ~a: ~s"
                    (list 1 bv-or-port) (list bv-or-port)))))

(define (for-each-right proc lst)
  (let loop ((lst lst))
    (unless (null? lst)
      (loop (cdr lst))
      (proc (car lst)))))

(define (%lzw-uncompress in out done? table-size)
  (let ((strings (make-hash-table table-size))
        (next-code (make-serial-number-generator 0 table-size))
        (universe (iota 256))
        (eof-code #f))
    (for-each (lambda (obj)
                (hash-set! strings (next-code) (list obj)))
              universe)
    (set! eof-code (next-code))
    (let loop ((previous-string '()))
      (let ((code (in)))
        (unless (or (done? code)
                    (= code eof-code))
          (unless (hash-ref strings code)
            (hash-set! strings
                       code
                       (cons (last previous-string) previous-string)))
          (for-each-right out
                          (hash-ref strings code))
          (let ((cs (hash-ref strings code)))
            (and=> (and (not (null? previous-string))
                        (next-code))
                   (cut hash-set! strings <> (cons (last cs)
                                                   previous-string)))
            (loop cs)))))))

(define* (lzw-compress bv #:key (table-size 65536) dictionary)
  (call-with-values
      (lambda ()
        (open-bytevector-output-port))
    (lambda (output-port get-result)
      (let ((dict (%lzw-compress (cute get-u8 (ensure-bv-input-port bv))
                                 (cute put-u16 output-port <>)
                                 eof-object?
                                 table-size)))
        (if dictionary
            (values (get-result) dict)
            (get-result))))))

(define* (lzw-uncompress bv #:key (table-size 65536) dictionary)
  (call-with-values
      (lambda ()
        (open-bytevector-output-port))
    (lambda (output-port get-result)
      (let ((dict (%lzw-uncompress (cute get-u16 (open-bytevector-input-port bv))
                                   (cute put-u8 output-port <>)
                                   eof-object?
                                   table-size)))
        (if dictionary
            (values (get-result) dict)
            (get-result))))))
