#! /usr/bin/guile \
--debug -e eguile-main -s
!#

;;    eguile -- embedded guile preprocessor
;;    Copyright (C) 2002 Neale Pickett <neale@woozle.org>
;;
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License as
;;    published by the Free Software Foundation; either version 2 of the
;;    License, or (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;    General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;    02111-1307 USA

(use-modules
 (ice-9 rdelim)
 (srfi srfi-13))

(define-macro (fluid-let bindings . body)
  `(begin
     ,@(map (lambda (binding)
              (let ((name (car binding)))
                `(or (fluid? ,name)
                     (let ((init ,name))
                       (set! ,name (make-fluid))
                       (fluid-set! ,name init)))))
            bindings)
     (with-fluids ,bindings ,@body)))

(define (substring? a b)
  (string-contains b a))

;;
;; Backwards compatibility.  Set this to:
;;
;;   'escm      for escm compatibility
;;   number     for old eguile compatibility
;;
;; Anything else gets you no backwards compatibility -- you get to do
;; things the current way.
;;
(define eguile-compatibility 'current)

;; Retrieve a named parameter with optional default
(define (getparam args name . default)
  (define (loop args)
    (cond
     ((null? args)
      (if (pair? default)
	  (car default)
	  #f))
     ((equal? (car args) name)
      (cadr args))
     (else
      (loop (cddr args)))))
  (loop args))


(define lmatch '())
(define rmatch '())

;;
;; New^3 improved parser!  Woo woo woo!
;;
(define (stml->commands inp)
  (define (loop inp needle other buf)
    ;; Read in a line, looking for needle.  If it's there, switch needle
    ;; and other, and outbuf buf + line
    (let ((line (read-line inp 'concat)))
      (cond
       ((eof-object? line) (cons buf '("")))
       (else
        (let ((pos (substring? needle line)))
          (cond
           (pos
            (unread-string (substring line
                                      (+ (string-length needle)
                                         pos))
                           inp)
            (cons (string-append buf (substring line 0 pos))
                  (loop inp other needle "")))
           (else
            (loop inp needle other (string-append buf line))))))))) ; Ha ha, scheme

  (define (list->commands list)
    ;; Convert the output of (loop) to something that can be evaled.
    (cond
     ((null? list) "")
     ((null? (cdr list))
      (string-append "Unbalanced preprocessor directives: "
                     (object->string list)))
     (else
      (let ((str (object->string (car list)))
            (expr (cadr list))
            (rest (list->commands (cddr list))))
        (string-append " (display "
                       (object->string (car list))
                       ") "
                       (cond
                        ((equal? (substring? ":d " expr) 0)
                         (string-append "(display "
                                        (substring expr 3)
                                        ")"))
                        (else
                         expr))
                       rest)))))

  (list->commands
   (loop inp "<?scm" " ?>" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *program-name* #f)		; Dynamically bound
(define *input-file* #f)		; Dynamically bound
(define *output-file* #f)		; Dynamically bound

(define (eguile-port inp)
  (let ((commands (stml->commands inp)))
    (eval-string commands)))

(define (eguile-file input)
  (fluid-let ((*input-file* input))
    (eguile-port (open-input-file input))))

(define (eguile input output)
  (fluid-let ((*output-file* output))
    (let ((out-port (open-output-file output))
	  (*stdout* (current-output-port)))
      (set-current-output-port out-port)
      (eguile-file input)
      (set-current-output-port *stdout*))))

(define (eguile-main argv)
  ;; Make sure we have the correct number of arguments
  (if (not (= (length argv) 3))
      (begin
	(display (string-append "Usage: " (car argv) " infile outfile"))
	(newline)
	(exit)))

  (fluid-let ((*program-name* (car argv)))
    ;; Do it to it
    (eguile (cadr argv) (caddr argv))))
