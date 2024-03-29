;;; Guile-zstd --- GNU Guile bindings to the zstd compression library.
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guile-zstd.
;;;
;;; Guile-zstd is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guile-zstd is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-zstd.  If not, see <http://www.gnu.org/licenses/>.

(define-module (zstd)
  #:use-module (zstd config)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:export (%default-compression-level

            make-zstd-input-port
            call-with-zstd-input-port

            make-zstd-output-port
            call-with-zstd-output-port

            error-code?
            error-name))

;;; Commentary:
;;;
;;; This module provides an interface to the zstd compression library.
;;;
;;; Code:

(define %zstd-library
  (dynamic-link %zstd-library-file-name))

(define (zstd-procedure return name args)
  (pointer->procedure return (dynamic-func name %zstd-library)
                      args))

(define %input-buffer-struct                      ;ZSTD_inBuffer_s
  `(* ,size_t ,size_t))

(define %output-buffer-struct                     ;ZSTD_outBuffer_s
  %input-buffer-struct)

(define error-code?
  (let ((proc (zstd-procedure unsigned-int "ZSTD_isError" (list size_t))))
    (lambda (err)
      "Return true if ERR, an integer returned by a zstd function, denotes an
error."
      (not (zero? (proc err))))))

(define error-name
  (let ((proc (zstd-procedure '* "ZSTD_getErrorName" (list size_t))))
    (lambda (err)
      "Return the error name corresponding to ERR."
      (pointer->string (proc err)))))


;;;
;;; Compression.
;;;

(define stream-compression-input-size
  (zstd-procedure size_t "ZSTD_CStreamInSize" '()))

(define stream-compression-output-size
  (zstd-procedure size_t "ZSTD_CStreamOutSize" '()))

(define make-compression-context
  (let ((make (zstd-procedure '* "ZSTD_createCCtx" '()))
        (free (delay (dynamic-func "ZSTD_freeCCtx" %zstd-library))))
    (lambda ()
      (let ((context (make)))
        (set-pointer-finalizer! context (force free))
        context))))

(define set-compression-context-parameter!
  (zstd-procedure void "ZSTD_CCtx_setParameter"
                  `(* ,int ,int)))

(define compress!
  (let ((proc (zstd-procedure size_t "ZSTD_compressStream2"
                              `(* * * ,int))))
    (lambda (context input output mode)
      (let ((result (proc context input output mode)))
        (when (< result 0)
          (throw 'zstd-error 'compress! result))
        result))))

;; ZSTD_cParameter
(define ZSTD_C_COMPRESSION_LEVEL 100)
(define ZSTD_C_CHECKSUM_FLAG 201)

;; ZSTD_EndDirective
(define ZSTD_E_END 2)
(define ZSTD_E_CONTINUE 0)

(define %default-compression-level 4)

(define* (make-zstd-output-port port
                                #:key
                                (close? #t)
                                (checksum? #t)
                                (level %default-compression-level))
  "Return an output port that compresses data at the given LEVEL, using PORT
as its sink.  When CLOSE? is true, PORT is automatically closed when the
resulting port is closed."
  (define context
    (make-compression-context))

  (define input-size (stream-compression-input-size))
  (define input-available 0)
  (define input-buffer (make-bytevector input-size))

  (define output-size (stream-compression-output-size))
  (define output-buffer (make-bytevector output-size))

  (define output-ptr (bytevector->pointer output-buffer))

  (define (flush mode)
    (let* ((input-ptr (bytevector->pointer input-buffer))
           (input     (make-c-struct %input-buffer-struct
                                     (list input-ptr input-available 0))))
      (let loop ()
        (define output
          (make-c-struct %output-buffer-struct
                         (list output-ptr output-size 0)))

        (define remaining
          (compress! context output input mode))

        (when (error-code? remaining)
          (throw 'zstd-error 'compress! remaining))
        (match (parse-c-struct output %output-buffer-struct)
          ((_ _ position)
           (put-bytevector port output-buffer 0 position)))
        (match (parse-c-struct input %input-buffer-struct)
          ((_ _ position)
           (if (or (= position input-available)
                   (and (= mode ZSTD_E_END)
                        (zero? remaining)))
               (set! input-available 0)
               (loop)))))))

  (define (write! bv start count)
    (if (< input-available input-size)
        (let ((count (min count (- input-size input-available))))
          (bytevector-copy! bv start
                            input-buffer input-available
                            count)
          (set! input-available (+ input-available count))
          count)
        (begin
          (flush ZSTD_E_CONTINUE)
          (write! bv start count))))

  (define (close)
    (unless (zero? input-available)
      (flush ZSTD_E_END))
    (when close?
      (close-port port)))

  (set-compression-context-parameter! context
                                      ZSTD_C_COMPRESSION_LEVEL level)
  (set-compression-context-parameter! context
                                      ZSTD_C_CHECKSUM_FLAG
                                      (if checksum? 1 0))
  (make-custom-binary-output-port "zstd-output" write! #f #f close))

(define* (call-with-zstd-output-port port proc
                                     #:key
                                     (level %default-compression-level))
  "Call PROC with an output port that wraps PORT and compresses data.  PORT is
closed upon completion."
  (let ((zstd (make-zstd-output-port port
                                     #:level level #:close? #t)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc zstd))
      (lambda ()
        (close-port zstd)))))


;;;
;;; Decompression.
;;;

(define stream-decompression-input-size
  (zstd-procedure size_t "ZSTD_DStreamInSize" '()))

(define stream-decompression-output-size
  (zstd-procedure size_t "ZSTD_DStreamOutSize" '()))

(define make-decompression-context
  (let ((make (zstd-procedure '* "ZSTD_createDCtx" '()))
        (free (delay (dynamic-func "ZSTD_freeDCtx" %zstd-library))))
    (lambda ()
      (let ((context (make)))
        (set-pointer-finalizer! context (force free))
        context))))

(define decompress!
  (zstd-procedure size_t "ZSTD_decompressStream" '(* * *)))

(define* (make-zstd-input-port port #:key (close? #t))
  "Return an input port that decompresses data read from PORT.
When CLOSE? is true, PORT is automatically closed when the resulting port is
closed."
  (define context
    (make-decompression-context))

  (define input-size (stream-decompression-input-size))
  (define input-available 0)
  (define input-buffer (make-bytevector input-size))

  (define input-ptr (bytevector->pointer input-buffer))
  (define input (make-c-struct %input-buffer-struct
                               (list input-ptr input-size 0)))

  (define eof? #f)
  (define expect-more? #f)

  (define (read! bv start count)
    (if (zero? input-available)
        (if eof?
            (if expect-more?
                (throw 'zstd-error 'decompress! 0) ;premature EOF
                0)
            (begin
              (set! input-available
                (match (get-bytevector-n! port input-buffer 0 input-size)
                  ((? eof-object?) 0)
                  (n n)))
              (set! input
                (make-c-struct %input-buffer-struct
                               (list input-ptr input-available 0)))
              (when (zero? input-available)
                (set! eof? #t))
              (read! bv start count)))
        (let* ((output-ptr (bytevector->pointer bv start))
               (output (make-c-struct %output-buffer-struct
                                      (list output-ptr count 0)))
               (ret (decompress! context output input)))
          (when (error-code? ret)
            (throw 'zstd-error 'decompress! ret))
          (set! expect-more? (not (zero? ret)))
          (match (parse-c-struct input %input-buffer-struct)
            ((_ size position)
             (set! input-available (- size position))))
          (match (parse-c-struct output %output-buffer-struct)
            ((_ _ position)
             (if (zero? position)                 ;didn't write anything
                 (read! bv start count)
                 position))))))

  (define (close)
    (when close?
      (close-port port)))

  (make-custom-binary-input-port "zstd-input" read! #f #f close))

(define* (call-with-zstd-input-port port proc)
  "Call PROC with a port that wraps PORT and decompresses data read from it.
PORT is closed upon completion."
  (let ((zstd (make-zstd-input-port port #:close? #t)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc zstd))
      (lambda ()
        (close-port zstd)))))
