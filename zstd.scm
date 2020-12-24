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
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; This module provides an interface to the zstd compression library.
;;;
;;; Code:

(define %zstd-library-file-name
  "/gnu/store/v04z33qas38iiv5ndasf4cw80kqyyr1r-zstd-1.4.4-lib/lib/libzstd.so.1")

(define %zstd-library
  (dynamic-link %zstd-library-file-name))

(define (zstd-procedure return name args)
  (pointer->procedure return (dynamic-func name %zstd-library)
                      args))

(define %input-buffer-struct                      ;ZSTD_inBuffer_s
  `(* ,size_t ,size_t))

(define %output-buffer-struct                     ;ZSTD_outBuffer_s
  %input-buffer-struct)

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
