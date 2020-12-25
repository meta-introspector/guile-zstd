;;; Guile-zstd --- GNU Guile bindings to the zstd compression library.
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (test-zstd)
  #:use-module (zstd)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match))

(define (random-seed)
  (logxor (getpid) (car (gettimeofday))))

(define %seed
  (let ((seed (random-seed)))
    (format (current-error-port) "random seed for tests: ~a~%"
            seed)
    (seed->random-state seed)))

(define (random-bytevector n)
  "Return a random bytevector of N bytes."
  (let ((bv (make-bytevector n)))
    (let loop ((i 0))
      (if (< i n)
          (begin
            (bytevector-u8-set! bv i (random 256 %seed))
            (loop (1+ i)))
          bv))))

(define* (compressed-data data #:key (level %default-compression-level))
  (let-values (((port get) (open-bytevector-output-port)))
    (call-with-zstd-output-port port
      (lambda (port)
        (put-bytevector port data)))
    (get)))

(define* (compress-and-decompress data
                                  #:key (level %default-compression-level))
  (bytevector=? (let ((compressed (compressed-data data #:level level)))
                  (call-with-zstd-input-port
                      (open-bytevector-input-port compressed)
                    (lambda (port)
                      (match (get-bytevector-all port)
                        ((? eof-object?) #vu8())
                        (bv bv)))))
                data))

(define stream-compression-input-size
  (@@ (zstd) stream-compression-input-size))


(test-begin "zstd")

(test-assert "empty bytevector"
  (compress-and-decompress #vu8()))

(test-assert "random bytevector"
  (compress-and-decompress (random-bytevector (+ (random 100000)
                                                 (* 20 1024)))))
(test-assert "small bytevector"
  (compress-and-decompress (random-bytevector 127)))

(test-assert "one byte"
  (compress-and-decompress (random-bytevector 1)))

(test-assert "bytevector of size equal to Zstd internal buffers"
  (compress-and-decompress (random-bytevector (stream-compression-input-size))))

(test-assert "bytevector of size equal to Zstd internal buffers -1"
  (compress-and-decompress (random-bytevector (1- (stream-compression-input-size)))))

(test-assert "bytevector of size relative to Zstd internal buffers +1"
  (compress-and-decompress (random-bytevector (1+ (stream-compression-input-size)))))

(test-assert "bytevector of 1MiB"
  (compress-and-decompress (random-bytevector (* 1024 1024))))

(test-assert "bytevector of 1MiB-1"
  (compress-and-decompress (random-bytevector (1- (* 1024 1024)))))

(test-assert "bytevector of 1MiB+1"
  (compress-and-decompress (random-bytevector (1+ (* 1024 1024)))))

(test-assert "bytevector of 2MiB, all compression levels"
  (let ((data (random-bytevector (* 2 1024 1024))))
    (every (lambda (level)
             (compress-and-decompress data #:level level))
           (iota 9 1))))

(test-equal "truncated compressed stream"
  '(zstd-error decompress!)
  (let* ((compressed (compressed-data (random-bytevector 7777)))
         (size       (- (bytevector-length compressed) 142))
         (truncated  (make-bytevector size)))
    (bytevector-copy! compressed 0 truncated 0 size)
    (catch 'zstd-error
      (lambda ()
        (call-with-zstd-input-port (open-bytevector-input-port truncated)
          get-bytevector-all))
      (lambda (key proc . _)
        (list key proc)))))

(test-equal "corrupt compressed stream"
  '(zstd-error decompress! "Restored data doesn't match checksum")
  (let ((compressed (compressed-data (random-bytevector 7777))))
    (bytevector-u8-set! compressed 42
                        (logxor (bytevector-u8-ref compressed 42)
                                #xff))
    (catch 'zstd-error
      (lambda ()
        (call-with-zstd-input-port (open-bytevector-input-port compressed)
          get-bytevector-all))
      (lambda (key proc error . _)
        (list key proc (error-name error))))))

(test-end)
