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

;;; Package definition for GNU Guix.

(use-modules (gnu) (guix)
             (guix licenses)
             (guix build-system gnu)
             (gnu packages pkg-config))

(define (source? file stat)
  (and (not (string-suffix? "~" file))
       (not (string-suffix? ".go" file))
       (not (string-suffix? ".log" file))
       (not (string-suffix? ".trs" file))
       (not (member (basename file)
                    '(".git" "autom4te.cache"
                      "configure" "config.status" "config.log"
                      "config.cache" "aclocal.m4"
                      "Makefile" "Makefile.in")))))

(define S specification->package)

(package
  (name "guile-zstd")
  (version "0.1.0-git")
  (source (local-file (dirname (current-filename))
                      (string-append name "-" version "-source")
                      #:recursive? #t
                      #:select? source?))
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,(S "autoconf"))
     ("automake" ,(S "automake"))
     ("pkg-config" ,pkg-config)                ;for cross-compilation support
     ,@(if (%current-target-system)
           `(("guile" ,(S "guile")))  ;for 'guild compile' and 'guile-3.0.pc'
           '())
     ("guile" ,(S "guile"))))
  (inputs
   `(("zstd" ,(S "zstd") "lib")
     ("guile" ,(S "guile"))))
  (synopsis "GNU Guile bindings to the zstd compression library")
  (description
   "This package provides a GNU Guile interface to the zstd (``zstandard'')
compression library.")
  (home-page "https://notabug.org/guile-zstd/guile-zstd")
  (license gpl3+))
