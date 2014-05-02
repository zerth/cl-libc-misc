; -*- mode: lisp -*-

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cffi-grovel))

(asdf:defsystem :libc-misc
  :description "CFFI bindings and misc helpers for interfacing with libc and the OS."
  :version "0.1"
  :license "MIT"
  :author "Mike Watters <mike@mwatters.net>"
  :depends-on (:cffi)
  :serial t
  :components ((:file "packages")
               (cffi-grovel:grovel-file "libc-grovel")
               (:file "memory")
               (:file "errno")
               (:file "file")
               (:file "io")
               (:file "rlimit")))
