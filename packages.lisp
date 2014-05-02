(defpackage :net.mwatters.libc-misc
  (:nicknames :libc-misc)
  (:use :cl)

  (:import-from :cffi
   :defcvar
   :defcfun
   :defctype
   :defcstruct)

  (:export
   :with-foreign-object :with-foreign-objects
   :get-resource-limit
   :uintptr-t
   :intptr-t
   :time-t
   :size-t
   :ssize-t
   :off-t
   :os-close
   #+(or freebsd) :os-close-from
   :os-bzero
   :os-memcpy

   :errno
   :errno-string
   :check-errno
   :error-with-errno+msg
   :ignoring-errnos
   :error-errno
   :mode-t
   :file-mode-flags
   :os-chmod
   :os-read
   :os-readv
   :os-read-byte
   :os-write
   :os-writev
   :iovec
   :iovec-base
   :iovec-len
   :os-fcntl-3
   :set-fd-nonblocking
   :set-fd-blocking
   :sendfile
   :+bad-file-descriptor+
   :+too-many-open-files+
   :+resource-not-available+
   :+operation-in-progress+
   :+connection-reset+
   :+e-pipe+
   :+e-perm+
   :+e-io+
   :+e-access+
   :+e-invalid+
   :+e-not-socket+
   :+e-not-connected+
   :make-anonymous-tempfile
   :open-file-for-reading
   :seek-file-offset
   :compute-file-size))
