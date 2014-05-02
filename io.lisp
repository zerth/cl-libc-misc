(in-package :net.mwatters.libc-misc)


(defcfun (os-read "read")
    ssize-t
  (fd :int)
  (buf (:pointer :void))
  (count size-t))


(defcfun (os-write "write")
    ssize-t
  (fd :int)
  (buf (:pointer :void))
  (count size-t))


(defun os-read-byte (fd)
  "try to read a byte from FD, ignoring EAGAIN errors."
  (ignoring-errnos (+resource-not-available+)
    (with-foreign-object (buf :uint8)
      (when (eql 1 (check-errno (os-read fd buf 1)
                                "failed to read\(\) from fd"))
        (cffi:mem-ref buf :uint8)))))



(defmacro iovec-base (iov)
  `(cffi:foreign-slot-value ,iov '(:struct iovec) 'base))

(defmacro iovec-len (iov)
  `(cffi:foreign-slot-value ,iov '(:struct iovec) 'len))


(defcfun (os-readv "readv")
    ssize-t
  (fd :int)
  (iovecs (:pointer (:struct iovec)))
  (num-iovecs :int))


(defcfun (os-writev "writev")
    ssize-t
  (fd :int)
  (iovecs (:pointer (:struct iovec)))
  (num-iovecs :int))


(defcfun (os-fcntl-3 "fcntl")
    :int
  (fd :int)
  (cmd :int)
  (arg :int))


(defcfun (os-fcntl-2 "fcntl")
    :int
  (fd :int)
  (cmd :int))


(defcfun (os-fcntl-3-ptr "fcntl")
    :int
  (fd :int)
  (cmd :int)
  (arg :pointer))



(defun obtain-exclusive-lock (fd)
  "Attempt to obtain an advisory exclusive lock on the file associated
with the file descriptor FD."
  (with-foreign-object (flock '(:struct flock))
    (cffi:with-foreign-slots ((lock-type
                               whence
                               start
                               len) flock (:struct flock))
      (setq
       lock-type (cffi:foreign-enum-value 'flock-lock-type :locked-exclusive)
       whence (cffi:foreign-enum-value 'seek-whence :start)
       start 0
       len 0)
      (check-errno (os-fcntl-3-ptr fd
                                   (cffi:foreign-enum-value 'fcntl-command :set-lock)
                                   flock)
                   "failed to obtain fcntl lock"))))


;; fixme; refactor
(defun set-fd-nonblocking (fd)
  "set the file descriptor FD to be non-blocking using fcntl\(\)."
  (check-errno (os-fcntl-3 fd
                           (cffi:foreign-enum-value 'fcntl-command :set-flags)
                           (logior (cffi:foreign-bitfield-value
                                    'file-open-flags '(:non-blocking))
                                   (check-errno
                                    (os-fcntl-2 fd
                                                (cffi:foreign-enum-value
                                                 'fcntl-command :get-flags))
                                    "failed to get fd flags via fcntl\(\)")))
               "failed to set fd flags via fcntl\(\)"))


(defun set-fd-blocking (fd)
  "set the file descriptor FD to be blocking using fcntl\(\)."
  (check-errno (os-fcntl-3 fd
                           (cffi:foreign-enum-value 'fcntl-command :set-flags)
                           (logand (lognot
                                    (cffi:foreign-bitfield-value
                                     'file-open-flags '(:non-blocking)))
                                   (check-errno
                                    (cffi:foreign-enum-value
                                     'fcntl-command :get-flags)
                                    "failed to get fd flags via fcntl\(\)")))
               "failed to set fd flags via fcntl\(\)"))



#+(or freebsd darwin)
(defcfun (sendfile-freebsd "sendfile")
    :int ; 0 on success, -1 on error
  (src-fd :int)
  (dst-socket :int)
  (offset off-t)
  (count size-t)
  (headers/trailers :pointer)
  (bytes-written :pointer) ; output param
  (flags :int))


#+linux
(defcfun (sendfile-linux "sendfile")
    ssize-t ; -1 on error, otherwise number of bytes written
  (dst-fd :int)
  (src-fd :int)
  ;; offset in: offset in src-fd from which to read.  if null, the
  ;; src-fd file offset will be modified.
  ;; offset out: offset in src-fd after reading.
  (offset (:pointer off-t))
  (count size-t))



(defun sendfile (fd socket offset count)
  "Using the sendfile\(\) system call, send COUNT octets starting at
OFFSET in the file associated with the file descriptor FD to the
socket associated with the file descriptor SOCKET, returning the
number of octets which were sent \(or -1 on error\)."
  #+(or freebsd darwin)
  (with-foreign-object (sent-bytes-ptr 'off-t)
    (check-errno (sendfile-freebsd fd socket offset count
                                   (cffi:null-pointer)
                                   sent-bytes-ptr 0)
                 "sendfile\(\) failed")
    (cffi:mem-ref sent-bytes-ptr 'off-t))
  #+linux
  (with-foreign-object (offset-ptr 'off-t)
    (setf (cffi:mem-ref offset-ptr 'off-t) offset)
    (check-errno (sendfile-linux socket fd offset-ptr count)
                 "sendfile\(\) failed"))
  #-(or linux freebsd darwin)
  (error "sendfile\(\) not yet implemented on this platform"))
