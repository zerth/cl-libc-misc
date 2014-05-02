(in-package :libc-misc)


(defcfun (os-close-1 "close")
    :int
  (fd :int))


(defun os-close (fd)
  (ignoring-errnos (+bad-file-descriptor+ +e-no-space+ +connection-reset+)
    (check-errno (os-close-1 fd)
                 "failed to close\(\)")))


#+(or freebsd)
(defcfun (os-close-from "closefrom")
    :int
  (low-fd :int))


(defcfun (os-chmod "chmod")
    :int
  (path :string)
  (mode mode-t))


(defcfun (os-open-2 "open")
    :int
  (path :string)
  (flags :int))


(defcfun (os-open-3 "open")
    :int
  (path :string)
  (flags :int)
  (mode mode-t))


(defcfun (os-lseek "lseek")
    :int
  (fd :int)
  (offset off-t)
  (whence seek-whence))


(defun seek-file-offset (fd offset)
  ;; offset: :start, :current, :end, or offset value
  (check-errno (os-lseek fd
                         (if (member offset '(:start :end :current))
                             0
                           offset)
                         (cffi:foreign-enum-value 'seek-whence
                                                  (if (keywordp offset)
                                                      offset
                                                    :start)))
               "failed to lseek\(\)"))


(defun compute-file-size (fd)
  "return the size in bytes of the file associated with the open file
descriptor FD."
  ;; fixme; this should instead be done using the stat struct/syscall:
  (let ((cur (seek-file-offset fd :current))
        (end (seek-file-offset fd :end)))
    (prog1 end
      (seek-file-offset fd cur))))


(defun get-real-path (path)
  (let ((path (enough-namestring path)))
    ;; undo CCL escaping:
    #+ccl (setq path (remove #\\ path))
    path))


(defun open-file-for-reading (path &key follow-symlinks-p (lock-p t))
  "return as two values a file descriptor corresponding to the file at
PATH opened for reading, and its current size."
  ;; results in various errnos (see open(2))
  (let ((fd (check-errno (os-open-2 (get-real-path path)
                                    (cffi:foreign-bitfield-value
                                     'file-open-flags
                                     (append
                                      '(:read)
                                      (when lock-p
                                        '(:locked-shared))
                                      (unless follow-symlinks-p
                                        '(:no-follow)))))
                         "failed to open\(\) for reading")))
    (values fd (compute-file-size fd))))


(defcfun (os-mktemp "mktemp")
    :string
  (template :string))


(defcfun (os-getenv "getenv")
    :string
  (var :string))


(defun tmpdir ()
  (let ((p (or #+lispworks (hcl:get-temp-directory)
               #+sbcl (sb-ext:posix-getenv "TMPDIR")
               #+unix (when (probe-file "/tmp")
                        "/tmp")
               (error "don't know how to locate temporary directory on this platform"))))
    (when (pathnamep p)
      (setq p (enough-namestring p)))
    (unless (eql (1- (length p)) (search "/" p :from-end t :test #'char=))
      (setq p (format nil "~A/" p)))
    p))


(defun make-anonymous-tempfile (&key non-blocking-p)
  "create a new anonymous temporary file and return its file
descriptor after unlinking it \(will be deleted when FD is closed\)."
  (tagbody
   RETRY
   (let ((filename (os-mktemp
                    (format nil "~Atmp.XXXXXXXXXXXXXXXX" (tmpdir)))))
     (unless filename
       (error 'error-with-errno+msg
              :errno (errno)
              :msg "failed to get temporary filename"))
     (handler-bind
         ((error-with-errno+msg (lambda (c)
                                  (when (eql +e-exist+ (error-errno c))
                                    ;; file already created before we could, retry:
                                    (go RETRY)))))
       (let ((fd (check-errno (os-open-3 filename
                                         (cffi:foreign-bitfield-value
                                          'file-open-flags
                                          (append
                                           '(:create :exclusive
                                             :read+write :no-follow)
                                           #+(or freebsd)
                                           '(:locked-exclusive)
                                           (when non-blocking-p
                                             '(:non-blocking))))
                                         (cffi:foreign-bitfield-value
                                          'file-mode-flags
                                          '(:owner-r :owner-w)))
                              "failed to open\(\) tempfile")))
         #-(or freebsd)
         (obtain-exclusive-lock fd)
         (check-errno (os-unlink filename)
                      "failed to unlink\(\) tempfile"
                      :error-class 'error-with-errno+msg)
         (return-from make-anonymous-tempfile fd))))))


(defcfun (os-unlink "unlink")
    :int
  (path :string))
