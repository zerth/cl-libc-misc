(in-package :net.mwatters.libc-misc)


#+(or freebsd)
(defcfun (errno-ptr "__error")
    (:pointer :int))


(defun errno ()
  #+(or freebsd)
  (let ((p (errno-ptr)))
    (unless (cffi:null-pointer-p p)
      (cffi:mem-ref p :int)))
  #-(or freebsd)
  *errno*)


(defvar *ignored-errnos* nil
  "a list of errno values which should be ignored by CHECK-ERRNO")


(declaim (inline ignored-errno-p))
(defun ignored-errno-p (e)
  (find e *ignored-errnos*))


(defmacro ignoring-errnos ((&rest errnos-to-ignore)
                           &body forms)
  `(let ((*ignored-errnos* (list ,@errnos-to-ignore)))
     ,@forms))


(defmacro with-interrupted-syscall-retry (&body forms)
  "execute FORMS, executing them again if an INTERRUPTED-SYSTEM-CALL
error is signaled."
  (let ((tag (gensym "TAG"))
        (c (gensym "C")))
    ;; note: using DO results in smaller code than RESTART-CASE.
    `(do ()
         (nil)
       ,tag
       (handler-bind
           ((interrupted-system-call (lambda (,c)
                                       (declare (ignore ,c))
                                       (go ,tag))))
         (return ,@forms)))))



(defun check-errno-1 (x msg error-pred error-class)
  (when (funcall error-pred x)
    (let ((e (errno)))
      (if (ignored-errno-p e)
          (return-from check-errno-1)
        (error (if (= #.+system-call-interrupted+ e)
                   'interrupted-system-call
                 error-class)
               :errno e
               :msg msg))))
  x)


(defun posix-error-pred (x)
  (and (integerp x)
       (minusp x)))


(defmacro check-errno (form msg &key
                            (error-pred '#'posix-error-pred)
                            (error-class ''error-with-errno+msg))
  "check the result of evaluating FORM, signaling an error if
ERROR-PRED returns non-nil when called with the result value
\(creating an ERROR-WITH-ERRNO+MSG whose errno slot is set.  if the
errno was EINTR \(interrupted system call\), FORM is instead retried."
  `(with-interrupted-syscall-retry
     (check-errno-1 ,form ,msg ,error-pred ,error-class)))




(define-condition error-with-errno+msg (error)
  ((errno :initarg :errno
          :initform nil
          :reader error-errno)
   (msg :initarg :msg :initform nil))
  (:report
   (lambda (c s)
     (with-slots (errno msg) c
       (format s "~A: ~A~A"
               (class-name (class-of c))
               msg
               (if errno
                   (format nil " \(errno ~D: ~A\)"
                           errno (errno-string errno))
                 ""))))))


(define-condition interrupted-system-call (error-with-errno+msg)
  ())



(defcfun (strerror-r #-linux "strerror_r"
                     #+linux "__xpg_strerror_r")
    :int
  (errno :int)
  (buf (:pointer :char))
  (len size-t))



(defun errno-string (errno)
  (with-foreign-object (buf :char #1=1024)
    (check-errno (strerror-r errno buf #1#)
                 "strerror_r\(\) failed")
    (values (cffi:foreign-string-to-lisp buf :max-chars #1#))))
