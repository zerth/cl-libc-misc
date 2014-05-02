(in-package :net.mwatters.libc-misc)


(defmacro with-foreign-object ((var type &optional (count 1) (zero-memory-p 't)) &body forms
                               &environment env)
  "allocate memory for COUNT elements of type TYPE, pointed to by the
pointer bound to VAR, and initially zeroed if ZERO-MEMORY-P evaluates
to a non-nil value."
  (let* ((c (gensym "C"))
         (count-var (if (constantp count env) count c)))
    `(let (,@(if (constantp count env) nil `((,c ,count))))
       (cffi:with-foreign-object (,var ,type ,count-var)
         #+with-assertions (assert (not (cffi:null-pointer-p ,var)))
         (when ,zero-memory-p
           (os-bzero ,var (* ,count-var
                             ,(if (constantp type env)
                                  (cffi:foreign-type-size (eval type))
                                `(cffi:foreign-type-size ,type)))))
         ,@forms))))


(defmacro with-foreign-objects (bindings &body forms)
  (do ((body `(progn ,@forms))
       (bindings (reverse bindings) (cdr bindings)))
      ((not bindings) body)
    (setq body `(with-foreign-object ,(car bindings) ,body))))



(defcfun (memset "memset")
    :pointer
  (ptr (:pointer :void))
  (c :int)
  (len size-t))


#-mswindows
(defcfun (os-bzero "bzero")
    :void
  (ptr (:pointer :void))
  (len size-t))


#+mswindows
(defun os-bzero (ptr len)
  (memset ptr 0 len))


(defcfun (os-memcpy "memcpy")
    :pointer
  (dst :pointer)
  (src :pointer)
  (len size-t))
