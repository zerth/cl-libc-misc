(in-package :net.mwatters.libc-misc)


(defcfun (get-rlimit "getrlimit")
    :int
  (resource rlimit-resource-type)
  (rlimit (:pointer (:struct rlimit))))


(defcfun (set-rlimit "setrlimit")
    :int
  (resource rlimit-resource-type)
  (rlimit (:pointer (:struct rlimit))))


(defun get-resource-limit (which)
  "Return the current resource limit value for WHICH, an
RLIMIT-RESOURCE-TYPE keyword \(e.g., :FILE-DESCRIPTORS\).  Returns as
two values the current soft and hard limit values \(either integers or
:INFINITY\)."
  (with-foreign-object (rlimit '(:struct rlimit))
    (check-errno (get-rlimit which rlimit)
                 "failed to getrlimit\(\)")
    (cffi:with-foreign-slots ((soft hard) rlimit (:struct rlimit))
      (values (if (= +rlimit-infinity+ soft)
                  :infinity
                soft)
              (if (= +rlimit-infinity+ hard)
                  :infinity
                hard)))))


(defun set-resource-limit (which value)
  "Attempt to set the soft resource limit denoted by WHICH to the
given value.  If VALUE is :INFINITY or greater than the current hard
limit, the value is clamped to the current hard limit value.  Returns
as values the new soft and hard limits."
  (multiple-value-bind (cur-soft cur-hard)
      (get-resource-limit which)
    (declare (ignore cur-soft))
    (with-foreign-object (rlimit '(:struct rlimit))
      (cffi:with-foreign-slots ((soft hard) rlimit (:struct rlimit))
        (unless (eq :infinity cur-hard)
          (when (or (eq :infinity value)
                    (> value cur-hard))
            #+nil (warn "clamping requested value ~A for ~A to maximum ~A"
                        value which cur-hard)
            (setq value cur-hard)))
        ;;
        (setq
         hard (if (eq :infinity cur-hard)
                  +rlimit-infinity+
                cur-hard)
         soft (if (eq :infinity value)
                  +rlimit-infinity+
                value))
        (check-errno (set-rlimit which rlimit)
                     "failed to setrlimit\(\)")
        #+nil (warn "set resource limit value for ~A to ~A" which soft)
        (values soft hard)))))


(defun (setf get-resource-limit) (value which)
  (set-resource-limit which value)
  value)
