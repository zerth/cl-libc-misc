(in-package :libc-misc)

(include "sys/types.h")
;; fixme; are some of these cffi builtins?
(ctype mode-t "mode_t")
(ctype uintptr-t "uintptr_t")
(ctype intptr-t "intptr_t")
(ctype time-t "time_t")
(ctype size-t "size_t")
(ctype ssize-t "ssize_t")
(ctype off-t "off_t")


(include "errno.h")
(constant (+resource-not-available+ "EAGAIN"))
(constant (+too-many-open-files+ "EMFILE"))
(constant (+system-call-interrupted+ "EINTR"))
(constant (+bad-file-descriptor+ "EBADF"))
(constant (+connection-reset+ "ECONNRESET"))
(constant (+operation-in-progress+ "EINPROGRESS"))
;; fixme; either use friendly names for all errors, or use unfriendly
;; names for everything to be more consistent
(constant (+e-exist+ "EEXIST"))
(constant (+e-no-space+ "ENOSPC"))
(constant (+e-pipe+ "EPIPE"))
(constant (+e-perm+ "EPERM"))
(constant (+e-io+ "EIO"))
(constant (+e-invalid+ "EINVAL"))
(constant (+e-not-socket+ "ENOTSOCK"))
(constant (+e-access+ "EACCES"))
(constant (+e-not-connected+ "ENOTCONN"))


#-(or freebsd) (cvar ("errno" *errno*) :int)


(include "time.h")
#+(or freebsd) (include "sys/timespec.h")
(cstruct timespec "struct timespec"
  (seconds "tv_sec" :type time-t)
  (nanoseconds "tv_nsec" :type :long))


(include "sys/stat.h")
(bitfield file-mode-flags
  ((:owner-rwx "S_IRWXU"))
  ((:owner-r   "S_IRUSR"))
  ((:owner-w   "S_IWUSR"))
  ((:owner-x   "S_IXUSR"))
  ((:group-rwx "S_IRWXG"))
  ((:group-r   "S_IRGRP"))
  ((:group-w   "S_IWGRP"))
  ((:group-x   "S_IXGRP"))
  ((:world-rwx "S_IRWXO"))
  ((:world-r   "S_IROTH"))
  ((:world-w   "S_IWOTH"))
  ((:world-x   "S_IXOTH")))


(include "stdio.h")
(constantenum seek-whence
  ((:start "SEEK_SET"))
  ((:current "SEEK_CUR"))
  ((:end "SEEK_END")))


(include "sys/uio.h")
(cstruct iovec "struct iovec"
  (base "iov_base" :type (:pointer :void))
  (len "iov_len" :type size-t))


(include "fcntl.h")
(bitfield file-open-flags
  ((:read+write "O_RDWR"))
  ((:read "O_RDONLY"))
  ((:write "O_WRONLY"))
  ((:non-blocking "O_NONBLOCK"))
  ((:exclusive "O_EXCL"))
  ((:no-follow "O_NOFOLLOW"))
  ((:create "O_CREAT"))
  #+(or freebsd) ((:locked-exclusive "O_EXLOCK"))
  #+(or freebsd) ((:locked-shared "O_SHLOCK")))


(constantenum fcntl-command
  ((:set-flags "F_SETFL"))
  ((:get-flags "F_GETFL"))
  ((:set-lock "F_SETLK"))
  ((:get-lock "F_GETLK")))


(constantenum flock-lock-type
  ((:locked-shared "F_RDLCK"))
  ((:locked-exclusive "F_WRLCK"))
  ((:unlock "F_UNLCK")))


(cstruct flock "struct flock"
  (lock-type "l_type" :type :short) ;flock-lock-type
  (whence "l_whence" :type :short) ;seek-whence
  (start "l_start" :type off-t)
  (len "l_len" :type off-t)) ; 0 means lock entire file


(include "sys/resource.h")

(ctype rlim-t "rlim_t")

(cstruct rlimit "struct rlimit"
  (soft "rlim_cur" :type rlim-t)
  (hard "rlim_max" :type rlim-t))

(constantenum rlimit-resource-type
  ((:address-space "RLIMIT_AS"))
  ((:core-file-size "RLIMIT_CORE"))
  ((:cpu-time "RLIMIT_CPU"))
  ((:data-size "RLIMIT_DATA"))
  ((:resident-size "RLIMIT_RSS"))
  ((:stack-size "RLIMIT_STACK"))
  ((:file-size "RLIMIT_FSIZE"))
  ((:file-descriptors "RLIMIT_NOFILE")))

(constant (+rlimit-infinity+ "RLIM_INFINITY") :type integer)
