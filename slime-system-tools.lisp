(package.seed:define-package :slime-system-tools :export-capitalized t)

(in-package :slime-system-tools)

(defvar *probe-asd-file-max-levels* 3)

(defun probe-asd-file (source-file 
                       &optional 
                       dir (tries-left *probe-asd-file-max-levels*))
  (when (< 0 tries-left)
    (setf dir (or dir (pathname-directory source-file)))
    (let ((asds (directory (make-pathname :type "asd"
                                          :name :wild
                                          :directory dir))))
      (cond ((< 1 (length asds))
             (error "More than one asd file found."))

            ((= 1 (length asds))
             (first asds))
            
            (t (probe-asd-file
                source-file
                (butlast dir)
                (1- tries-left)))))))

(defun determine-system-name (source-file asd-file)
  (let ((namestring (enough-namestring 
                     source-file
                     (directory-namestring asd-file))))
    (concatenate 'string
                 (pathname-name asd-file)
                 "/"
                 (directory-namestring namestring)
                 (pathname-name namestring)))) 

(defun Source-file-system-name (source-file)
  (let ((asd (probe-asd-file source-file)))
    (when asd
      (determine-system-name source-file asd))))
