;(defun list-get (k lst &key test )
;  "this is a function get value from list like hash  "
;  (if (null test)
;    (elt lst (+ (position-if #'(lambda (x) (string-equal x k))  lst ) 1))
;    (elt lst (+ (position-if #'(lambda (x) (funcall test x k))  lst ) 1))))

(in-package :cl-user )

(defpackage :qingluan.file
  (:use :cl)
  (:export :read-file
	   :with-file
	   :list-directory
	   :file-exists-p
	   :directory-pathname-p
	   :file-pathname-p
	   :walk-directory
	   :directory-p
	   :file-p
	   :pathname-as-file
	   :pathname-as-directory))

(in-package :qingluan.file)

;; ============  about file name . path . test-exists =====

(defun component-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (directory)
  "a function test if it is a directory name "
  (and
   (not (component-p (pathname-name directory )))
   (not (component-p (pathname-type directory )))
   directory))

(defun pathname-as-directory (name)
  (let ((pathname name))
    (when
	(wild-pathname-p pathname)
      (error "Can't convert wild pathnames ."))
    (if (not (directory-pathname-p pathname))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name     nil
	 :type     nil
	 :defaults  pathname)
	pathname)))

(defun directory-wildcard (pathname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory pathname))) 

(defun list-directory (direname)
  (when
      (wild-pathname-p direname)
    (error "Can only list concrete directory names ."))
  (directory (directory-wildcard direname)))

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "can't reliably convert wild pathname."))
    (if (directory-pathname-p pathname)
	(let*
	    ((directory (pathname-as-directory pathname))
	     (name-and-type (pathname (first (last  directory)))))
	  (make-pathname
	   :directory (butlast directory)
	   :name (pathname-name name-and-type)
	   :type (pathname-type name-and-type)
	   :defaults pathname))
	  pathname)))

(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)
  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory file))
      (probe-file (pathname file)))

  #+clisp
  (or (ignore-errors
	(let
	    ((directory-form  (pathname-as-file pathname)))
	  (when
	      (ext:probe-directory directory-form)
	    directory))))

  #-(or sbcl cmu lispworks openmcl allegro  clisp )
  (error "file-exitst-p  is not immplemention"))

	     
		   

;; ============   follow is about io operator =============
(defmacro read-file (filename  read-way  as  to-file use-method  &rest body )
  (let 
    ((f-name (gensym))
     (f-read-way (gensym))
     (to (gensym))
     (use-fun (gensym)))
    `(let 
	 ((,f-name ,filename)
	  (,to ,to-file)
	  (,f-read-way ,read-way)
	  (,use-fun ,use-method))
       (if (null ,to)
	   (with-open-file (%f%  ,f-name :direction :input)
	     (do
	      ((,as (funcall  ,f-read-way  %f% nil) 
		    (funcall  ,f-read-way  %f% nil) ))
	      ((null ,as))
	       ,@body))
	   (with-open-file (%to-file% ,to :direction :output
				      :if-exists :supersede)
	     (with-open-file (%f% ,f-name :direction :input )
	       (do
		((,as (funcall ,f-read-way %f% nil)
		      (funcall ,f-read-way %f% nil)))
		((null ,as))
		 (progn
		     ,@body
		    (funcall ,use-fun ,as %to-file%)))))))))


(defmacro with-file (&rest args ) 
  "key :  do read-by as to use"
  (let 
     ((temp-var (gensym))
      (do (gensym))
      (filename (gensym))
      (read-w (gensym))
      (to-file (gensym))
      (use-fun (gensym)))
     (progn 
        (setq do (getf (subseq args 1) :DO ))
        (if (find :read-by args ) 
             (setq read-w   (getf (subseq args 1) :read-by ))
             (setq read-w #'read-char))
	(setq use-fun nil)
	(if (find :TO args)
	   (progn
	     (setq to-file (getf (subseq  args 1 ) :TO ))
	     (setq use-fun  (getf (subseq  args 1 ):USE )))
	   (setq to-file nil))
        (setq temp-var (getf (subseq args 1) :AS ))
        (setq filename (car args))
        `(read-file   ,filename ,read-w ,temp-var  ,to-file ,use-fun ,do   ))))
