(defmacro n-do (n &rest body) 
  (let 
    ((var (gensym))
     (h (gensym)))
    `(do 
       ((var 0 (+ var 1))
        (h ,n))
       ((> var h))
       ,@body)))
    
    
(defmacro each (n var &rest body ) 
  (let 
    ((pro (gensym)))
    `(let 
       ((,pro ,n))
       (typecase ,pro
                 (number 
                   (loop for ,var from 0 to ,pro collect  
                         ,@body))
                 (list 
                   (loop for ,var in ,pro collect
                         ,@body))
                 (string 
                   (loop for ,var across ,pro collect 
                         ,@body))))))
                 


(defun arrange (start end &optional (precision 1))
  (loop for i from start below end by precision collect i))


(defun list-get (k lst &key test )
  "this is a function get value from list like hash  "
  (if (null test)
    (elt lst (+ (position-if #'(lambda (x) (string-equal x k))  lst ) 1))
    (elt lst (+ (position-if #'(lambda (x) (funcall test x k))  lst ) 1))))

(defmacro find-mul (lst &rest items )
  `(loop for item in ',items if (find item ,lst)
     collect  item into result 
     finally (return  result)))


(defun filter (fun lst )
  (let
      ((acc nil))
    (dolist (i lst)
      (when  (funcall fun i)
	(push i acc)))
    acc))


(defun most (fun lst)
  (let
      ((item (car lst )))
    (dolist (i (cdr lst))
      (when (funcall fun i item )
	(setf item i )))
    item))



(defmacro with-list (lst &rest args )
  (let
      ((l (gensym))
       (fun (gensym))
       (do (gensym)))
    `(let
	((,l  ,lst) 
	 (,fun ,(symbol-function
	       (intern (string-upcase (list-get 'by args)))))
	 (,do ,(symbol-function (intern (string-upcase (list-get 'do args))))))
       (funcall ,do ,fun ,l))))
