(load "./macros.lisp")


(defun m* (&rest args)
    (typecase (car args)
        (number  (apply #'* args))
        (string (apply #'(lambda (str c ) 
          (let 
            ((result str))
            (n-do (- c 1) 
                (setf result (string-concat result str ))) 
            result))))))   

(defun m+ (&rest args)
  (typecase (car args)
    (number (apply #'+ arsg))
    (list (apply #'append args))
    (string (apply #'string-concat args))
    (nil)))