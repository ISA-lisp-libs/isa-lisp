

(defmacro while (test &rest body)
  `(do  
    ()
    ((not ,test))
    ,@body))


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
                 
