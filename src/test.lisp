
(defpackage :isa.test
  (:use :cl)
  (:export :test-mac))

(in-package :isa.test)

(defmacro test-mac (fun &rest body)
  (let
      ((as (getf body :as  )))
    `(funcall ,fun  ,@body)))

   
