(in-package #:cesdi)

(defclass cesdi:cesdi-mixin (c2mop:standard-class) ())

(defgeneric cesdi:compute-effective-slot-definition-initargs (class direct-slot-definitions)
  (:method ((class cesdi:cesdi-mixin) slotds)
    (multiple-value-bind (initform initfunction)
        (let ((slotd (find-if #'identity slotds :key #'c2mop:slot-definition-initfunction)))
          (if slotd
              (values (c2mop:slot-definition-initform slotd)
                      (c2mop:slot-definition-initfunction slotd))
              (values nil nil)))
      (let ((first-slotd (first slotds)))
        (list :name (c2mop:slot-definition-name first-slotd)
              :allocation (c2mop:slot-definition-allocation first-slotd)
              :initform initform
              :initfunction initfunction
              :type (let ((types (mapcan (lambda (slotd)
                                           (let ((type (c2mop:slot-definition-type slotd)))
                                             (unless (eq type t)
                                               (list type))))
                                         slotds)))
                      (if types
                          `(and ,@types)
                          t))
              :initargs (delete-duplicates (mapcan (lambda (slotd)
                                                     (copy-seq (c2mop:slot-definition-initargs slotd)))
                                                   slotds)
                                           :test #'eq)
              :documentation (let ((slotd (find-if #'identity slotds :key (lambda (slotd)
                                                                            (documentation slotd t)))))
                               (and slotd (documentation slotd t))))))))

(defun %mappl (function plist)
  (mapl (let ((processp t))
          (lambda (tail)
            (prog1 (when processp
                     (funcall function (first tail) (second tail)))
              (setf processp (not processp)))))
        plist))

(defparameter *%standard-slot-definition-initarg-to-writer*
  (let ((hash (make-hash-table :test 'eq)))
    (prog1 hash
      (%mappl (lambda (initarg writer)
                (if (typep writer '(cons (eql setf)))
                    (let ((setf-function (and (fboundp writer) (fdefinition writer))))
                      (when setf-function
                        (setf (gethash initarg hash) setf-function)))
                    (setf (gethash initarg hash) writer)))
              (list :name '(setf c2mop:slot-definition-name)
                    :allocation '(setf c2mop:slot-definition-allocation)
                    :initform '(setf c2mop:slot-definition-initform)
                    :initfunction '(setf c2mop:slot-definition-initfunction)
                    :type '(setf c2mop:slot-definition-type)
                    :initargs '(setf c2mop:slot-definition-initargs)
                    :documentation (lambda (new-documentation slotd)
                                     (setf (documentation slotd t) new-documentation)))))))

(defun %amend (effective-slot-definition amendments)
  (let* ((seen (make-hash-table :test 'eq))
         (class (class-of effective-slot-definition))
         (class-slots (c2mop:class-slots class))
         (initarg-to-writer *%standard-slot-definition-initarg-to-writer*))
    (%mappl (lambda (key value)
              (unless (gethash key seen)
                (setf (gethash key seen) t)
                (let ((standard-writer (gethash key initarg-to-writer)))
                  (if standard-writer
                      (funcall standard-writer value effective-slot-definition)
                      (let ((slot-name
                             (let ((slotd (find-if (lambda (slotd)
                                                     (member key (c2mop:slot-definition-initargs slotd)))
                                                   class-slots)))
                               (if slotd
                                   (c2mop:slot-definition-name slotd)
                                   (error "Invalid initarg ~S for class ~S." key class)))))
                        (setf (slot-value effective-slot-definition slot-name) value))))))
            amendments))
  effective-slot-definition)

(defvar *effective-slot-definition-class*)

(defmethod c2mop:compute-effective-slot-definition ((class cesdi:cesdi-mixin) name direct-slot-definitions)
  (declare (ignore name))
  (let* ((initargs (cesdi:compute-effective-slot-definition-initargs class direct-slot-definitions))
         (*effective-slot-definition-class* (apply #'cesdi:effective-slot-definition-class class initargs)))
    (%amend (call-next-method)
            (let ((standard-tail (last initargs 14)))
              (if (eq (first standard-tail) :name)
                  (ldiff initargs standard-tail)
                  (error "~S failed to return the standard initargs tail given class ~S. Initargs:~%~S"
                         'cesdi:compute-effective-slot-definition-initargs class initargs))))))

(defgeneric cesdi:effective-slot-definition-class (class &rest initargs)
  (:method ((class c2mop:standard-class) &rest initargs)
    (declare (ignore class initargs))
    (find-class 'c2mop:standard-effective-slot-definition)))

(defmethod c2mop:effective-slot-definition-class ((class cesdi:cesdi-mixin) &rest initargs)
  (declare (ignore class initargs))
  *effective-slot-definition-class*)
