(cl:defpackage #:cesdi_tests
  (:use #:cl #:parachute))

(cl:in-package #:cesdi_tests)

(defclass test-metaclass (cesdi:cesdi-mixin)
  ())

(defmethod c2mop:validate-superclass ((class test-metaclass) (superclass c2mop:standard-class))
  t)

(defclass custom-mixin ()
  ((%custom :initarg :custom
            :reader custom)))

(defclass test-direct-slot-definition (c2mop:standard-direct-slot-definition custom-mixin)
  ())

(defmethod c2mop:direct-slot-definition-class ((class test-metaclass) &key (custom nil customp) &allow-other-keys)
  (declare (ignore custom))
  (if customp
      (find-class 'test-direct-slot-definition)
      (call-next-method)))

(defmethod cesdi:effective-slot-definition-class ((class test-metaclass) &key (custom nil customp) &allow-other-keys)
  (declare (ignore custom))
  (if customp
      (find-class 'test-effective-slot-definition)
      (call-next-method)))

(defclass test-effective-slot-definition (c2mop:standard-effective-slot-definition custom-mixin)
  ())

(defmethod cesdi:compute-effective-slot-definition-initargs ((class test-metaclass) direct-slot-definitions)
  (let ((rest (call-next-method))
        (custom-direct-slot (find-if (lambda (slotd)
                                       (typep slotd 'test-direct-slot-definition))
                                     direct-slot-definitions)))
    (if custom-direct-slot
        (list* :custom (custom custom-direct-slot) rest)
        rest)))

(defclass test-class ()
  ((%normal-slot :initarg :normal
                 :reader normal)
   (%custom-slot :initarg :custom
                 :custom :yay))
  (:metaclass test-metaclass))


(defclass test2-metaclass (cesdi:cesdi-mixin)
  ())

(defmethod c2mop:validate-superclass ((class test2-metaclass) (superclass c2mop:standard-class))
  t)

(defmethod cesdi:compute-effective-slot-definition-initargs ((class test2-metaclass) direct-slot-definitions)
  (list* :allocation :class (call-next-method)))

(defclass test2-class ()
  (%slot)
  (:metaclass test2-metaclass))


(define-test "main"
  (let* ((class (find-class 'test-class))
         (class-slots (progn
                        (c2mop:finalize-inheritance class)
                        (c2mop:class-slots class)))
         (normal-slot (find '%normal-slot class-slots :key #'c2mop:slot-definition-name))
         (custom-slot (find '%custom-slot class-slots :key #'c2mop:slot-definition-name)))
    (is eq (find-class 'c2mop:standard-effective-slot-definition)
        (class-of normal-slot))
    (is eq (find-class 'test-effective-slot-definition)
        (class-of custom-slot))
    (is eq :yay (custom custom-slot)))
  (let ((test (make-instance 'test-class :normal :value :custom :other-value)))
    (is eq :value (normal test))
    (is eq :other-value (slot-value test '%custom-slot)))
  (let ((class (find-class 'test2-class)))
    (c2mop:finalize-inheritance class)
    (is eq :class
        (c2mop:slot-definition-allocation (first (c2mop:class-slots class))))))
