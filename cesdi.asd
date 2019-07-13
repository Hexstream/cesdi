(asdf:defsystem #:cesdi

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  ;; See the UNLICENSE file for details.
  :license "Unlicense"

  :description "Provides a compute-effective-slot-definition-initargs generic function that allows for more ergonomic initialization of effective slot definition objects."

  :depends-on ("closer-mop")

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
	       (:file "main"))

  :in-order-to ((asdf:test-op (asdf:test-op #:cesdi_tests))))
