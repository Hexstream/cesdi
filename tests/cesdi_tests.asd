(asdf:defsystem #:cesdi_tests

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "cesdi unit tests."

  :depends-on ("cesdi"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:cesdi_tests)))
