;;;; snekeyes.asd

(asdf:defsystem #:snekeyes
  :description "A Matrix bot to roll dice."
  :author "Shoshin <shshoshin@protonmail.com>"
  :license  "AGPL"
  :version "0.0.1"
  :serial t
  :depends-on (#:granolin #:cl-ppcre)
  :components ((:file "package")
               (:file "snekeyes")))
