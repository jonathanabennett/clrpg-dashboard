;;;; clrpg-dashboard.asd

(asdf:defsystem #:clrpg-dashboard
  :description "Describe clrpg-dashboard here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :depends-on (#:mcclim)
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "clrpg-dashboard")))
