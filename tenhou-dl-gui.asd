(asdf:defsystem #:tenhou-dl-gui
  :description "Describe tenhou-dl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "./src/"
  :depends-on (:travv0.prelude :tenhou-dl :lparallel :cl-cpus)
  :components ((:file "tenhou-dl-gui")))
