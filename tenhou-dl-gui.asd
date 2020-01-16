(asdf:defsystem #:tenhou-dl-gui
  :description "Describe tenhou-dl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "./src/"
  :depends-on (:tenhou-dl :travv0.utils :lparallel :cl-cpus)
  :components ((:file "tenhou-dl-gui")))
