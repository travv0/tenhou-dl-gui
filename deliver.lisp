;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

;;; Load the application:
(load "/home/travis/code/lisp/tenhou-dl-gui/build.lisp")

;; (compile-system 'tenhou-dl-gui :load t)

(deliver 'tenhou-dl-gui:start "/home/travis/code/lisp/tenhou-dl-gui/tenhou-dl" 4
         :interface :capi
         :startup-bitmap-file nil)
