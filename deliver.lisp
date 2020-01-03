;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

;;; Load the application:
(load "~/common-lisp/tenhou-dl-gui/build.lisp")

(deliver 'tenhou-dl-gui:start "~/common-lisp/tenhou-dl-gui/tenhou-dl" 4
         :interface :capi
         :startup-bitmap-file nil
         :keep-top-level nil)
