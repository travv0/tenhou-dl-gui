;;; Automatically generated delivery script

(in-package "CL-USER")

(load-all-patches)

;;; Load the application:
(load "~/code/common-lisp/tenhou-dl-gui/build.lisp")

(deliver 'tenhou-dl-gui:start
         #+:cocoa
         (create-macos-application-bundle
          "~/Desktop/Tenhou DL.app"
          ;; Do not copy file associations...
          :document-types nil
          ;; ...or CFBundleIdentifier from the LispWorks bundle
          ;; :identifier "com.example.Hello"
          )
         #-:cocoa "~/code/common-lisp/tenhou-dl-gui/tenhou-dl"
         4
         :interface :capi
         :startup-bitmap-file nil
         :keep-top-level nil)
