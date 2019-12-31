(defpackage #:tenhou-dl-gui
  (:use #:cl #:capi #:tenhou-dl))

(in-package #:tenhou-dl-gui)

(define-interface window ()
  ()
  (:panes (tenhou-id-input text-input-pane :title "Tenhou ID")
          (save-path-input text-input-pane :title "Save Path"
                           :file-completion t
                           :directories-only t
                           :editing-callback
                           (lambda (pane type)
                             (when (eql type :start)
                               (text-input-pane-complete-text pane))))
          (output-textbox collector-pane :enabled nil)
          (download-button push-button
                           :data "Download"
                           :default-p t
                           :callback #'download))
  (:layouts (main-layout column-layout
                         '(tenhou-id-input
                           save-path-input
                           download-button
                           :separator
                           output-textbox)))
  (:default-initargs :title "Tenhou DL"))

(defun download (data interface)
  (with-slots (download-button tenhou-id-input save-path-input output-textbox) interface
    (cond ((or (= 0 (length (text-input-pane-text tenhou-id-input)))
               (= 0 (length (text-input-pane-text save-path-input))))
           (display-message "Tenhou ID and save path are required."))
          (t (setf (button-enabled download-button) nil)
             (bt:make-thread
              (lambda ()
                (let ((*standard-output* (collector-pane-stream output-textbox)))
                  (unwind-protect
                      (format t "~%Downloaded ~a replay~:p~%"
                              (length (download-replays
                                       (text-input-pane-text tenhou-id-input)
                                       (text-input-pane-text save-path-input))))
                    (setf (button-enabled download-button) t)))))))))
