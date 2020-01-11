(defpackage #:tenhou-dl-gui
  (:use #:travv0.prelude #:capi #:tenhou-dl)
  (:export #:start))

(in-package #:tenhou-dl-gui)

(defparameter *config-path* "~/.tenhou_dl")

(define-interface window ()
  ()
  (:panes (tenhou-id-input text-input-pane
                           :title "Tenhou ID"
                           :visible-min-width '(:character 30))
          (save-path-input text-input-pane
                           :title "Save Path"
                           :visible-min-width '(:character 30)
                           :file-completion t
                           :directories-only t
                           :editing-callback
                           (lambda (pane type)
                             (when (eql type :start)
                               (text-input-pane-complete-text pane))))
          (output-textbox collector-pane
                          :enabled :read-only
                          :visible-min-height '(:character 20))
          (download-button push-button
                           :data "Download"
                           :default-p t
                           :callback 'download))
  (:layouts (main-layout column-layout
                         '(top-layout
                           :separator
                           output-textbox))
            (top-layout row-layout
                        '(tenhou-id-input
                          save-path-input
                          download-button)))
  (:default-initargs :title "Tenhou DL"
                     :destroy-callback (lambda (interface)
                                         (declare (ignore interface))
                                         (lparallel:end-kernel))))

(defun save-input (tenhou-id-input save-path-input)
  (with-open-file (out *config-path*
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print (list :tenhou-id (text-input-pane-text tenhou-id-input)
                   :save-path (text-input-pane-text save-path-input))
             out))))

(defun load-input (tenhou-id-input save-path-input)
  (when (probe-file *config-path*)
    (with-open-file (in *config-path*)
      (with-standard-io-syntax
        (let ((config (read in)))
          (setf (text-input-pane-text tenhou-id-input)
                (getf config :tenhou-id)
                (text-input-pane-text save-path-input)
                (getf config :save-path)))))))

(defun download (data interface)
  (display-errors
    (with-slots (download-button tenhou-id-input save-path-input output-textbox) interface
      (cond ((or (= 0 (length (text-input-pane-text tenhou-id-input)))
                 (= 0 (length (text-input-pane-text save-path-input))))
             (display-message "Tenhou ID and save path are required."))
            (t (save-input tenhou-id-input save-path-input)
               (setf (button-enabled download-button) nil)
               (bt:make-thread
                (lambda ()
                  (let ((*standard-output* (collector-pane-stream output-textbox)))
                    (unwind-protect
                         (format t "~%Downloaded ~a replay~:p~%"
                                 (length (download-replays
                                          (text-input-pane-text tenhou-id-input)
                                          (text-input-pane-text save-path-input))))
                      (setf (button-enabled download-button) t))))))))))

(defun start ()
  (setf lparallel:*kernel*
        (lparallel:make-kernel (cpus:get-number-of-processors)))
  (let ((window (make-instance 'window)))
    (with-slots (tenhou-id-input save-path-input) window
      (load-input tenhou-id-input save-path-input))
    (display window)))
