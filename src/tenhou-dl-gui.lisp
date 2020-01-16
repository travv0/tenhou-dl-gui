(defpackage #:tenhou-dl-gui
  (:use #:cl #:tenhou-dl)
  (:export #:start))

(in-package #:tenhou-dl-gui)

(defparameter *config-path* "~/.tenhou_dl")

(capi:define-interface window ()
  ()
  (:panes (tenhou-id-input capi:text-input-pane
                           :title "Tenhou ID"
                           :visible-min-width '(:character 30))
          (save-path-input capi:text-input-pane
                           :title "Save Path"
                           :visible-min-width '(:character 30)
                           :file-completion t
                           :directories-only t
                           :editing-callback
                           (lambda (pane type)
                             (when (eql type :start)
                               (capi:text-input-pane-complete-text pane))))
          (output-textbox capi:collector-pane
                          :enabled :read-only
                          :visible-min-height '(:character 20))
          (download-button capi:push-button
                           :data "Download"
                           :default-p t
                           :callback 'download))
  (:layouts (main-layout capi:column-layout
                         '(top-layout
                           :separator
                           output-textbox))
            (top-layout capi:row-layout
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
      (print (list :tenhou-id (capi:text-input-pane-text tenhou-id-input)
                   :save-path (capi:text-input-pane-text save-path-input))
             out))))

(defun load-input (tenhou-id-input save-path-input)
  (when (probe-file *config-path*)
    (with-open-file (in *config-path*)
      (with-standard-io-syntax
        (let ((config (read in)))
          (setf (capi:text-input-pane-text tenhou-id-input)
                (getf config :tenhou-id)
                (capi:text-input-pane-text save-path-input)
                (getf config :save-path)))))))

(tu:desfun download (_data interface)
  (capi:display-errors
    (with-slots (download-button tenhou-id-input save-path-input output-textbox) interface
      (cond ((or (= 0 (length (capi:text-input-pane-text tenhou-id-input)))
                 (= 0 (length (capi:text-input-pane-text save-path-input))))
             (capi:display-message "Tenhou ID and save path are required."))
            (t (save-input tenhou-id-input save-path-input)
               (setf (capi:button-enabled download-button) nil)
               (bt:make-thread
                (lambda ()
                  (let ((*standard-output* (capi:collector-pane-stream output-textbox)))
                    (unwind-protect
                         (format t "~%Downloaded ~a replay~:p~%"
                                 (length (download-replays
                                          (capi:text-input-pane-text tenhou-id-input)
                                          (capi:text-input-pane-text save-path-input))))
                      (setf (capi:button-enabled download-button) t))))))))))

(defun start ()
  (setf lparallel:*kernel*
        (lparallel:make-kernel (cpus:get-number-of-processors)))
  (let ((window (make-instance 'window)))
    (with-slots (tenhou-id-input save-path-input) window
      (load-input tenhou-id-input save-path-input))
    (capi:display window)))
