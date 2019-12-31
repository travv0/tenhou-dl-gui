(defpackage #:tenhou-dl-gui
  (:use #:cl #:capi #:tenhou-dl))

(in-package #:tenhou-dl-gui)

(defun create-window ()
  (let ((tenhou-id-input (make-instance 'text-input-pane :title "Tenhou ID"))
        (save-path-input (make-instance 'text-input-pane
                                        :title "Save Path"
                                        :file-completion t
                                        :directories-only t
                                        :editing-callback
                                        (lambda (pane type)
                                          (when (eql type :start)
                                            (text-input-pane-complete-text pane)))))
        (output-textbox (make-instance 'collector-pane :enabled nil)))
    (contain
     (make-instance
      'column-layout
      :description
      (list tenhou-id-input
            save-path-input
            (make-instance
             'push-button
             :data "Download"
             :default-p t
             :callback-type :item
             :callback
             (lambda (item)
               (cond ((or (= 0 (length (text-input-pane-text tenhou-id-input)))
                          (= 0 (length (text-input-pane-text save-path-input))))
                      (display-message "Tenhou ID and save path are required."))
                     (t (setf (button-enabled item) nil)
                        (bt:make-thread
                         (lambda ()
                           (let ((*standard-output* (collector-pane-stream output-textbox)))
                             (format t "~%Downloaded ~a replay~:p~%"
                                     (length (download-replays
                                              (text-input-pane-text tenhou-id-input)
                                              (text-input-pane-text save-path-input))))
                             (setf (button-enabled item) t))))))))
            :separator
            output-textbox)))))
