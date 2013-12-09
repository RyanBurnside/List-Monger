;;;; Ryan Burnside 12-06-2013, 12:28 AM, don't muck with it!

(defun text-input-with-check (prompt check-prompt 
			      &key (title "") (default ""))
  (let* ((*exit-mainloop* nil)
	 (w (make-instance 'toplevel :title title))
         (l (make-instance 'label :master w :text prompt))
         (e (make-instance 'entry :master w :width 40))
	 (f (make-instance 'frame :master w))
	 (use-reg (make-instance 'check-button :master w
				      :text check-prompt))
	 
         (b_ok (make-instance 'button :master f :text "Ok"
                              :command (lambda ()
                                         (let ((result (list (text e) 
							     (value use-reg))))
					   (destroy w)
					   (break-mainloop)
					   (print result)
					   (return-from text-input-with-check 
					     result)))))
	 (cancel (make-instance 'button :master f :text "Cancel"
                              :command (lambda ()
                                         (let ((result (list "" nil)))
					   (destroy w)
					   (break-mainloop)
					   (print result)
					   (return-from 
					    text-input-with-check result))))))
    (pack l :side :top :anchor :w)
    (pack e :side :top :expand 1 :fill :both)
    (pack f :side :top :anchor :e)
    (pack cancel :side :left)
    (pack use-reg :side :left)
    (pack b_ok :side :right)
    ;; pressing the <Return> key on the keyboard closes the window
    (bind w "<Return>" (lambda (event)
                         (declare (ignore event))
			 (let ((result (list (text e) 
					     (value use-reg))))
			   (destroy w)
			   (break-mainloop)
			   (format t "Returning ~a" result)
			   (return-from text-input-with-check result))))
    ;; clicking the window manager's <close> button closes the window
    ;; the second argument NIL means the dialog has been cancelled
    (on-close w (lambda () 
		  (let ((result (list "" "")))
		    (destroy w)
		    (break-mainloop)
		    (format t "Returning ~a" result)
		    (return-from text-input-with-check result))))

    (if (> (length default) 0)
      (setf (text e) default)
      (setf (text e) ""))
    (focus e)
    (grab w)
    (mainloop)
    (break-mainloop)
    (grab-release w)
    (destroy w)
    (format t "Returning ~a" result)
    (list "" "" nil)))  ; the return-value of NIL is rather useless
