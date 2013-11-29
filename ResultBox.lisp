(defun result-box (prompt &key (title "") default (word-wrap "none"))
  (let* ((*exit-mainloop* nil)
	 (return-value default)
	 (ok t)
	 (w (make-instance 'toplevel :title title))
	 (l (make-instance 'label :master w :text prompt))
	 (e (make-instance 'scrolled-text :master w :width 40))
	 (f (make-instance 'frame :master w))
	 (b_ok (make-instance 'button :master f :text "Ok" 
			      :command (lambda ()
					 (break-mainloop) w
					 )))
	 )
    (pack l :side :top :anchor :w)
    (pack e :side :top :expand 1 :fill :both)
    (pack f :side :top :anchor :e)
    (pack b_ok :side :right)
    (bind w "<Return>" (lambda (event)
			 (declare (ignore event))
			 (break-mainloop)))
    (configure (textbox e) 
	       :width *text-width* 
	       :highlightcolor *highlight-color* 
	       :foreground *text-color*
	       :background *field-color*
	       :highlightthickness *highlight-thickness* 
	       :selectbackground *select-background*
	       :undo t
	       :wrap word-wrap
	       :maxundo 100
		  )


    (when (and default (> (length default) 0))
      (setf (text e) default))
    (focus e)
    (grab w)
    (mainloop)
    (grab-release w)
    (withdraw w)
    (setf return-value (text e))
    (destroy w) ; Destroy main window and free memory
    (and ok
	 return-value)
    ))
