;;;; Special thanks to edgar from the Lisp Forum

(defun result-handler (w e)
  (let ((return-value (and e (text e))))
    ;; if the second argument is NIL, the RETURN-VALUE is NIL too
    ;; this signals that the result-box has been cancelled
    (grab-release w)
    (withdraw w)
    (destroy w)
    (format t "return-value ~s~%" return-value)  ; debugging message
    ;; here call functions to process the RETURN-VALUE
    return-value))  ; the RETURN-VALUE here is rather useless

(defun result-box (prompt &key (title "") default (word-wrap "none"))
  (let* ((w (make-instance 'toplevel :title title))
         (l (make-instance 'label :master w :text prompt))
         (e (make-instance 'scrolled-text :master w :width 40))
         (f (make-instance 'frame :master w))
         (b_ok (make-instance 'button :master f :text "Ok"
                              :command (lambda ()
                                         (result-handler w e)))))
    (pack l :side :top :anchor :w)
    (pack e :side :top :expand 1 :fill :both)
    (pack f :side :top :anchor :e)
    (pack b_ok :side :right)
    ;; pressing the <Return> key on the keyboard closes the window
    (bind w "<Return>" (lambda (event)
                         (declare (ignore event))
                         (result-handler w e)))
    ;; clicking the window manager's <close> button closes the window
    ;; the second argument NIL means the dialog has been cancelled
    (on-close w (lambda () (result-handler w nil)))
    (configure (textbox e) :width *text-width*
                           :highlightcolor *highlight-color*
                           :foreground *text-color*
                           :background *field-color*
                           :highlightthickness *highlight-thickness*
                           :selectbackground *select-background*
                           :undo t
                           :wrap word-wrap
                           :maxundo 100)
    (when (and default (> (length default) 0))
      (setf (text e) default))
    (focus e)
    (grab w)
    (format t "result-box~%")  ; debugging message
    nil))  ; the return-value of NIL is rather useless
