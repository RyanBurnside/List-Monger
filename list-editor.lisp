;;;; Ryan Burnside 2013 
;;;; LIST MONGER
;;;; This is free and open source software!

;;;; TODO section
; Regex searching and marking
; Line numbers for lists
; More advanced dialogs (collect more information at once vs 3 commands)
; Fix program freezing with "x"ing out of dialogs
; Ensure pop up windows and all members are destroyed after function ends
;;    (no memory leaks)
;; Possibly convert from making tons of list to tons of vectors for speed


;; Yeah, this is for personal configuration, config yours as needed
(load "ltk.fasl")
(in-package :ltk)

;;; My custom result box that returns a scrolled box dialog
(load "result-box.lisp")

(defparameter *window-width* 640)
(defparameter *window-height* 480)
(defparameter *text-width* 40)
(defparameter *text-color* 'Gray15)
(defparameter *field-color* 'Ivory)
(defparameter *select-background* 'DeepSkyBlue)
(defparameter *highlight-color* 'DeepSkyBlue)
(defparameter *highlight-thickness* 3)

;;; Defuns for list manipulation
(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun token-split (string &optional (token #\Newline))
  "Splits a string into a list of strings"
  (loop for start = 0 then (1+ finish)
     for finish = (position token string :start start)
     collecting (subseq string start finish)
     until (null finish)))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun display-key-help ()
  (result-box "Program Key Commands"
	      :default (file-string "./resources/keyboard.txt") 
	      :word-wrap "word"))

(defun display-info ()
  "Gives information on this project"
  (message-box "Ryan Burnside's List Monger 
Written in ANSI Common Lisp using the LTK library.
This is free software released under GPL v 3." "Info" "ok" "info"))

(defun count-lines (widget)
  "Questionable method, counts newline characters"
  (message-box (concatenate 'string "Lines: " 
			    (write-to-string (count #\newline (text widget)))) 
	       "Line Count" "ok" "info"))

(defun line-list (widget &optional (seperator #\Newline))
  "Return a list of lines seperated by seperator from widget"
  (remove "" (token-split (text widget) seperator) :test #'string=))

(defun write-list-to-file (widget)
  (let ((path (get-save-file)))
    (if (not (equal path ""))
	(with-open-file (stream path :direction :output :if-exists :supersede)
	  (format stream (text widget))))))

(defun read-file-to-list (widget)
  (let ((path (get-open-file)))
    (when (not (equal path ""))
      (setf (text widget) (scrub (file-string path))))))

(defun replace-text (widget)
  (let ((term (input-box "Enter term to be replaced:" :default nil))
	(replacement (input-box "Enter replacement:" :default nil)))
    (and term replacement
	 (setf (text widget) 
	       (replace-all (text widget) term replacement :test #'char=)))))

(defun search-all-text-dialog (widget)
  (let ((search-value (input-box "Search term:" :default nil)))
    (if (> (length search-value) 0)
	(search-all-text (textbox widget) search-value))))

(defun search-text-dialog (widget)
  (let ((search-value (input-box "Search term:" :default nil)))
    (if (> (length search-value) 0)
	(search-next-text (textbox widget) search-value))))

(defun scrub (string)
  "Removes white space characters from beginning and end of strings"
  (string-trim '(#\Newline #\Tab #\Space) string))

(defun line-list-to-string (list &optional(add-newlines t))
  "Returns a string made up of a list of strings"
  (if add-newlines
      (scrub (format nil "~{~a~%~}" (remove "" list :test #'string=)))
      (scrub (format nil "~{~a~}" (remove "" list :test #'string=)))))

(defun trim-list-strings (widget) 
  ;TODO doesnlt like fancy web space prefix chars
  "Removes trailing and prefixed space from lines"
  (setf (text widget) 
	(line-list-to-string 
	 (loop for str in (line-list widget)
	    collect (string-trim '(#\Space #\Tab #\Newline) str)))))

(defun line-to-column (widget &optional (seperator #\,))
  "Takes a line seperated by seperator and returns them as newline characters"
  (setf (text widget) (line-list-to-string (line-list widget seperator))))

(defun column-to-line (widget &optional (seperator ","))
  "Turn the widget's columns into a seperator seperated line and set"
  (setf (text widget) (line-list-to-string 
		       (loop for l in (line-list widget)
			  collect (concatenate 'string l ",")) nil)))

(defun sort-lines (widget)
  "Sort lines"
  (let ((lines (sort (line-list widget) #'string<)))
    (setf (text widget) (line-list-to-string lines))))

(defun reverse-lines (widget)
  (setf (text widget) (line-list-to-string (reverse (line-list widget)))))

(defun uppercase-lines (widget)
  (setf (text widget) (scrub (string-upcase (text widget)))))

(defun lowercase-lines (widget)
  (setf (text widget) (scrub (string-downcase (text widget)))))

(defun capitalize-lines (widget)
  (setf (text widget) (scrub (string-capitalize (text widget)))))

(defun enclose (widget)
  (let ((wrap (input-box "Wrapping character(s):" :default nil)))
    (if (> (length wrap) 0)
	(setf (text widget)
	      (line-list-to-string
	       (loop for i in (line-list widget)
		  collect (concatenate 'string wrap i wrap))))))) 

(defun append-prefix (widget)
  (let ((prefix (input-box "Prefix:" :default nil)))
    (if (> (length prefix) 0)
	(setf (text widget)
	      (line-list-to-string
	       (loop for i in (line-list widget)
		  collect (concatenate 'string prefix i))))))) 

(defun append-suffix (widget)
  (let ((suffix (input-box "Suffix:" :default nil)))
    (if (> (length suffix) 0)
	(setf (text widget)
	      (line-list-to-string
	       (loop for i in (line-list widget)
		  collect (concatenate 'string i suffix))))))) 

(defun strip-prefix (widget)
  (let ((prefix (input-box "Prefix to strip off:" :default nil))
	(found-position nil))
    (when (> (length prefix) 0)
      (setf (text widget)
	    (line-list-to-string
	     (loop for i in (line-list widget) do
		  (setf found-position (search prefix i :test #'equal))
		  collect
		  (if (eq found-position 0)
		      (subseq i (length prefix))
		      i)))))))

(defun strip-suffix (widget)
  (let* ((suffix (input-box "Suffix to strip off:" :default nil))
	(found-position nil)
	(len-suffix (length suffix)))
    (when (> (length suffix) 0)
      (setf (text widget)
	    (line-list-to-string
	     (loop for i in (line-list widget) do
		  (setf found-position (search suffix i :from-end t
					       :test #'equal))
		  collect
		  (if (eq found-position (- (length i) len-suffix)) 
		      (subseq i 0 (- (length i) len-suffix))
		      i)))))))
		  
(defun remove-duplicate-lines (widget) ;;return order not insured
  (setf (text widget) (line-list-to-string 
		       (remove-duplicates (line-list widget) :test #'string=))))

(defun remove-containing (widget)
  (let ((str (input-box "Remove lines containing:" :default nil)))
    (when (> (length str) 0)
      (setf (text widget)
	    (line-list-to-string
	     (loop for i in (line-list widget)
		if (not (search str i)) collect i))))))

(defun keep-containing (widget)
  (let ((str (input-box "Keep lines containing:" :default nil)))
    (when (> (length str) 0)
      (setf (text widget)
	    (line-list-to-string
	     (loop for i in (line-list widget)
		if (search str i) collect i))))))

(defun find-intersection (widget widget2)
  (let ((lines1 (line-list widget))
	(lines2 (line-list widget2)))
    (result-box "Result of Intersection Operation (order not preserved)" 
		:default
		(line-list-to-string (intersection lines1 lines2 
						   :test #'string=)))))

(defun find-union (widget widget2)
  (let ((lines1 (line-list widget))
	(lines2 (line-list widget2)))
    (result-box "Result of Union Operation (order not preserved)" 
		:default
		(line-list-to-string (union lines1 lines2 :test #'string=)))))

(defun find-exclusive (widget widget2) ;TODO show differences by list
  (let ((lines1 (line-list widget))
	(lines2 (line-list widget2)))
    (result-box "Result of Exclusive Operation (order not preserved)" 
		:default
		(line-list-to-string 
		 (set-exclusive-or lines1 lines2 :test #'string=)))))

(defun attach-list-menus (parent widget)
  "This populates the left and right menu for both lists"
  (make-menubutton parent "Find"
		   (lambda () (search-text-dialog widget)))
  (make-menubutton parent "Highlight"
		   (lambda () (search-all-text-dialog widget)))
  (make-menubutton parent "Replace"
		   (lambda () (replace-text widget)))
  (make-menubutton parent "Count List" 
		   (lambda () (count-lines widget)))
  (add-separator parent)
  (make-menubutton parent "List to Comma Seperated Line" 
		   (lambda () (column-to-line widget)))
  (make-menubutton parent "Comma Seperated Line to List" 
		   (lambda () (line-to-column widget)))
  (add-separator parent)  
  (make-menubutton parent "Trim Lines" 
		   (lambda () (trim-list-strings widget)))
  (make-menubutton parent "Sort List" 
		   (lambda () (sort-lines widget)))
  (make-menubutton parent "Reverse List" 
		   (lambda () (reverse-lines widget)))
  (make-menubutton parent "Uppercase Words" 
		   (lambda () (uppercase-lines widget)))
  (make-menubutton parent "Lowercase Words" 
		   (lambda () (lowercase-lines widget)))
  (make-menubutton parent "Capitalize Words" 
		   (lambda () (capitalize-lines widget)))
  (add-separator parent)
  (make-menubutton parent "Quote Lines"
		   (lambda () (enclose widget)))
  (make-menubutton parent "Add Prefix"
		   (lambda () (append-prefix widget)))
  (make-menubutton parent "Add Suffix"
		   (lambda () (append-suffix widget)))
  (make-menubutton parent "Remove Prefix" 
		   (lambda () (strip-prefix widget)))
  (make-menubutton parent "Remove Suffix"
		   (lambda () (strip-suffix widget)))
  (add-separator parent)

  (make-menubutton parent "Remove Duplicates" 
		   (lambda () (remove-duplicate-lines widget)))
  (make-menubutton parent "Remove Containing"
		   (lambda () (remove-containing widget)))
  (make-menubutton parent "Keep Containing"
		   (lambda () (keep-containing widget))))


;;; This defun configures the look and behavior for our text widgets
(defun style-textbox (widget)
  (configure (textbox widget) :width *text-width* 
		 :highlightcolor *highlight-color* 
		 :foreground *text-color*
		 :background *field-color*
		 :highlightthickness *highlight-thickness* 
		 :selectbackground *select-background*
		 :undo t
		 :maxundo 100
		 :wrap "none")
  (bind (textbox widget) "<Control-f>"
	    (lambda (evt)
	      (search-next-text (textbox widget) 
			       (input-box "Find next occurance:"
					  :default nil))
	      ;(tag-configure (textbox text1) "SEL" "1.0" "END")
	      ;(see (textbox widget) "SEL")
	      (finish-output))))

;;; This is the function that contains all processes
(defun main-function ()
  (with-ltk ()
    (let* ((frame (make-instance 'frame))
	   (text1 (make-instance 'scrolled-text)) ;Text field left
	   (text2 (make-instance 'scrolled-text)) ;Text field right
	   (m (make-menubar)) ; Main menu bar
	   (mfile (make-menu m "File"))
           (medit (make-menu m "Edit"))
	   (mleft (make-menu medit "Left List"))   ; Menu for left field
	   (mright (make-menu medit "Right List")) ; Menu for right field
	   (mboth (make-menu medit "Compare")) ; Menu for right field
	   (mhelp (make-menu m "Help")))
      (make-menubutton mfile "Load Left List" 
		       (lambda () (read-file-to-list text1)))
      (make-menubutton mfile "Load Right List"
		       (lambda () (read-file-to-list text2)))
      (add-separator mfile) 
      (make-menubutton mfile "Save Left List"
		       (lambda () (write-list-to-file text1)))
      (make-menubutton mfile "Save Right List"
		       (lambda () (write-list-to-file text2)))
      (add-separator mfile)
      (make-menubutton mfile "Quit" (lambda () (setf *exit-mainloop* t)))
      (attach-list-menus mleft text1)
      (attach-list-menus mright text2)
      (make-menubutton mboth "Intersection of Sets"
		       (lambda () (find-intersection text1 text2)))
      (make-menubutton mboth "Union of Sets"
		       (lambda () (find-union text1 text2)))
      (make-menubutton mboth "Exclusion of Sets"
		       (lambda () (find-exclusive text1 text2)))
      
      (make-menubutton mhelp "Text Editing Keys" (lambda () (display-key-help)))
      (make-menubutton mhelp "About" (lambda () (display-info)))
      (set-geometry *tk* *window-width* *window-height* 0 0)
      (wm-title *tk* "List Monger")
      (pack frame :side :bottom)      
      (pack text1 :expand 1 :fill :both :side :left)
      (pack text2 :expand 1 :fill :both :side :right)
      (configure medit :tearoff t)

      (style-textbox text1)
      (style-textbox text2)
      (configure frame :relief :sunken))))

;;;Start program
(main-function)
