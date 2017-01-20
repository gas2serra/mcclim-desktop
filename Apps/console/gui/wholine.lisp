;;; Listener "wholine"

;;; (C) Copyright 2003 by Andy Hefner (hefner1@umbc.edu)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :desktop-console)

;;; Presentation types
(clim:define-presentation-type bytes () :inherit-from 'integer)

(clim:define-presentation-method clim:present (object (type bytes)
						      stream (view clim:textual-view)
						      &key &allow-other-keys)
  (if (zerop object)
      (princ "0" stream)
      (let* ((suffixes '(" bytes" " KB" " MB" " GB" " TB" " PB"))
	     (x (floor (realpart (log object 1000))))
	     (idx (min x (1- (length suffixes)))))
	(if (zerop idx)
	    (format stream "~A bytes" object)
	    (format stream "~,1F~A" (/ object (expt 1000 idx)) (nth idx suffixes))))))


;; Wholine Pane

(defclass wholine-pane (clim:application-pane) ()
  (:default-initargs :background clim:+gray90+))

(defmethod clim:compose-space ((pane wholine-pane) &key width height)
  (declare (ignore width height))  
  (let ((h (* 1.5 (clim:text-style-height (clim:medium-text-style pane) pane)))) ; magic padding
    (clim:make-space-requirement :height h
				 :min-height h
				 :max-height h)))

(defmethod clim:allocate-space :after ((pane wholine-pane) width height)
  (declare (ignore width height)))

(defun print-package-name (stream)
  (let ((foo (package-name *package*)))
    (clim:with-drawing-options (stream :ink clim:+royalblue+)
      (format stream "~A" (reduce (lambda (&optional (a foo) (b foo))
                                    (if (< (length a) (length b)) a b))
                                  (package-nicknames *package*))))))

(defun frob-pathname (pathname)
  (namestring (truename pathname)))

(defun generate-wholine-contents (frame pane)
  (declare (ignore frame))
  (let* ((*standard-output* pane)
         (username (or (osicat:environment-variable "USER")
                       "luser"))        ; sorry..
         (sitename (machine-instance))
         ;; :sans-serif :roman :small is the best looking jaggy font.
         ;; But :small looks awful using freetype, perhaps because the
         ;; fonts are, for whatever reason, slightly smaller.
         ;; Very distressing.
         (text-size (if (find-package :mcclim-truetype) :normal :small))
         (memusage #+(or cmu scl) (lisp::dynamic-usage)
                   #+sbcl  (sb-kernel:dynamic-usage)
                   #+lispworks (getf (system:room-values) :total-allocated)
		   #+openmcl (+ (ccl::%usedbytes) (ccl::%freebytes))
                   #+clisp (values (sys::%room))
                   #-(or cmu scl sbcl lispworks openmcl clisp) 0))
    (clim:with-text-style (t (clim:make-text-style :sans-serif :roman text-size))
      (clim:formatting-table (t :x-spacing '(2 :character))
        (clim:formatting-row (t)                        
          (macrolet ((cell ((align-x) &body body)                         
                       `(clim:formatting-cell (t :align-x ,align-x) ,@body)))
            (cell (:left)   (format t "~A@~A" username sitename))
            (cell (:center)
              (format t "Package ")
              (clim:with-output-as-presentation (t *package* 'package)
                (print-package-name t)))
	    (cell (:center)
		  (when (numberp memusage)
		    (clim:present memusage 'bytes)))
	    (cell (:center)
		  (format t "~A threads" (length (bt:all-threads))))
            (cell (:center)
                  ;; CLISP gives us an error when calling
                  ;; `cl:probe-file' with a directory argument.
              (when #+clisp (or (ignore-errors (ext:probe-directory *default-pathname-defaults*))
                                (ignore-errors (probe-file *default-pathname-defaults*)))
                    #-clisp (probe-file *default-pathname-defaults*)
                (clim:with-output-as-presentation (t (truename *default-pathname-defaults*) 'pathname)
                  (format t "~A" (frob-pathname *default-pathname-defaults*)))))
          ;; Although the CLIM spec says the item formatter should try to fill
          ;; the available width, I can't get either the item or table formatters
          ;; to really do so such that the memory usage appears right justified.
	    ))))))

(defun invoke-and-center-output (stream-pane continuation
                                 &key (horizontally t) (vertically t) (hpad 0) (vpad 0))
  (let ((record (clim:with-output-to-output-record (stream-pane)
                  (funcall continuation))))
    (clim:with-bounding-rectangle* (sx0 sy0 sx1 sy1) (clim:sheet-region stream-pane)
      (clim:with-bounding-rectangle* (rx0 ry0 rx1 ry1) (clim:bounding-rectangle record)
        (setf (clim:output-record-position record)
              (values (if horizontally
                          (+ rx0 (/ (- (- sx1 sx0)
                                       (- rx1 rx0))
                                    2))
                          (+ rx0 hpad))
                      (if vertically
                          (+ ry0 (/ (- (- sy1 sy0)
                                       (- ry1 ry0))
                                    2))
                          (+ ry0 vpad))))))	
    (clim:add-output-record record (clim:stream-output-history stream-pane))))

(defun display-wholine (frame pane)
  (invoke-and-center-output pane
    (lambda () (generate-wholine-contents frame pane))
    :horizontally nil :hpad 5)
  (clim:repaint-sheet pane (clim:sheet-region pane)))
