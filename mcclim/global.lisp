(in-package :mcclim-desktop)

;;;;
;;;; Global variables
;;;;

(defparameter *mcclim-directory*
  (asdf:component-pathname (asdf:find-system "mcclim")))

#|
(setf mcclim-truetype:*truetype-font-path*
      (find-if #'probe-file
	       '(#p"/usr/share/fonts/truetype/ttf-dejavu/"
		 #p"/usr/share/fonts/TTF/")))

|#
