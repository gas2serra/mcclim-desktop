(in-package :cl-desktop)

(asdf:require-system "scigraph")

(register-application "scigraph" 'standard-mcclim-application
		      :system-name "scigraph"
		      :debug-p t
		      :entry-fn #'(lambda (&rest args)
				    (graph:make-demo-frame)))


