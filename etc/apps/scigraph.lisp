(in-package :cl-desktop)

(asdf:require-system "scigraph")

(register-application "scigraph" 'standard-mcclim-application
		      :system-name "graph"
		      :debug-p t
		      :entry-fn #'(lambda (&rest args)
				    (graph:make-demo-frame)))


