(in-package :cl-desktop)

(ql:quickload "clim-pkg-doc")

(register-application "pkg-doc" 'standard-mcclim-application
		      :system-name "clim-pkg-doc"
		      :entry-fn #'(lambda (&rest args)
				    (clim-pkg-doc:pkg-doc)))


