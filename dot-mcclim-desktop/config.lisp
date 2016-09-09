(in-package :mcclim-desktop-user)

(setf (manager-debugger-fn *manager*) *clim-debugger*)
(setf *application-style* :my)
(discover-applications *manager*)
