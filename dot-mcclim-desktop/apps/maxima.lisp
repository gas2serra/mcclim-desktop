(in-package :desktop-user)

(register-application "maxima" 'standard-mcclim-application
		      :pretty-name "Maxima"
		      :icon nil
		      :home-page "https://github.com/lokedhs/maxima-client"
		      :git-repo "https://github.com/lokedhs/maxima-client.git"
		      :system-name "maxima-client")
