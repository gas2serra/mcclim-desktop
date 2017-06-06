(in-package :desktop-user)

(register-application "mastodon" 'standard-mcclim-application
		      :pretty-name "Mastodon"
		      :icon nil
		      :home-page "https://github.com/lokedhs/mastodon"
		      :git-repo "https://github.com/lokedhs/mastodon.git"
		      :system-name "mastodon")
