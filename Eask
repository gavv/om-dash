(package "om-dash"
         "0.6"
         "Building blocks for org-based dashboards")

(website-url "https://github.com/gavv/om-dash")
(keywords "org" "outlines")
(license "GPLv3")

(package-file "om-dash.el")

(source "gnu")
(source "melpa")

(depends-on "emacs" "28.2")
(depends-on "org-ql")
(depends-on "s")
(depends-on "ts")

;; These dependencies are optional and we don't list them in Package-Requires.
;; However, we want to enable them when byte-compiling on CI, so we use
;; "eask install-deps --dev" for that.
(development
 (depends-on "parse-csv"
             :repo "mrc/el-csv"
             :fetcher 'github))

;; Allow setting checkdoc flags via local variables during eask invocation.
(put 'checkdoc--argument-missing-flag
     'safe-local-variable
     #'booleanp)
