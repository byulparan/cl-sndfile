
(asdf/defsystem:defsystem #:cl-sndfile
  :name "cl-sndfile"
  :author "Park Sungmin. byulparan@icloud.com"
  :description "just wrapper libsndfile for CommonLisp"
  :version "0.1"
  :depends-on (#:simple-utils #:alexandria #:cffi)
  :serial t
  :components ((:file "package")
	       (:file "cffi")
	       (:file "sndfile")))
