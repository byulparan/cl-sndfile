
(asdf/defsystem:defsystem #:cl-sndfile
  :name "cl-sndfile"
  :author "Park Sungmin. byulparan@icloud.com"
  :description "just wrapper libsndfile for CommonLisp"
  :version "2017.3.14"
  :depends-on (#:alexandria #:cffi)
  :serial t
  :components ((:file "package")
	       (:file "cffi")
	       (:file "sndfile")))
