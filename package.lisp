
(defpackage #:sf
  (:use #:cl)
  (:export #:with-open-sndfile
	   #:frames
	   #:sr
	   #:chanls
	   #:sections
	   #:seekable
	   #:sf-type
	   #:sf-format))
