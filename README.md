# cl-sndfile
just wrapper libsndfile for CommonLisp

#### require:
  - [libsndfile](http://www.mega-nerd.com/libsndfile/)
  - [Quicklisp](http://www.quicklisp.org)
  - [Simple-Utility](https://github.com/byulparan/Simple-Utility)
  - Common Lisp Implementations
	+ [ClozureCL](http://www.clozure.com/clozurecl.html)
	+ [SBCL](http://www.sbcl.org)
	+ [ECL](http://ecls.sourceforge.net)

#### usage:

	;;; write audio-file(4secs, 48000hz, stereo, PCM_24)
	(sf:with-open-sndfile (snd "~/Desktop/sinewave.wav"
			   :direction :output
			   :chanls 2
			   :sr 48000)
	   (sf:write-frames-double
          (loop for i from 0 below (* 48000 4)
	          append (su:dup (sin (* 2 pi (/ i (/ 48000 440)))) 2))
		  snd))


	;;; read-frames-* function is get a buffer(size as frames * channels)
	(sf:with-open-sndfile (snd "~/Desktop/sinewave.wav")
		(sf:read-frames-float snd 10))
	
