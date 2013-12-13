(in-package #:sf)

(defstruct sndfile :ptr :path :info)
(defstruct sf-info :frames :sr :chanls :type :format :sections :seekable)

(defun frames (sndfile)
  (sf-info-frames (sndfile-info sndfile)))

(defun sr (sndfile)
  (sf-info-sr (sndfile-info sndfile)))

(defun chanls (sndfile)
  (sf-info-chanls (sndfile-info sndfile)))

(defun sections (sndfile)
  (sf-info-sections (sndfile-info sndfile)))

(defun seekable (sndfile)
  (sf-info-seekable (sndfile-info sndfile)))

(defun sf-type (sndfile)
  (sf-info-type (sndfile-info sndfile)))

(defun sf-format (sndfile)
  (sf-info-format (sndfile-info sndfile)))



(defun make-sndfile-info (sf-info)
  (cffi:with-foreign-slots
      ((frames sample-rate channels format sections seekable) sf-info (:struct sf_info))
    (make-sf-info :frames frames
		  :sr sample-rate
		  :chanls channels
		  :type (cffi:foreign-enum-keyword
			 'sf-file-format
			 (logand format
				 (cffi:foreign-enum-value
				  'sf-file-format :typemask)))
		  :format (cffi:foreign-enum-keyword
			   'sf-file-format
			   (logand format
				   (cffi:foreign-enum-value
				    'sf-file-format :submask)))
		  :sections sections
		  :seekable (not (zerop seekable)))))


(defmacro sndfile-err-check (sndfile-ptr)
  `(assert (zerop (sf-error ,sndfile-ptr)) nil
	   (format nil "~a in libsndfile" (sf-strerror ,sndfile-ptr))))

(defmacro with-io-context ((type direction) &body body)
  `(let* ((fn (ecase ,type ((:floatf :doublef :intf :shortf) 'frames)
		     ((:float :double :int :short) 'items)))
	  (cffi-type (ecase ,type ((:float :floatf) :float)
			    ((:double :doublef) :double)
			    ((:int :intf) :int)
			    ((:short :shortf) :short)))
	  (name (intern (su:cat ,direction "-" (string-upcase fn) "-" (string-upcase cffi-type)))))
     ,@body))

(defmacro def-read-data-function (type)
  (with-io-context (type "READ")
    (alexandria:with-gensyms (vect size chanls data)
      `(progn
	 (defun ,name (sndfile &optional ,fn)
	   (let ((,chanls (chanls sndfile)))
	     (unless ,fn (setf ,fn ,(ecase fn
				      (frames `(frames sndfile))
				      (items `(* (frames sndfile) ,chanls)))))
	     (let* ((,size ,(ecase fn
			      (frames `(* ,fn ,chanls))
			      (items fn))))
	       (cffi:with-foreign-objects ((,data ,cffi-type ,size))
		 (setf ,fn (,(intern (string-upcase (su:cat "sf-read" (if (eql fn 'frames) "f-" "-") (string cffi-type))))
			    (sndfile-ptr sndfile) ,data ,fn))
		 (sndfile-err-check (sndfile-ptr sndfile))
		 (let ((,vect (make-array ,size :element-type ',(ecase type
								  ((:short :shortf :int :intf) 'fixnum)
								  ((:float :floatf) 'single-float)
								  ((:double :doublef) 'double-float)))))
		   (loop for i from 0 below ,size
			 do (setf (aref ,vect i) (cffi:mem-aref ,data ,cffi-type i)))
		   (values ,vect ,fn))))))
	 (export ',name :sf)))))

(def-read-data-function :short)
(def-read-data-function :shortf)
(def-read-data-function :int)
(def-read-data-function :intf)
(def-read-data-function :float)
(def-read-data-function :floatf)
(def-read-data-function :double)
(def-read-data-function :doublef)

(defmacro def-write-data-function (type)
  (with-io-context (type "WRITE")
    (alexandria:with-gensyms (snd-frames data)
      `(progn
	 (defun ,name (buffer sndfile)
	   (let* ((,snd-frames (length buffer)))
	     ;;; write operate to list, too much slow than array.(x150)
	     (unless (typep buffer 'vector)
	       (setf buffer (map '(vector ,(ecase type
	     				     ((:short :shortf :int :intf) 'fixnum)
	     				     ((:float :floatf) 'single-float)
	     				     ((:double :doublef) 'double-float))) #'identity buffer)))
	     (cffi:with-foreign-objects ((,data ,cffi-type ,snd-frames))
	       (loop for i from 0 below ,snd-frames
		     do (setf (cffi:mem-aref ,data ,cffi-type i) (aref buffer i)))
	       (,(intern (string-upcase (su:cat "sf-write" (if (eql fn 'frames) "f-" "-") (string cffi-type))))
		(sndfile-ptr sndfile) ,data ,(if (eql fn 'frames) `(/ ,snd-frames (chanls sndfile)) snd-frames))
	       (sndfile-err-check (sndfile-ptr sndfile))
	       (values))))
	 (export ',name :sf)))))


(def-write-data-function :short)
(def-write-data-function :shortf)
(def-write-data-function :int)
(def-write-data-function :intf)
(def-write-data-function :float)
(def-write-data-function :floatf)
(def-write-data-function :double)
(def-write-data-function :doublef)


(defmacro with-open-sndfile ((sndfile path &key (direction :input) 
					     (chanls 1)
					     (sr 44100)
					     (format :pcm_24)) &body body)
  (let ((mode (ecase direction
		(:input :sfm_read)
		(:output :sfm_write))))
    (alexandria:with-gensyms (sf-info full-path)
      `(let ((,full-path (su:full-pathname ,path)))
	 (cffi:with-foreign-objects ((,sf-info '(:struct sf_info)))
	   (cffi:foreign-funcall "memset"
				 :pointer ,sf-info
				 :int 0
				 :int (cffi:foreign-type-size '(:struct sf_info)))
	   (when (eql ,mode :sfm_write)
	     (cffi:with-foreign-slots ((sample-rate channels format) ,sf-info (:struct sf_info))
	       (setf sample-rate ,sr
		     channels ,chanls
		     format (+ (cffi:foreign-enum-value
				'sf-file-format
				(intern (string-upcase (pathname-type ,full-path)) :keyword))
			       (cffi:foreign-enum-value 'sf-file-format ,format)
			       (cffi:foreign-enum-value 'sf-file-format :file)))))
	   (let* ((,sndfile (make-sndfile
			     :ptr (sf-open ,full-path
					   (cffi:foreign-enum-value 'sf-file-mode ,mode)
					   ,sf-info)
			     :path ,full-path)))
	     (sndfile-err-check (sndfile-ptr ,sndfile))
	     (setf (sndfile-info ,sndfile) (make-sndfile-info ,sf-info))
	     (unwind-protect (progn ,@body)
	       (sf-close (sndfile-ptr ,sndfile)))))))))


