
(in-package #:sf)

(cffi:define-foreign-library libsndfile
  (:darwin "libsndfile.1.dylib")
  (:unix (:or "libsndfile.so"))
  (:windows "libsndfile.dll"))

(cffi:use-foreign-library libsndfile)

(cffi:defctype sf_count_t :int64)

(cffi:defcstruct sf_info
  (frames sf_count_t)
  (sample-rate :int)
  (channels :int)
  (format :int)
  (sections :int)
  (seekable :int))

(cffi:defcenum sf-file-mode
  (:sf_false 0)
  (:sf_true 1)
  (:sfm_read #x10)
  (:sfm_write #x20)
  (:sfm_rdwr #x30)
  (:sf_ambisonic_none #x40)
  (:sf_ambisonic_b_foramt #x41))

(cffi:defcenum sf-file-format
  (:WAV          #x010000)
  (:AIFF         #x020000)
  (:AU           #x030000)
  (:RAW          #x040000)
  (:PAF          #x050000)
  (:SVX          #x060000)
  (:NIST         #x070000)
  (:VOC          #x080000)
  (:IRCAM        #x0A0000)
  (:W64          #x0B0000)
  (:MAT4         #x0C0000)
  (:MAT5         #x0D0000)
  (:PVF          #x0E0000)
  (:XI           #x0F0000)
  (:HTK          #x100000)
  (:SDS          #x110000)
  (:AVR          #x120000)
  (:WAVEX        #x130000)
  (:SD2          #x160000)
  (:FLAC         #x170000)
  (:CAF          #x180000)
  (:WVE          #x190000)
  (:OGG          #x200000)
  (:MPC2K        #x210000)
  (:RF64         #x220000)

  (:PCM_S8       #x0001)
  (:PCM_16       #x0002)
  (:PCM_24       #x0003)
  (:PCM_32       #x0004)
  (:PCM_U8       #x0005)
  (:FLOAT        #x0006)
  (:DOUBLE       #x0007)
  (:ULAW         #x0010)
  (:ALAW         #x0011)
  (:IMA_ADPCM    #x0012)
  (:MS_ADPCM     #x0013)
  (:GSM610       #x0020)
  (:VOX_ADPCM    #x0021)
  (:G721_32      #x0030)
  (:G723_24      #x0031)
  (:G723_40      #x0032)
  (:DWVW_12      #x0040)
  (:DWVW_16      #x0041)
  (:DWVW_24      #x0042)
  (:DWVW_N       #x0043)
  (:DPCM_8       #x0050)
  (:DPCM_16      #x0051)
  (:VORBIS       #x0060)
  
  (:FILE          #x00000000)
  (:LITTLE        #x10000000)
  (:BIG           #x20000000)
  (:CPU           #x30000000)
  
  (:SUBMASK       #x0000FFFF)
  (:TYPEMASK      #x0FFF0000)
  (:ENDMASK       #x30000000))

(cffi:defcfun "sf_open" :pointer
  (path :string)
  (mode :int)
  (sf_info :pointer))

(cffi:defcfun "sf_close" :int
  (sndfile :pointer))


;;; File Read/Write Functions(Items/Frames)
(defmacro def-sf-rw-function (type)
  (let* ((type (string-downcase type)))
    (cons 'progn
	  (loop for operate in '("readf" "writef" "read" "write")
		collect
		`(cffi:defcfun ,(concatenate 'string "sf_" operate "_" type) sf_count_t
		   (sndfile :pointer)
		   (,(intern (string-upcase (concatenate 'string type "-array"))) :pointer)
		   (,(if (eql #\f (elt (reverse operate) 0)) 'frames 'items) sf_count_t))))))

(def-sf-rw-function :short)
(def-sf-rw-function :int)
(def-sf-rw-function :float)
(def-sf-rw-function :double)

(cffi:defcfun "sf_error" :int
  (sndfile :pointer))

(cffi:defcfun "sf_strerror" :string
  (sndfile :pointer))
