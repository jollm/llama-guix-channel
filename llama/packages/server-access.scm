(define-module (llama packages server-access)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages package-management)
  #:use-module (llama packages patool)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build copy-build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public scaleft-client-tools
  (package
    (name "scaleft-client-tools")
    (version "1.67.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkg.scaleft.com/rpm/"
				  name "-" version "-1.x86_64.rpm"))
              (sha256
               (base32
                "0fy8dna58bv3gywx2hqmhbczbf0w2xzz0zyq2s59lzqa6f2r444i"))))
    (build-system copy-build-system)
    (inputs (list glibc))
    (native-inputs (list cpio rpm patchelf patool which))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
		  (lambda* (#:key source #:allow-other-keys)
		    (invoke "patool" "extract" source)))
	 (replace 'patch-shebangs
		  (lambda* (#:key outputs inputs #:allow-other-keys)
		    (let ((out (assoc-ref %outputs "out"))
			  (ld (string-append (assoc-ref inputs "glibc") "/lib/ld-linux-x86-64.so.2")))
		      (invoke
		       "patchelf"
		       "--set-interpreter"
		       ld
		       (string-append out "/bin/sft"))))))
       #:install-plan
       '(("usr/bin" "bin"))))
    (synopsis "ScaleFT(TM) client tools")
    (description "ScaleFT(TM) client tools")
    (home-page "https://scaleft.com")
    (license #f)))
