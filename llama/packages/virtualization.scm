(define-module (llama packages virtualization)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages package-management)
  #:use-module (llama packages patool)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build copy-build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (guix utils))


(define-public vagrant
  (package
    (name "vagrant")
    (version "2.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://releases.hashicorp.com/vagrant/"
				  version "/vagrant-" version "-1.x86_64.rpm"))
              (sha256
               (base32
                "0zc250ivlfx0klnk55k38krmpzg212b0cyivr469aqgksybnc253"))))
    (build-system copy-build-system)
    (inputs (list openssl libssh libxml2 libxslt libarchive))
    (propagated-inputs (list curl perl ruby xz))
    (native-inputs (list glibc cpio rpm patchelf patool which))
    (arguments
     `(
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
		  (lambda* (#:key source #:allow-other-keys)
		    (invoke "patool" "extract" source)))
	 (add-after 'install 'install-openssl-lib
		    (lambda* (#:key outputs inputs #:allow-other-keys)
		      (let* ((out (assoc-ref %outputs "out"))
			     (openssl (assoc-ref inputs "openssl"))
			     (ssl-lib (string-append openssl "/lib/libssl.so.1.1"))
			     (crypto-lib (string-append openssl "/lib/libcrypto.so.1.1"))
			     (target (string-append out "/embedded/lib")))
			(install-file crypto-lib target)
			(install-file ssl-lib target))))
	 (add-after 'patch-shebangs 'patch-bin-elfs
		  (lambda* (#:key outputs inputs #:allow-other-keys)
		    (let* ((out (assoc-ref %outputs "out"))
			   (ld (string-append (assoc-ref inputs "glibc") "/lib/ld-linux-x86-64.so.2"))
			   (elves (find-files (string-append out "/embedded/bin")
					      (lambda (file stat)
						(and (not (string-suffix? "-config" (basename file)))
						     (not (string-contains (basename file) "bundle"))
						     (not (string=? (basename file) "c_rehash"))
						     (not (string=? (basename file) "erb"))
						     (not (string=? (basename file) "gem"))
						     (not (string=? (basename file) "irb"))
						     (not (string-contains (basename file) "racc"))
						     (not (string=? (basename file) "rake"))
						     (not (string=? (basename file) "rdoc"))
						     (not (string=? (basename file) "ri")))))))
		      (for-each
		       (lambda (elf)
			 (invoke "patchelf" "--set-interpreter" ld elf))
		       elves)))))
       #:install-plan
       '(("ri4j6b28ch9ndmg3a2kdafwc9qf241r7-vagrant-2.3.4-1.x86_64/opt/vagrant" "."))))
    (synopsis "Build and distribute virtualized development environments")
    (description "Vagrant is a tool for building complete development environments.")
    (home-page "https://hashicorp.com/vagrant")
    (license #f)))
