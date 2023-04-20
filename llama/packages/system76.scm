(define-module (llama packages system76)
  #:use-module (gnu packages)
  #:use-module (guix build-system linux-module)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux))

(define-public system76-io-dkms
  (package
    (name "system76-io-dkms")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/pop-os/system76-io-dkms")
                (commit "0217576dd5a14d68c8c9e7253f198ffbf735e283")))
              (sha256
               (base32
                "1w1hc4wblzr4ww78gsmm1nlk1v4hfhmmyy2spz2lr3i8d9126r8d"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:tests? #f                  ; no `check' target
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-initramfs-tools
                 (lambda _
                   (copy-recursively "usr/share" (string-append #$output "/share")))))))
    (home-page "https://github.com/pop-os/system76-io-dkms")
    (synopsis "DKMS module for controlling System76 Io board")
    (description "DKMS module for controlling System76 Io board")
    (license license:gpl3)))

(define-public system76-io-dkms-non-free
  (package
    (inherit system76-io-dkms)
    (name "system76-io-dkms-non-free")
    (arguments
     (append (package-arguments system76-io-dkms)
             (list #:linux linux)))))
