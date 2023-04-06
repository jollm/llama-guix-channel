(define-module (llama packages security-token)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages man)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config))

(define-public yubico-piv-tool
  (package
    (name "yubico-piv-tool")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/yubico-piv-tool/Releases/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "0knnnmh6b7bxyf0xv4mx9ay2kbr9vffkyxa6adism1b1igyxm2fs"))))
    (build-system cmake-build-system)
    (inputs
     (list gengetopt perl pcsc-lite openssl))
    (native-inputs
     (list doxygen
           graphviz
           help2man
           check
           texlive-bin
           pkg-config))
    (home-page "https://developers.yubico.com/yubico-piv-tool/")
    (synopsis "Interact with the PIV application on a YubiKey")
    (description
     "The Yubico PIV tool is used for interacting with the Privilege and
Identification Card (PIV) application on a YubiKey.  With it you may generate
keys on the device, import keys and certificates, create certificate requests,
and other operations.  It includes a library and a command-line tool.")
    ;; The file ykcs11/pkcs11.h also declares an additional, very short free
    ;; license for that one file.  Please see it for details.  The vast
    ;; majority of files are licensed under bsd-2.
    (license license:bsd-2)))
