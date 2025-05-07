(define-module (llama packages misc)
  #:use-module (gnu packages base)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public english-words
  (let ((commit "20f5cc9b3f0ccc8ce45d814c532b7c2031bba31c")
        (revision "1"))
    (package
      (name "english-words")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dwyl/english-words")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0a2wz1n55bn8bsc7shp2cdm5ak29q0sx38yg6dxvqf5g4vv51w1x"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder (begin
                     (use-modules (guix build utils))
                     (let* ((source (assoc-ref %build-inputs "source"))
                            (out (assoc-ref %outputs "out"))
                            (share (string-append out "/share/english-words")))
                       (copy-recursively source share)
                       #t))))
      (home-page "https://github.com/dwyl/english-words")
      (synopsis "Plain lists of english words")
      (description "A text file containing over 466k English words.")
      (license license:unlicense))))
