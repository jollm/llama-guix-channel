(define-module (llama packages emacs)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages file)
  #:use-module (gnu packages package-management)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public emacs-pcache
  (package
    (name "emacs-pcache")
    (version "20220724.1841")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sigma/pcache.git")
                    (commit "507230d094cc4a5025fe09b62569ad60c71c4226")))
              (sha256
               (base32
		"1fjdn4g9ww70f3x6vbzi3gqs9dsmqg16isajlqlflzw2716zf2nh"))))
    (build-system emacs-build-system)
    (home-page "unspecified")
    (synopsis "persistent caching for Emacs.")
    (description
     "pcache provides a persistent way of caching data, in a hashtable-like structure.
 It relies on `eieio-persistent in the backend, so that any object that can be
serialized by EIEIO can be stored with pcache.  pcache handles objects called
\"repositories\" (`pcache-repository') and \"entries\" (`pcache-entry').  Each
repository is identified by a unique name, that defines an entry in
`pcache-directory'.  Subdirectories are allowed, by the use of a directory
separator in the repository name.  Example: (let ((repo (pcache-repository
\"plop\"))) (pcache-put repo foo 42) ; store value 42 with key foo (pcache-get
repo foo) ; => 42 ) Keys can be pretty much any Lisp object, and are compared
for equality using `eql Optionally, cache entries can expire: (let ((repo
(pcache-repository \"plop\"))) (pcache-put repo foo 42 1) ; store value 42 with
key foo for 1 second (sleep-for 1) (pcache-get repo foo) ; => nil )")
    (license #f)))

(define-public emacs-logito
  (package
    (name "emacs-logito")
    (version "20201226.534")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sigma/logito.git")
                    (commit "d5934ce10ba3a70d3fcfb94d742ce3b9136ce124")))
              (sha256
               (base32
		"0bnkc6smvaq37q08q1wbrxw9mlcfbrax304fxw4fx7pc1587av0d"))))
    (build-system emacs-build-system)
    (home-page "unspecified")
    (synopsis "logging library for Emacs")
    (description "This module provides logging facility for Emacs")
    (license #f)))

(define-public emacs-fontsloth
  (package
   (name "emacs-fontsloth")
   (version "20230516.1901")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/jollm/fontsloth.git")
                  (commit "8dd771aae34ce282036c7533735e6251770fcbd0")))
            (sha256
             (base32
              "16yk55nvpn8s4cv9wlfm2zp9wvaianal7c5pkk2jxildw7ras55x"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-f emacs-logito emacs-pcache emacs-stream))
   (home-page "https://github.com/jollm/fontsloth")
   (synopsis "Elisp otf/ttf font loader/renderer")
   (description
    "fontsloth: the slowest font renderer in the world written in pure elisp inspired
by fontdue, the fastest font renderer in the world, written in pure rust *Please
see the website for a detailed README.* To use this module, load and enable it
as follows: (use-package fontsloth) If you also want layout functions (includes
fontsloth): (use-package fontsloth-layout)")
   (license license:gpl3+)))

(define-public emacs-backlight
  (package
    (name "emacs-backlight")
    (version "20210513.129")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mschuldt/backlight.el.git")
                    (commit "b6826a60440d8bf440618e3cdafb40158de920e6")))
              (sha256
               (base32
		"0nj5l0wwza1j908n9k0896b972b84s401szkgb0acf4fs834vc0w"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/mschuldt/backlight.el")
    (synopsis "backlight brightness adjustment on GNU/Linux")
    (description
     "This package provides a simple utility for setting backlight brightness on some
GNU/Linux systems using sysfs files.  This works like most system provided
backlight brightness controls but allows for increased resolution when the
brightness percentage nears zero.  On some systems a udev rule must be added, in
/etc/udev/rules.d/backlight.rules add: ACTION==\"add\", SUBSYSTEM==\"backlight\",
KERNEL==\"acpi_video0\", GROUP=\"video\", MODE=\"then\" then reload with: sudo udevadm
control --reload-rules && udevadm trigger USAGE M-x backlight Then use < or > to
adjust backlight brightness, C-g when done.  M-x backlight-set-raw prompts for a
value to write directly to the device file.")
    (license #f)))

(define-public emacs-volume
  (package
    (name "emacs-volume")
    (version "20220904.1727")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dbrock/volume.el.git")
                    (commit "050d3e6d2543a6771a13f95612055864679b6301")))
              (sha256
               (base32
		"1vyl13swx82njqlfzmaj9c4vbdpdsj4m9f8v32a9kycdhbm9x90z"))))
    (build-system emacs-build-system)
    (home-page "http://www.brockman.se/software/volume-el/")
    (synopsis "tweak your sound card volume from Emacs")
    (description
     "To use this program, put this file in your `load-path', and put the following
autoload in your ~/.emacs: (autoload volume \"volume\" \"Tweak your sound card
volume.\" t) Then type `M-x volume <RET> to run the program.  Of course, use `M-x
customize-group <RET> volume <RET> to customize it.  Tweaking the volume of my
music used to be one of the few things I constantly went outside of Emacs to do.
 I just decided I've had enough of that, and so I wrote this simple mixer
frontend.  It comes with backend glue for aumix and amixer, but the latter is
pretty slow, so I have to recommend the former.  If you can't use either,
writing your own glue should be straightforward.  And if you do, please consider
sending the code to me, so I can integrate it into this file.")
    (license #f)))

(define-public emacs-exlybar
  (package
   (name "emacs-exlybar")
   (version "0.22.3")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://git.sr.ht/~joj/exlybar")
                  (commit "v0.22.3")))
            (sha256
             (base32
              "1h3qz9ql6l6in4m6xw742raj40da7cwhgv6da7aq38hckadj624x"))))
   (build-system emacs-build-system)
   (propagated-inputs (list
		       emacs-f emacs-s emacs-dash
		       emacs-xelb emacs-fontsloth emacs-log4e
		       emacs-backlight emacs-volume
		       emacs-all-the-icons))
   (arguments
    '(#:include '("^[^/]*\\.el$" "^[^/]*\\.info$" "^doc/.*\\.info$" "^modules/[^/]*\\.el$")))
   (home-page "https://github.com/jollm/exlybar")
   (synopsis "Emacs polybar-like thing")
   (description
    "This module uses xelb to build polybar like modules for displaying status information.

     *Please see the website for a detailed README.*

     To use this module, load and enable it as follows:
     (use-package exlybar
       :config (exlybar))")
   (license license:gpl3+)))

(define-public emacs-gotest
  (package
    (name "emacs-gotest")
    (version "20230221.945")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nlamirault/gotest.el.git")
                    (commit "490189e68d743a851bfb42d0017428a7550e8615")))
              (sha256
               (base32
                "19lpr9wa73415jmdl1acijz54h5sdsj95wxigigbiqdhq6pd301p"))))
    (build-system emacs-build-system)
    (arguments
     '(#:include '("^gotest.el$")
       #:exclude '()))
    (propagated-inputs (list emacs-s emacs-f emacs-go-mode))
    (home-page "https://github.com/nlamirault/gotest.el")
    (synopsis "Launch GO unit tests")
    (description "")
    (license #f)))

(define-public emacs-go-gen-test
  (package
    (name "emacs-go-gen-test")
    (version "20230127.1422")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/s-kostyaev/go-gen-test.git")
                    (commit "f84f4177af7fcbe10ce2116d5417ad5f0485034b")))
              (sha256
               (base32
                "1bxa3870vr2hyaj4isvhzfhhs56qyrp9lngapsggxhdq1r9vwqra"))))
    (build-system emacs-build-system)
    (arguments
     '(#:include '("^go-gen-test.el$")
       #:exclude '()))
    (home-page "https://github.com/s-kostyaev/go-gen-test")
    (synopsis "Generate tests for go code with gotests")
    (description
     "This package is simple wrapper for https://github.com/cweill/gotests You should
  install `gotests for use it.")
    (license #f)))

(define-public emacs-impostman
  (package
    (name "emacs-impostman")
    (version "20230111.2012")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flashcode/impostman.git")
                    (commit "936575500f733c2428ba878f9400f3eef8c9645e")))
              (sha256
               (base32
                "16zd5bk7s1h9yrrsk0ngpzb4cfyj4gkmq70m0ijsc94az7m9rlx3"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/flashcode/impostman.git")
    (synopsis "Import of Postman collections in Emacs")
    (description
     "Postman collections and environments can be imported and used with these Emacs HTTP clients:
    verb
    restclient")
    (license #f)))

(define-public emacs-ob-mermaid
  (package
    (name "emacs-ob-mermaid")
    (version "20220225.2012")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/arnm/ob-mermaid.git")
                    (commit "b4ce25699e3ebff054f523375d1cf5a17bd0dbaf")))
              (sha256
               (base32
                "0fhj3241gpj6qj2sawr8pgyn5b7320vjfb7idsy23kh4jvmj2wb8"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/arnm/ob-mermaid")
    (synopsis "Generate mermaid diagrams using org-mode, org-babel and mermaid.cli")
    (description
     "Generate mermaid diagrams using org-mode, org-babel and mermaid.cli")
    (license #f)))

(define-public emacs-elfeed-tube
  (package
    (name "emacs-elfeed-tube")
    (version "20230316.313")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/karthink/elfeed-tube.git")
                    (commit "194215ae02f4f7bfc4b693317afd6338d30459d1")))
              (sha256
               (base32
                "1arr63vwlyc7h0nqpdh4i0kava77yr0z9lcyh0d6hd923vaidc8r"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-aio emacs-elfeed emacs-mpv))
    (home-page "https://github.com/karthink/elfeed-tube")
    (synopsis "Youtube on your terms")
    (description
     "Elfeed Tube adds video descriptions, metadata and “live”
transcripts for all Youtube video entries in Elfeed.")
    (license license:unlicense)))
