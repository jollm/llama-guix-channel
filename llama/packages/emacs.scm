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
   (version "0.22.4")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://git.sr.ht/~joj/exlybar")
                  (commit "v0.22.4")))
            (sha256
             (base32
              "0djy7404zllbyl3fmqzqji046imxsjby39g31vs9223n5bf0882s"))))
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
    (version "20230616.2053")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/s-kostyaev/go-gen-test.git")
                    (commit "af00a9abbaba2068502327ecdef574fd894a884b")))
              (sha256
               (base32
                "0q81zkyrl1njwxq29rx7gq9m9w3jags6akxzl7jd9yrnl4k2l27p"))))
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

(define-public emacs-mermaid-mode
  (package
    (name "emacs-mermaid-mode")
    (version "20240123.1729")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/abrochard/mermaid-mode.git")
                    (commit "d8bfb8c819cda9ead19c871842f6b0b8d56c56c0")))
              (sha256 (base32
                       "0vsnyn2g8525k9vgmifzix9gd1g3149h54ni6rrc1y05h91883fv"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/abrochard/mermaid-mode")
    (synopsis "major mode for working with mermaid graphs")
    (description
     "Major mode for working with mermaid graphs.  See https://mermaid-js.github.io/")
    (license #f)))

(define-public emacs-elfeed-tube
  (package
    (name "emacs-elfeed-tube")
    (version "20240123.1825")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/karthink/elfeed-tube.git")
                    (commit "92c66d6adcebe0588ccf811616decf7ef8a8ac65")))
              (sha256
               (base32
                "1mz2mk9vzdpj0x6dv1grkz03algrsf62kfqrlhnww60vnzqi6i3r"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-aio emacs-elfeed emacs-mpv))
    (arguments '(#:include '("^elfeed-tube.el$" "^elfeed-tube-utils.el$"
                             "^elfeed-tube-fill.el$" "^elfeed-tube-mpv.el$")
                 #:exclude '()))
    (home-page "https://github.com/karthink/elfeed-tube")
    (synopsis "YouTube integration for Elfeed")
    (description
     "Elfeed Tube is an extension for Elfeed, the feed reader for Emacs, that enhances
your Youtube RSS feed subscriptions.  Typically Youtube RSS feeds contain only
the title and author of each video.  Elfeed Tube adds video descriptions,
thumbnails, durations, chapters and \"live\" transcrips to video entries.  See
https://github.com/karthink/elfeed-tube for demos.  This information can
optionally be added to your entry in your Elfeed database.  The displayed
transcripts and chapter headings are time-aware, so you can click on any
transcript segment to visit the video at that time (in a browser or your video
player if you also have youtube-dl).  A companion package, `elfeed-tube-mpv',
provides complete mpv (video player) integration with the transcript, including
video seeking through the transcript and following along with the video in
Emacs.  To use this package, (i) Subscribe to Youtube channel or playlist feeds
in Elfeed.  You can use the helper function `elfeed-tube-add-feeds provided by
this package to search for Youtube channels by URLs or search queries. (ii)
Place in your init file the following: (require elfeed-tube) (elfeed-tube-setup)
(iii) Use Elfeed as normal, typically with `elfeed'.  Your Youtube feed entries
should be fully populated.  You can also call `elfeed-tube-fetch in an Elfeed
buffer to manually populate an entry, or obtain an Elfeed entry-like summary for
ANY youtube video (no subscription needed) by manually calling
`elfeed-tube-fetch from outside Elfeed.  User options: There are three options
of note: `elfeed-tube-fields': Customize this to set the kinds of metadata you
want added to Elfeed's Youtube entries.  You can selectively turn on/off
thumbnails, transcripts etc. `elfeed-tube-auto-save-p': Set this boolean to save
fetched Youtube metadata to your Elfeed database, i.e.  to persist the data on
disk for all entries. `elfeed-tube-auto-fetch-p': Unset this boolean to turn off
fetching metadata.  You can then call `elfeed-tube-fetch to manually fetch data
for specific feed entries.  See the customization group `elfeed-tube for more
options.  See the README for more information.")
    (license license:unlicense)))

(define-public emacs-reverso
  (package
    (name "emacs-reverso")
    (version "20240113.2128")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SqrtMinusOne/reverso.el.git")
             (commit "d1b39da3c7df1541f98435f3172a7ff4f3123634")))
       (sha256
        (base32 "1fpk5wyzlssfrm4jbsrflxvlfn80yh6y1nh63ml8barf1nypsx55"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-transient emacs-request))
    (home-page "https://github.com/SqrtMinusOne/reverso.el")
    (synopsis "Translation, grammar checking, context search")
    (description
     "Emacs client for the https://reverso.net service.  The service doesn't offer an
official API, so this package accesses it with whatever means possible.  The
implemented features are as follows: - Translation (run `reverso-translate') -
\"Context\" or bilingual concordances (run `reverso-context') - Grammar check (run
`reverso-grammar') - Synonyms search (run `reverso-synonyms') - Verb conjugation
(run `reverso-conjugation') There's also `reverso-grammar-buffer', which does
grammar check in the current buffer and displays the result with overlays.  The
`reverso command is the entrypoint to all the functionality.  The Elisp API of
the listed features is as follows: - `reverso--translate - `reverso--get-context
- `reverso--get-grammar - `reverso--get-context - `reverso--get-synonyms -
`reverso--get-conjugation Also check out the README file at
<https://github.com/@code{SqrtMinusOne/reverso.el>}")
    (license #f)))
