(define-module (llama packages emacs)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
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

(define-public emacs-xelb-next
  (package
    (inherit emacs-xelb)
    (name "emacs-xelb-next")
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-xelb)
       ((#:emacs emacs) `,emacs-next)))))

(define-public emacs-exwm-next
  (package
    (inherit emacs-exwm)
    (name "emacs-exwm-next")
    (synopsis "Emacs X window manager (for Emacs next)")
    (propagated-inputs
     (list emacs-xelb-next))
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-exwm)
       ((#:emacs emacs) `,emacs-next)))))

(define-public emacs-org-modern-next
  (package
    (inherit emacs-org-modern)
    (name "emacs-org-modern-next")
    (arguments
     `(#:emacs ,emacs-next)
     )))

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
                  (commit "873befaed7cb48fe30e8d25eff07247ab7e1f069")))
            (sha256
             (base32
              "1n1fj0y107q864185rczvns628x8yq7biq8zn136mzdqnm6im1fv"))))
   (build-system emacs-build-system)
   (propagated-inputs (list
		       emacs-f emacs-s emacs-dash
		       emacs-xelb emacs-fontsloth emacs-log4e
		       emacs-backlight emacs-volume
		       emacs-all-the-icons))
   (arguments
    (list
     #:include #~(cons "^doc/.*\\.info$" %default-include)))
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
    (version "20250321.1717")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/karthink/elfeed-tube.git")
             (commit "79d5a08d76ea3ae96d7def9a5e2ede2e3562462a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pzxama7qyj9i4x74im5r875b7vv1zrkgfncf5j1qxixj96jzfna"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-elfeed emacs-aio))
    (arguments
     '(#:include '("^elfeed-tube.el$" "^elfeed-tube-utils.el$"
                   "^elfeed-tube-fill.el$")
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
    (license #f)))

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

(define-public emacs-language-detection
  (package
    (name "emacs-language-detection")
    (version "20161123.1813")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/andreasjansson/language-detection.el.git")
             (commit "54a6ecf55304fba7d215ef38a4ec96daff2f35a4")))
       (sha256
        (base32 "0p8kim8idh7hg9398kpgjawkxq9hb6fraxpamdkflg8gjk0h5ppa"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/andreasjansson/language-detection.el")
    (synopsis "Automatic language detection from code snippets")
    (description
     "Automatic programming language detection using pre-trained random forest
classifier.  Supported languages: * ada * awk * c * clojure * cpp * csharp * css
* dart * delphi * emacslisp * erlang * fortran * fsharp * go * groovy * haskell
* html * java * javascript * json * latex * lisp * lua * matlab * objc * perl *
php * prolog * python * r * ruby * rust * scala * shell * smalltalk * sql *
swift * visualbasic * xml Entrypoints: * language-detection-buffer - When called
interactively, prints the language of the current buffer to the echo area - When
called non-interactively, returns the language of the current buffer *
language-detection-string - Non-interactive function, returns the language of
its argument")
    (license #f)))

(define-public emacs-shrface
  (package
    (name "emacs-shrface")
    (version "20240401.957")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chenyanming/shrface.git")
             (commit "b82a174ee33f19ed96c7e8c85ec362eab147d4aa")))
       (sha256
        (base32 "0qspg55r397p6p16c4cps547x2w7gdplshy3scr810iwphrzsvfy"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-org emacs-language-detection))
    (home-page "https://github.com/chenyanming/shrface")
    (synopsis "Extend shr/eww with org features and analysis capability")
    (description
     "This package extends `shr / `eww with org features and analysis capability.  It
can be used in `dash-docs', `eww', `nov.el', `mu4e', `anki.el', etc. -
Configurable org-like heading faces, headline bullets, item bullets, paragraph
indentation, fill-column, item bullet, versatile hyper
links(http/https/file/mailto/etc) face and so on. - Browse the internet or local
html file with `eww just like org mode. - Read dash docsets with `dash-docs and
the beauty of org faces. - Read epub files with `nov.el , just like org mode. -
Read html email with `mu4e , the same reading experience just like org mode
without formatting html to org file. - Switch/jump the headlines just like
org-mode in `eww and `nov.el with `imenu - Toggle/cycle the headlines just like
org-mode in `eww and `nov.el with `outline-minor-mode and
`org-cycle'/`org-shifttab - Analysis capability: - Headline analysis: List all
headlines with clickable texts. - URL analysis: List all classified URL with
clickable texts. - Export HTML buffer to an org file using shr engine (no Pandoc
is needed).")
    (license #f)))

(define-public emacs-pollen-mode
  (package
    (name "emacs-pollen-mode")
    (version "20220904.447")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lijunsong/pollen-mode.git")
             (commit "19174fab69ce4d2ae903ef2c3da44054e8b84268")))
       (sha256
        (base32 "1w15v2xj01h9j7glg9854zszh7mi1cdshaacjhplk64s6c9brkfp"))))
    (build-system emacs-build-system)
    (arguments
     '(#:include '("^pollen-mode.el$")
       #:exclude '()))
    (home-page "https://github.com/lijunsong/pollen-mode")
    (synopsis "major mode for editing pollen files")
    (description
     "Pollen mode provides editing assistant for pollen, the digital-publishing tool.
See README for usage in detail.")
    (license #f)))
