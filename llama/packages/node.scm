(define-module (llama packages node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages node)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

(define libuv-for-node
  ;; When upgrading Node, also upgrade this. Get the version from
  ;; https://github.com/nodejs/node/blob/master/deps/uv/include/uv/version.h
  (package
    (inherit libuv)
    (name "libuv")
    (version "1.44.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dist.libuv.org/dist/v" version
                                  "/libuv-v" version ".tar.gz"))
              (sha256
               (base32
                "1d1wy1061cf2mfygr2j6jbm0da2mhsf0l9yq4rjkqrsmijbdrz6c"))))
    ))

(define-public llhttp-bootstrap
  (package
    (name "llhttp")
    (version "6.0.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nodejs/llhttp.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0izwqa77y007xdi0bj3ccw821n19rz89mz4hx4lg99fwkwylr6x8"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Fix imports for esbuild.
                  ;; https://github.com/evanw/esbuild/issues/477
                  (substitute* "src/llhttp/http.ts"
                    (("\\* as assert") "assert"))
                  (substitute* "Makefile"
                    (("npx ts-node bin/generate.ts")
                     "node bin/generate.js"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list (string-append "CLANG=" ,(cc-for-target))
                          (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "PREFIX=")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((esbuild (search-input-file (or native-inputs inputs)
                                               "/bin/esbuild")))
               (invoke esbuild
                       "--platform=node"
                       "--outfile=bin/generate.js"
                       "--bundle" "bin/generate.ts"))))
         (add-before 'install 'create-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (dir)
                           (mkdir-p (string-append out dir)))
                         (list "/lib" "/include" "/src"))
               #t)))
         (add-after 'install 'install-src
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (src-dir (string-append out "/src")))
               (install-file "build/c/llhttp.c" src-dir)
               (install-file "src/native/api.c" src-dir)
               (install-file "src/native/http.c" src-dir)
               #t))))))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("node" ,node-bootstrap)
       ("node-semver" ,node-semver-bootstrap)
       ("node-llparse-bootstrap" ,node-llparse-bootstrap)))
    (home-page "https://github.com/nodejs/llhttp")
    (properties '((hidden? . #t)))
    (synopsis "Parser for HTTP messages")
    (description "This is a rewrite of
@url{https://github.com/nodejs/http-parser, http-parser} using
@url{https://github.com/nodejs/llparse, llparse} to generate the C
source files.")
    (license license:expat)))

(define-public node-lts
  (package
    (inherit node)
    (version "18.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.gz"))
              (sha256
               (base32
                "0axc4jl71wjjhwzpdc3xg607r32554d0h0lmvb65p0f8ycz4qp6n"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; openssl.cnf is required for build.
                  (for-each delete-file-recursively
                            (find-files "deps/openssl"
                                        (lambda (file stat)
                                          (if (string-contains file "nodejs-openssl.cnf")
                                              #f #t))))
                  ;; Remove bundled software, where possible
                  (for-each delete-file-recursively
                            '(
                              "deps/cares"
                              "deps/icu-small"
                              "deps/nghttp2"
                              "deps/zlib"
                              ))
                  (substitute* "Makefile"
                    ;; Remove references to bundled software.
                    (("deps/uv/uv.gyp") "")
                    (("deps/zlib/zlib.gyp") ""))
                  #t))
              ))
    (arguments
     (substitute-keyword-arguments (package-arguments node)
       ((#:configure-flags configure-flags)
        ``("--shared-cares"
           "--shared-libuv"
           "--shared-nghttp2"
           "--shared-openssl"
           "--shared-zlib"
           "--shared-brotli"
           "--with-intl=system-icu"
           ;;Needed for correct snapshot checksums
           "--v8-enable-snapshot-compression"
           ))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'set-bootstrap-host-rpath
             (lambda* (#:key native-inputs inputs #:allow-other-keys)
               (let* ((inputs        (or native-inputs inputs))
                      (c-ares        (assoc-ref inputs "c-ares"))
                      (brotli        (assoc-ref inputs "brotli"))
                      (icu4c         (assoc-ref inputs "icu4c"))
                      (nghttp2       (assoc-ref inputs "nghttp2"))
                      (openssl       (assoc-ref inputs "openssl"))
                      (libuv         (assoc-ref inputs "libuv"))
                      (zlib          (assoc-ref inputs "zlib"))
                      (host-binaries '("torque"
                                       "bytecode_builtins_list_generator"
                                       "gen-regexp-special-case"
                                       "node_mksnapshot"
                                       "mksnapshot")))
                 (substitute* '("node.gyp" "tools/v8_gypfiles/v8.gyp")
                   (((string-append "'target_name': '("
                                    (string-join host-binaries "|")
                                    ")',")
                      target)
                    (string-append target
                                   "'ldflags': ['-Wl,-rpath="
                                   c-ares "/lib:"
                                   brotli "/lib:"
                                   icu4c "/lib:"
                                   nghttp2 "/lib:"
                                   ;; openssl "/lib:"
                                   libuv "/lib:"
                                   zlib "/lib"
                                   "'],"))))))
           (add-after 'patch-hardcoded-program-references
               'patch-additional-hardcoded-program-references
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "test/parallel/test-stdin-from-file-spawn.js"
                 (("'/bin/sh'") (string-append
                                 "'" (search-input-file inputs "/bin/sh")
                                 "'")))))
           (replace 'delete-problematic-tests
             (lambda* (#:key inputs #:allow-other-keys)
               ;; FIXME: These tests fail in the build container, but they don't
               ;; seem to be indicative of real problems in practice.
               (for-each delete-file
                         '(
                           "test/parallel/test-cluster-primary-error.js"
                           "test/parallel/test-cluster-primary-kill.js"
                            ))

               ;; These require a DNS resolver.
               (for-each delete-file
                         '("test/parallel/test-dns.js"
                           "test/parallel/test-dns-lookupService-promises.js"
                           ))

               ;; These tests require networking.
               (for-each delete-file
                         '("test/parallel/test-https-agent-unref-socket.js"
                           "test/parallel/test-tcp-wrap-listen.js"
                           "test/parallel/test-net-socket-connect-without-cb.js"
                           ))

               ;; This test is timing-sensitive, and fails sporadically on
               ;; slow, busy, or even very fast machines.
               (delete-file "test/parallel/test-fs-utimes.js")

               ;; FIXME: This test fails randomly:
               ;; https://github.com/nodejs/node/issues/31213
               (delete-file "test/parallel/test-net-listen-after-destroying-stdin.js")

               ;; hard-coded /bin/sh path
               (delete-file "test/parallel/test-stdin-from-file-spawn.js")

               ;; FIXME: These tests fail on armhf-linux:
               ;; https://github.com/nodejs/node/issues/31970
               ,@(if (target-arm32?)
                     '((for-each delete-file
                                 '("test/parallel/test-zlib.js"
                                   "test/parallel/test-zlib-brotli.js"
                                   "test/parallel/test-zlib-brotli-flush.js"
                                   "test/parallel/test-zlib-brotli-from-brotli.js"
                                   "test/parallel/test-zlib-brotli-from-string.js"
                                   "test/parallel/test-zlib-convenience-methods.js"
                                   "test/parallel/test-zlib-random-byte-pipes.js"
                                   "test/parallel/test-zlib-write-after-flush.js")))
                     '())

               ;; These tests have an expiry date: they depend on the validity of
               ;; TLS certificates that are bundled with the source.  We want this
               ;; package to be reproducible forever, so remove those.
               ;; TODO: Regenerate certs instead.
               (for-each delete-file
                         '("test/parallel/test-tls-passphrase.js"
                           "test/parallel/test-tls-server-verify.js"))))
           (add-after 'delete-problematic-tests 'replace-llhttp-sources
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Replace pre-generated llhttp sources
               (let ((llhttp (assoc-ref inputs "llhttp")))
                 (copy-file (string-append llhttp "/src/llhttp.c")
                            "deps/llhttp/src/llhttp.c")
                 (copy-file (string-append llhttp "/src/api.c")
                            "deps/llhttp/src/api.c")
                 (copy-file (string-append llhttp "/src/http.c")
                            "deps/llhttp/src/http.c")
                 (copy-file (string-append llhttp "/include/llhttp.h")
                            "deps/llhttp/include/llhttp.h"))))
           ))))
    (native-inputs
     (list ;; Runtime dependencies for binaries used as a bootstrap.
           c-ares-for-node
           brotli
           icu4c
           libuv-for-node
           `(,nghttp2 "lib")
           openssl
           zlib
           ;; Regular build-time dependencies.
           perl
           pkg-config
           procps
           python
           python-bz2file
           util-linux
           which))
    (inputs
     (list bash-minimal
           coreutils
           c-ares-for-node
           icu4c
           libuv-for-node
           llhttp-bootstrap
           brotli
           `(,nghttp2 "lib")
           openssl
           python-wrapper ;; for node-gyp (supports python3)
           zlib))))

(define-public libnode
  (package/inherit node-lts
    (name "libnode")
    (arguments
     (substitute-keyword-arguments (package-arguments node)
       ((#:configure-flags flags ''())
        `(cons* "--shared" "--without-npm" ,flags))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (delete 'install-npmrc)
           (delete 'patch-nested-shebangs)))))))
