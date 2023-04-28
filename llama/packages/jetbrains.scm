(define-module (llama packages jetbrains)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mate)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (nonguix build-system binary))

(define-public jetbrains-jdk
  (hidden-package
   (package
     (name "jetbrains-jdk")
     (version "2023.1")
     (source (origin
               (method url-fetch)
               (uri (string-append "https://download.jetbrains.com/idea/ideaIU-"
				   version ".tar.gz"))
               (sha256
                (base32
                 "1b6jq83r4fwyxmm6vvraskmpmlqh627ypv0z0bqgx1kdqd8wfa9h"))))
     (build-system binary-build-system)
     (inputs `(("alsa-lib" ,alsa-lib)
               ("cups" ,cups)
               ("dbus" ,dbus)
               ("fontconfig" ,fontconfig)
               ("freetype" ,freetype)
               ("giflib" ,giflib)
               ("gcc:lib" ,gcc "lib")
               ("glibc" ,glibc)
               ("lcms" ,lcms)
               ("libdrm" ,libdrm)
               ("libexpat" ,expat)
               ("libjpeg-turbo" ,libjpeg-turbo)
               ("libpng" ,libpng)
               ("libx11" ,libx11)
               ("libxcb" ,libxcb)
               ("libxext" ,libxext)
               ("libxi" ,libxi)
               ("libxkbcommon" ,libxkbcommon)
               ("libxrandr" ,libxrandr)
               ("libxrender" ,libxrender)
               ("libxt" ,libxt)
               ("libxtst" ,libxtst)
               ("mate" ,mate)
               ("mesa" ,mesa)
               ("nss" ,nss)
               ("nspr" ,nspr)
               ("pango" ,pango)
               ("zlib" ,zlib)))
     (native-inputs (list patchelf which))
     (arguments
      (list
       #:strip-binaries? #f
       #:patchelf-plan
       #~(let* ((patchelf-inputs (list "alsa-lib" "cups" "dbus" "freetype" "gcc:lib" "giflib" "glibc"
                                       "lcms" "libdrm" "libexpat" "libjpeg-turbo" "libpng" "libx11"
                                       "libxcb" "libxext" "libxi" "libxkbcommon"
                                       "libxrandr" "libxrender" "libxt" "libxtst" "mate" "mesa"
                                       '("nss" "/lib/nss") "nspr" "pango" "zlib"
                                       "out" '("out" "/lib/server")))
                (bin-path "jbr/bin/")
                (lib-path "jbr/lib/"))
           (append
            (map (lambda (file) (list (string-append bin-path file) patchelf-inputs))
                 '("javadoc" "jstack" "keytool" "jmap" "jstat" "serialver" "jcmd" "jinfo" "jhsdb"
                   "rmiregistry" "jdb" "java" "javac" "jrunscript" "jfr" "jps"))
            (map (lambda (file) (list (string-append lib-path file) patchelf-inputs))
                 '("libEGL.so" "libjsound.so" "librmi.so" "libjava.so" "libj2pkcs11.so" "libsctp.so"
                   "libawt_headless.so" "server/libjvm.so" "server/libjsig.so" "libawt_xawt.so"
                   "liblcms.so" "libnativewindow_x11.so" "libextnet.so" "libmanagement.so"
                   "libjogl_mobile.so" "libj2gss.so" "libvk_swiftshader.so" "libjcef.so"
                   "libdt_socket.so" "libmlib_image.so" "libverify.so" "libsaproc.so"
                   "libjsig.so" "libnio.so" "libjdwp.so" "libmanagement_ext.so" "libnewt_head.so"
                   "libjaas.so" "libj2pcsc.so" "libawt.so" "libnewt_drm.so" "libnativewindow_drm.so"
                   "libjli.so" "libjavajpeg.so" "libfontmanager.so" "libattach.so" "libsplashscreen.so"
                   "libnativewindow_awt.so" "libzip.so" "libnet.so" "libvulkan.so.1"
                   "libmanagement_agent.so" "libGLESv2.so" "libjimage.so" "libprefs.so"
                   "libjogl_desktop.so" "libjawt.so" "libinstrument.so" "libcef.so"
                   "libgluegen_rt.so" "chrome-sandbox" "jcef_helper" "jspawnhelper" "jexec"))))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'chmod-lib-bins
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((lib-path (string-append (assoc-ref outputs "out") "/lib/"))
                     (bins '("jcef_helper" "jspawnhelper" "jexec")))
                 (map
                  (lambda (bin)
                    (chmod (string-append lib-path bin) #o755))
                  bins)))))
       #:install-plan
       #~'(("jbr" "."))))
     (synopsis "JetBrains Bundled JDK")
     (description "JetBrains Bundled JDK - (a version of OpenJDK 17)")
     (home-page "https://www.jetbrains.com/idea/")
     (license #f))))

(define-public intellij-idea-ultimate
  (package
    (name "intellij-idea-ultimate")
    (version "2023.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.jetbrains.com/idea/ideaIU-"
				  version ".tar.gz"))
              (sha256
               (base32
                "1b6jq83r4fwyxmm6vvraskmpmlqh627ypv0z0bqgx1kdqd8wfa9h"))))
    (build-system binary-build-system)
    (inputs
     `(("jetbrains-jdk" ,jetbrains-jdk)
       ("openjdk17" ,openjdk17)
       ("openjdk18" ,openjdk18) ("openjdk15" ,openjdk15) ("openjdk14" ,openjdk14)
       ("openjdk11" ,openjdk11)
       ("maven" ,maven)
       ("gcc:lib" ,gcc "lib") ("glib" ,glib) ("glibc" ,glibc)
       ("libspatialite" ,libspatialite) ("python" ,python) ("sqlite" ,sqlite)
       ("gtk+" ,gtk+)
       ("e2fsprogs" ,e2fsprogs)))
    (native-inputs (list patchelf which))
    (arguments
     (list
      #:strip-binaries? #f
      #:patchelf-plan
      #~(let* ((patchelf-inputs (list "gcc:lib" "glib" "glibc" "gtk+"
                                      "libspatialite" "sqlite" "jetbrains-jdk"
                                      "out"))
               (bin-path "bin/"))
          (append
           (map (lambda (file) (list (string-append bin-path file) patchelf-inputs))
        	'("libdbm.so" "fsnotifier"))))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'wrap-launcher
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((idea-sh (string-append (assoc-ref outputs "out") "/bin/idea.sh"))
                     (jdk-path (assoc-ref inputs "jetbrains-jdk"))
                     (jdk18-path (assoc-ref inputs "openjdk18"))
                     (jdk17-path (assoc-ref inputs "openjdk17"))
                     (jdk15-path (assoc-ref inputs "openjdk15"))
                     (jdk14-path (assoc-ref inputs "openjdk14"))
                     (jdk11-path (assoc-ref inputs "openjdk11"))
                     (maven-path (assoc-ref inputs "maven")))
                 (wrap-program idea-sh
                   `("IDEA_JDK" = (,jdk-path))
                   `("_JAVA_AWT_WM_NONREPARENTING" = ("1"))
                   `("AWT_TOOLKIT" = ("MToolkit"))
                   `("JDK18_PATH" = (,jdk18-path))
                   `("JDK17_PATH" = (,jdk17-path))
                   `("JDK15_PATH" = (,jdk15-path))
                   `("JDK14_PATH" = (,jdk14-path))
                   `("JDK11_PATH" = (,jdk11-path))
                   `("MAVEN_HOME" = (,maven-path))
                   `("LD_LIBRARY_PATH" ":" prefix
                     (,(string-append (assoc-ref inputs "e2fsprogs") "/lib")))
                   ))))
	 )
       #:install-plan
       #~'(("bin" "bin")
           ("help" "help")
           ("lib" "lib")
           ("license" "license")
           ("plugins" "plugins")
           ("build.txt" "build.txt")
           ("Install-Linux-tar.txt" "Install-Linux-tar.txt")
           ("product-info.json" "product-info.json"))))
    (synopsis "JetBrains IntelliJ IDEA - Java and Kotlin IDE")
    (description "Java and Kotlin IDE, subscription based binary edition")
    (home-page "https://www.jetbrains.com/idea/")
    (license #f)))

(define-public goland
  (package
    (version "2023.1")
    (name "goland")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download-cdn.jetbrains.com/go/goland-"
				  version ".tar.gz"))
              (sha256
               (base32
                "0b1dsjfkjqy8shlbsgln4iwlnqff2j64whs9zwrf5i7m3r3y427j"))))
    (build-system binary-build-system)
    (inputs
     `(("jetbrains-jdk" ,jetbrains-jdk)
       ("openjdk17" ,openjdk17)
       ("gcc:lib" ,gcc "lib") ("glib" ,glib) ("glibc" ,glibc)
       ("libspatialite" ,libspatialite) ("python" ,python) ("sqlite" ,sqlite)
       ("gtk+" ,gtk+)
       ("e2fsprogs" ,e2fsprogs)
       ("go" ,go-1.20)))
    (native-inputs (list patchelf which))
    (arguments
     (list
      #:patchelf-plan
      #~(let* ((patchelf-inputs (list "gcc:lib" "glib" "glibc" "gtk+"
                                      "libspatialite" "sqlite" "openjdk17"
                                      "out"))
               (bin-path "bin/"))
          (append
           (map (lambda (file) (list (string-append bin-path file) patchelf-inputs))
                '("libdbm.so" "fsnotifier"))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-launcher
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((goland-sh (string-append (assoc-ref outputs "out") "/bin/goland.sh"))
                    (jdk-path (assoc-ref inputs "jetbrains-jdk"))
                    (jdk17-path (assoc-ref inputs "openjdk17"))
                    (go-root (string-append (assoc-ref inputs "go") "/lib/go")))
                (wrap-program goland-sh
                  `("IDEA_JDK" = (,jdk17-path))
                  `("GOLAND_JDK" = (,jdk17-path))
                  `("GOROOT" = (,go-root))
                  `("_JAVA_AWT_WM_NONREPARENTING" = ("1"))
                  `("AWT_TOOLKIT" = ("MToolkit"))
                  `("CGO_CPPFLAGS" " " prefix ("-U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0"))
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append (assoc-ref inputs "e2fsprogs") "/lib"))))))))
      #:install-plan
       #~'(("bin" "bin")
           ("help" "help")
           ("lib" "lib")
           ("license" "license")
           ("plugins" "plugins")
           ("build.txt" "build.txt")
           ("Install-Linux-tar.txt" "Install-Linux-tar.txt")
           ("product-info.json" "product-info.json"))))
    (synopsis "JetBrains Golang - IDE")
    (description "Golang subscription based binary edition")
    (home-page "https://www.jetbrains.com/go/")
    (license #f)))
