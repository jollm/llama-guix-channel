(define-module (llama packages jetbrains)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (nonguix build-system binary))

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
     `(("openjdk17" ,openjdk17)
       ("openjdk18" ,openjdk18) ("openjdk15" ,openjdk15) ("openjdk14" ,openjdk14)
       ("openjdk11" ,openjdk11)
       ("maven" ,maven)
       ("gcc:lib" ,gcc "lib") ("glib" ,glib) ("glibc" ,glibc)
       ("libspatialite" ,libspatialite) ("python" ,python) ("sqlite" ,sqlite)
       ("gtk+" ,gtk+)))
    (native-inputs (list patchelf which))
    (arguments
     (list
      #:strip-binaries? #f
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
               (let ((idea-sh (string-append (assoc-ref outputs "out") "/bin/idea.sh"))
                     (jdk-path (assoc-ref inputs "openjdk17"))
                     (jdk18-path (assoc-ref inputs "openjdk18"))
                     (jdk15-path (assoc-ref inputs "openjdk15"))
                     (jdk14-path (assoc-ref inputs "openjdk14"))
                     (jdk11-path (assoc-ref inputs "openjdk11"))
                     (maven-path (assoc-ref inputs "maven")))
                 (wrap-program idea-sh
                   `("IDEA_JDK" = (,jdk-path))
                   `("_JAVA_AWT_WM_NONREPARENTING" = ("1"))
                   `("AWT_TOOLKIT" = ("MToolkit"))
                   `("JDK18_PATH" = (,jdk18-path))
                   `("JDK15_PATH" = (,jdk15-path))
                   `("JDK14_PATH" = (,jdk14-path))
                   `("JDK11_PATH" = (,jdk11-path))
                   `("MAVEN_HOME" = (,maven-path))
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
