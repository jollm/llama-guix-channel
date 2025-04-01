(define-module (llama packages python)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public python-joystick-wake
  (package
    (name "python-joystick-wake")
    (version "0.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/foresto/joystickwake.git")
                    (commit "78bf30e54cbf842af4449d32d6ec9c2f4f1531fa")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qzkirwic2vyvvd59yww852bxn5ggj4g0588k16rbmxxlhndnia5"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Remove global .desktop file from setup.py
          (add-after 'unpack 'remove-setup-desktop
            (lambda _
              (substitute* "setup.py"
                ((".*'/etc/xdg/autostart'.*") "")))))))
    (propagated-inputs (list python-dbus-next python-pyudev python-xlib))
    (home-page "https://github.com/foresto/joystickwake")
    (synopsis "Suppresses screen blankers while game controllers are in use")
    (description "Supports most popular screen savers by default, and can be configured
 with custom commands to support others.")
    (license license:expat)))
