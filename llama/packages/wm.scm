(define-module (llama packages wm)
  #:use-module (guix build-system copy)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public waybar-dracula-theme
  (package
    (name "waybar-dracula-theme")
    (version "0-799c59e-0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dracula/waybar")
             (commit "799c59ed4ff328e0089586f9d1229a64d4e468e7")))
       (sha256
        (base32 "15l1dgbg84ckc3mhq4li5i4szrlcs9fv29mi2bq3wvdv8m7b60kq"))))
    (build-system copy-build-system)
    (home-page "https://github.com/dracula/waybar")
    (synopsis "A dark theme for waybar")
    (description
     "Font is Iosevka, icons obsidian. Change CITY to your city/town/country in ~/.config/waybar/wittr.sh if you want weather")
    (license #f)))

(define-public waybar-catppuccin-theme
  (package
    (name "waybar-catppuccin-theme")
    (version "1.1-ee8ed32")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/catppuccin/waybar")
             (commit "ee8ed32b4f63e9c417249c109818dcc05a2e25da")))
       (sha256
        (base32 "0q4mzqx3w6cywfifs7ij8qzhzvj59dfdzpvqx76vpnhd2zm35bfd"))))
    (build-system copy-build-system)
    (home-page "https://github.com/catppuccin/waybar")
    (synopsis "Soothing pastel theme for waybar")
    (description
     "For the high-spirited. Comes in flavors latté, frappé, macchiato, mocha")
    (license #f)))
