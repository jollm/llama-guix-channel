(define-module (llama packages patool)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages file)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages xiph))

(define-public patool
  (package
    (name "patool")
    (version "1.12")
    (source
     (origin
       (method git-fetch)               ;no test data in PyPI archive
       (uri (git-reference
             (url "https://github.com/wummel/patool")
             (commit (string-append "upstream/" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0zgvgx9549rvb57rgkpjalydz46k71gibfs6ab3b3sy439s0ay4h"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; until the dev team sorts it out
       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "pytest")))))))
    (native-inputs
     (list bzip2
           cabextract
           cdrtools
           clzip
           cpio
           dpkg
           file
           flac
           libarchive ;for bsdtar
           lhasa
           lrzip
           lzip
           lzop
           python-pytest
           p7zip
           pbzip2
           pigz
           plzip
           rpm))
    (home-page "https://wummel.github.io/patool/")
    (synopsis "Portable archive file manager")
    (description "Patool provides a unified command line interface for a
plethora of archivers.  It supports the following archive formats natively:
@itemize
@item TAR (.tar, .cbt)
@item BZIP2 (.bz2)
@item GZIP (.gz)
@item ZIP (zip, .jar, .cbz).
@end itemize
The archive formats below are also supported as long as the corresponding
compressor or archiver commands are available:
@itemize
@item 7z (.7z, .cb7)
@item ACE (.ace, .cba)
@item ADF (.adf)
@item ALZIP (.alz)
@item APE (.ape)
@item AR (.a)
@item ARC (.arc)
@item ARJ (.arj)
@item CAB (.cab)
@item COMPRESS (.Z)
@item CPIO (.cpio)
@item DEB (.deb)
@item DMS (.dms)
@item FLAC (.flac)
@item GZIP (.gz)
@item ISO (.iso)
@item LRZIP (.lrz)
@item LZH (.lha, .lzh)
@item LZIP (.lz)
@item LZMA (.lzma)
@item LZOP (.lzo)
@item RPM (.rpm)
@item RAR (.rar, .cbr)
@item RZIP (.rz)
@item SHN (.shn)
@item TAR (.tar, .cbt)
@item XZ (.xz)
@item ZOO (.zoo).
@end itemize")
    (license license:gpl3+)))
