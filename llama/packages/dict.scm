(define-module (llama packages dict)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public dictd-dicts
  (package
    (name "dictd-dicts")
    (version "0.0-e12f3f3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ferdnyc/dictd-dicts.git")
                    (commit "e12f3f30f78bf176a7f168cb1038f7bb124a684d")))
              (sha256
               (base32
		"00raxdx6hl0dawhclzvvb1xy3j7g5j509l6ap3m3xpx5wz23585m"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("." "share/dict/dictd/devils"
                         #:include ("devils.dict" "devils.index"))
                        ("." "share/dict/dictd/easton"
                         #:include ("easton.dict" "easton.index"))
                        ("." "share/dict/dictd/elements"
                         #:include ("elements.dict" "elements.index"))
                        ("." "share/dict/dictd/foldoc"
                         #:include ("foldoc.dict" "foldoc.index"))
                        ("." "share/dict/dictd/gazetteer"
                         #:include ("gazetteer.dict" "gazetteer.index"))
                        ("." "share/dict/dictd/gcide"
                         #:include ("gcide.dict" "gcide.index"))
                        ("." "share/dict/dictd/hitchcock"
                         #:include ("hitchcock.dict" "hitchcock.index"))
                        ("." "share/dict/dictd/jargon"
                         #:include ("jargon.dict" "jargon.index"))
                        ("." "share/dict/dictd/moby-thesaurus"
                         #:include ("moby-thesaurus.dict" "moby-thesaurus.index"))
                        ("." "share/dict/dictd/vera"
                         #:include ("vera.dict" "vera.index"))
                        ("." "share/dict/dictd/web1913"
                         #:include ("web1913.dict" "web1913.index"))
                        ("." "share/dict/dictd/wn"
                         #:include ("wn.dict" "wn.index"))
                        ("." "share/dict/dictd/world95"
                         #:include ("world95.dict" "world95.index")))))
    (home-page "https://github.com/ferdnyc/dictd-dicts")
    (synopsis "A collection of .dict and .index files for local dictionary servers")
    (description
     "Includes the following databases:
- The Devil's Dictionary
- Easton's Bible Dictionary
- The Elements (like from chemistry)
- Free On-Line Dictionary of Computing
- United States Towns and Zip Codes (Gazetteer)
- The Collaborative International Dictionary of English
- Hitchcock's Bible Names Dictionary
- Jargon File (hacker slang)
- Moby Thesaurus II
- Virtual Entity of Relevant Acronyms
- Webster's Revised Unabridged Dictionary
- WordNet: A lexical database for the English language (2.0)
- The CIA World Factbook")
    (license #f)))
