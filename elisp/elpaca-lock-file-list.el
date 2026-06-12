(
 (dired-single :source "elpaca-menu-lock-file" :recipe
               (:package "dired-single" :fetcher codeberg :repo
                         "amano.kenji/dired-single" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                          "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el"
                                    "*-test.el" "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth
                         treeless :ref
                         "b3db172b5961e10c7236d6f997d794d3b9e0f03d"))
)
