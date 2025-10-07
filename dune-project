(lang dune 2.7)
(generate_opam_files true)
(allow_approximate_merlin)

(name repr)
(source (github mirage/repr))
(license ISC)
(authors "Thomas Gazagnaire" "Craig Ferguson")
(maintainers "thomas@gazagnaire.org")

(package
 (name repr)
 (documentation "https://mirage.github.io/repr")
 (depends
  (ocaml (>= 4.08.0))
  (fmt (>= 0.8.7))
  uutf
  either
  (jsonm (>= 1.0.0))
  (base64 (>= 3.0.0))
  (optint (>= 0.1.0)))
 (synopsis "Dynamic type representations. Provides no stability guarantee")
 (description "\
This package defines a library of combinators for building dynamic type
representations and a set of generic operations over representable types, used
in the implementation of Irmin and related packages.

It is not yet intended for public consumption and provides no stability
guarantee.
"))

(package
 (name ppx_repr)
 (documentation "https://mirage.github.io/repr")
 (depends
  (repr (= :version))
  (ppxlib (>= 0.36.2))
  ppx_deriving
  fmt
  ; Test dependencies inherited from [repr] (see [test/repr/dune])
  (hex :with-test)
  (optint (and (>= 0.1.0) :with-test))
  (alcotest (and (>= 1.4.0) :with-test)))
 ;; See https://github.com/mirage/repr/issues/48
 ;; Can be removed once using Ppxlib >= 0.16.0
 (conflicts (ocaml-migrate-parsetree (= 1.7.1)))
 (synopsis "PPX deriver for type representations")
 (description "PPX deriver for type representations"))

(package
 (name repr-bench)
 (documentation "https://mirage.github.io/repr")
 (depends
  (repr (= :version))
  (ppx_repr (= :version))
  bechamel
  yojson
  fpath)
 (synopsis "Benchmarks for the `repr` package")
 (description "Benchmarks for the `repr` package"))

(package
 (name repr-fuzz)
 (documentation "https://mirage.github.io/repr")
 (depends
  (repr (= :version))
  (crowbar (= 0.2))
  (ppxlib (and (>= 0.12.0))))
 (synopsis "Fuzz tests for the `repr` package")
 (description "Fuzz tests for the `repr` package"))
