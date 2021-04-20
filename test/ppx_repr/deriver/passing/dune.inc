; -------- Test: `alias.ml` --------

; The PPX-dependent executable under test
(executable
 (name alias)
 (modules alias)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets alias.actual)
 (deps
  (:pp pp.exe)
  (:input alias.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff alias.expected alias.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./alias.exe)))

; -------- Test: `arguments.ml` --------

; The PPX-dependent executable under test
(executable
 (name arguments)
 (modules arguments)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets arguments.actual)
 (deps
  (:pp pp.exe)
  (:input arguments.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff arguments.expected arguments.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./arguments.exe)))

; -------- Test: `as_alias.ml` --------

; The PPX-dependent executable under test
(executable
 (name as_alias)
 (modules as_alias)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets as_alias.actual)
 (deps
  (:pp pp.exe)
  (:input as_alias.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff as_alias.expected as_alias.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./as_alias.exe)))

; -------- Test: `basic.ml` --------

; The PPX-dependent executable under test
(executable
 (name basic)
 (modules basic)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets basic.actual)
 (deps
  (:pp pp.exe)
  (:input basic.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff basic.expected basic.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./basic.exe)))

; -------- Test: `composite.ml` --------

; The PPX-dependent executable under test
(executable
 (name composite)
 (modules composite)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets composite.actual)
 (deps
  (:pp pp.exe)
  (:input composite.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff composite.expected composite.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./composite.exe)))

; -------- Test: `extension.ml` --------

; The PPX-dependent executable under test
(executable
 (name extension)
 (modules extension)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets extension.actual)
 (deps
  (:pp pp.exe)
  (:input extension.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff extension.expected extension.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./extension.exe)))

; -------- Test: `json_module.ml` --------

; The PPX-dependent executable under test
(executable
 (name json_module)
 (modules json_module)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets json_module.actual)
 (deps
  (:pp pp.exe)
  (:input json_module.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff json_module.expected json_module.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./json_module.exe)))

; -------- Test: `lib_relocated.ml` --------

; The PPX-dependent executable under test
(executable
 (name lib_relocated)
 (modules lib_relocated)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets lib_relocated.actual)
 (deps
  (:pp pp.exe)
  (:input lib_relocated.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff lib_relocated.expected lib_relocated.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./lib_relocated.exe)))

; -------- Test: `meta_deriving.ml` --------

; The PPX-dependent executable under test
(executable
 (name meta_deriving)
 (modules meta_deriving)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets meta_deriving.actual)
 (deps
  (:pp pp.exe)
  (:input meta_deriving.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff meta_deriving.expected meta_deriving.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./meta_deriving.exe)))

; -------- Test: `module.ml` --------

; The PPX-dependent executable under test
(executable
 (name module)
 (modules module)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets module.actual)
 (deps
  (:pp pp.exe)
  (:input module.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff module.expected module.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./module.exe)))

; -------- Test: `nobuiltin.ml` --------

; The PPX-dependent executable under test
(executable
 (name nobuiltin)
 (modules nobuiltin)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets nobuiltin.actual)
 (deps
  (:pp pp.exe)
  (:input nobuiltin.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff nobuiltin.expected nobuiltin.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./nobuiltin.exe)))

; -------- Test: `nonrec.ml` --------

; The PPX-dependent executable under test
(executable
 (name nonrec)
 (modules nonrec)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets nonrec.actual)
 (deps
  (:pp pp.exe)
  (:input nonrec.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff nonrec.expected nonrec.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./nonrec.exe)))

; -------- Test: `polyvariant.ml` --------

; The PPX-dependent executable under test
(executable
 (name polyvariant)
 (modules polyvariant)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets polyvariant.actual)
 (deps
  (:pp pp.exe)
  (:input polyvariant.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff polyvariant.expected polyvariant.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./polyvariant.exe)))

; -------- Test: `record.ml` --------

; The PPX-dependent executable under test
(executable
 (name record)
 (modules record)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets record.actual)
 (deps
  (:pp pp.exe)
  (:input record.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff record.expected record.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./record.exe)))

; -------- Test: `recursive.ml` --------

; The PPX-dependent executable under test
(executable
 (name recursive)
 (modules recursive)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets recursive.actual)
 (deps
  (:pp pp.exe)
  (:input recursive.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff recursive.expected recursive.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./recursive.exe)))

; -------- Test: `signature.ml` --------

; The PPX-dependent executable under test
(executable
 (name signature)
 (modules signature)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets signature.actual)
 (deps
  (:pp pp.exe)
  (:input signature.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff signature.expected signature.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./signature.exe)))

; -------- Test: `tuple_deep.ml` --------

; The PPX-dependent executable under test
(executable
 (name tuple_deep)
 (modules tuple_deep)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets tuple_deep.actual)
 (deps
  (:pp pp.exe)
  (:input tuple_deep.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff tuple_deep.expected tuple_deep.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./tuple_deep.exe)))

; -------- Test: `type_params.ml` --------

; The PPX-dependent executable under test
(executable
 (name type_params)
 (modules type_params)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets type_params.actual)
 (deps
  (:pp pp.exe)
  (:input type_params.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff type_params.expected type_params.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./type_params.exe)))

; -------- Test: `variant.ml` --------

; The PPX-dependent executable under test
(executable
 (name variant)
 (modules variant)
 (preprocess (pps ppx_repr))
 (libraries repr))

; Run the PPX on the `.ml` file
(rule
 (targets variant.actual)
 (deps
  (:pp pp.exe)
  (:input variant.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (diff variant.expected variant.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_repr)
 (action
  (run ./variant.exe)))


