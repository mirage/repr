### 0.6.0 (2022-01-04)

- Change the type of `Repr.decode_bin` to take a mutable buffer offset rather
  than threading an immutable position. (#81, @CraigFe)

- Expose a `Repr.Binary` module providing direct access to functions for
  interacting with Repr's binary serialisation format. (#88, @CraigFe)

### 0.5.0 (2021-10-12)

- Add `Repr.int63`, a representation of the `Optint.Int63.t` type (provided by
  the `optint` library). (#80, @CraigFe)

- Change `Repr.{like,map,partially_abstract}` functions to not require `_
  staged` wrappers around any (monomorphic) overrides. (#77, @CraigFe)

- Fix a bug causing custom `Repr.{random,random_state}` implementations to be
  ignored. (#79, @CraigFe)

- Fix `Repr.pre_hash` to rely on itself recursively. This ensures that custom
  `pre_hash` functions attached to components of larger types are not ignored.
  (#71, @CraigFe)

### 0.4.0 (2021-06-16)

- Add `Repr.{random,random_state}`, a pair of generic functions for sampling
  random instances of representable types. (#58, @CraigFe)

- Add `Repr.Size`, which provides sizing functions for binary codecs that are
  more informative than the existing `Repr.size_of`. Types built using `Repr.v`
  and `Repr.like` must now pass a sizer built using `Repr.Size.custom_*`. (#69,
  @CraigFe)

### 0.3.0 (2021-04-30)

- `Repr.v` is now called `Repr.abstract`. (#52, @CraigFe)

- Added `Repr.partially_abstract`, a helper combinator for constructing type
  representations with overridden operations. (#52, @CraigFe)

- Add combinators for standard library container types: `ref`, `Lazy.t`,
  `Seq.t`, `Queue.t`, `Stack.t`, `Hashtbl.t`, `Set.t` and `Map.t`.
  (#43, @CraigFe)

- Improve PPX `Repr.t` generation for types in the standard library. References
  to e.g. `Bool.t` or `Stdlib.Int32.t` will be resolved to the corresponding
  combinators. (#43, @CraigFe)

- Add support for deriving mutually-recursive pairs of type representations
  with `ppx_repr`. (#42, @CraigFe)

- Add a JSON object combinator: `Json.assoc` (#53, @Ngoguey42)

- Drop the payload of NaN floating point values during JSON encoding. `-nan`
  strings are not emitted any more. (#55, @Ngoguey42)

### 0.2.1 (2021-01-18)

- Support Ppxlib versions >= 0.18.0. (#35, @CraigFe)
- Add missing dependency on `ppx_deriving`. (#36, @CraigFe)
- Add a representation of the `Either.t` type. (#33, @CraigFe)

### 0.2.0 (2020-12-18)

- Improve performance of variable-size integers encoding and decoding.
  (#24, #30, @samoht)
- Require `short_hash` operations to be explicitly unstaged.
  (#15, @CraigFe)
- Require `equal` and `compare` operations to be explicitly unstaged.
  (#16, @samoht)

### 0.1.0 (2020-10-16)

Initial release.
