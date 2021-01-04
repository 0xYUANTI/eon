```
                      @@@@@@@@  @@@@@@  @@@  @@@
                      @@!      @@!  @@@ @@!@!@@@
                      @!!!:!   @!@  !@! @!@@!!@!
                      !!:      !!:  !!! !!:  !!!
                      : :: :::  : :. :  ::    :
                        Erlang Object Notation

```

# Manual

## Overview

In an Erlang system, a dictionary-style datastructure should be used to
pass data between subsystems. This library supports that observation in
two ways.

## Unified API

The `eon` module exports a clean API for working with dictionaries
(internally referred to as "objects").

- Basics: `new`, `get`, `set`, `del`;
- Higher order: `map`, `filter`, `fold`;
- Sets: `union`, `intersection`, `difference`;
- Iterators: `make`, `next`, `done`.

Most functions take the object as the first argument
(mnemonic: `set(Obj, K, V) --> Obj[K] = V`).

All API functions support both common implementations of the dictionary
interface in Erlang (lists of two-tuples aka proplists and dicts) as inputs and
promote their outputs to dicts.

EON also supports a third representation, lists with an even number of
elements where the odd elements are interpreted as keys and the even
elements are interpreted as values. We call these "literals".
Example:

The following terms are equivalent as far as EON is concerned:

```erlang
dict:store(k2, v2, dict:store(k1, v1, dict:new()))

[ {k1, v1}
, {k2, v2}
]

[ k1, v1
, k2, v2
]
```

## Comprehensive input validation and conversion

### Basics

The `eon_type1 module supports checking EON objects (dicts) against a
type declaration.

The API is `eon_type:check_obj(Obj, Decl)` where both `Obj` and `Decl` are
EON objects (proplists or dicts). `Obj` and `Decl` must have the same
keys. `Obj` maps those keys to arbitrary terms whereas `Decl` maps them
to types. A type is simply an atom, the name of a module implementing
one of the `eon_type_` behaviours.

There are two `eon_type_` behaviours:

#### 1). `eon_type_prim`

Requires implementations of 5 callback functions:

- `name/0` returns the name of the type (for error reporting).
- `parameters/0` returns the parameters needed by the type (see below).
- `normalize/2` takes the term being checked and the current parameters
and returns the term, possibly after applying normalization of some
sort to it. This callback is usually the identity function (but can
come in handy when checking external data, which is often encoded as
strings).
- `validate/2` takes the term being checked and the current parameters and
returns true iff the term is a member of this type (crash or return
false otherwise). This is the central validation callback.
- `convert/2` takes the term being checked and the current parameters and
returns the term, possibly after transforming it to an internal
representation. This is the central conversion callback.

#### 2). `eon_type_prim`

Requires implementations of 3 callback functions.

- `name/0` as above.
- `parameters/0` as above.
- `decl/2` takes the term being checked and the current parameters and
returns a decl object (a specification of what the term, which
implicitly must be an EON object, should look like, see above).
This is the central recursion callback.

### Parameters

All EON types take parameters. These are arbitrary key/value mappings
which provide external context to the various callbacks (for instance,
the `validate/2` callback for a phone number may need to know which
country the phone number is supposed to be legal for).

The `parameters/0` callbacks return a list of parameter names:

```erlang
parameters() -> [].              %no params
parameters() -> [foo, bar]       %two params, foo and bar
parameters() -> [foo, {bar, 42}] %two params, foo and bar
                                 %bar defaults to 42
```

Parameters are represented as EON objects, so the callback functions'
second argument will be a dict mapping all the names in the list
returned by the type's `parameters/0` callback to terms.

There are three kinds of parameters values.

- **Explicit** parameter values are passed in the decl object: instead of just
mapping a key to a type (atom) the decl object may map it to a tuple,
`{Type, Args}`, where `Args` is an object which provides mappings for a
subset of `Type`'s parameters.
- **Implicit** parameter values are extracted from the decl object a type
occurs in. If there's a key which matches the name of a parameter and
no explicit value has been provided for this parameter, the value
associated with this key _after_ type checking (and conversion!) will
be passed as the value of the parameter.
- **Default** parameter values are provided in the paramters callback (see
example above).

Under-parameterized types are untypable.

### Operation

The EON type checker takes the input object and the decl object and
returns the converted (original values are replaced by the output of
the primitive types' `convert/2` callbacks) version of the input object
if and only if all fields can be validated (the primitive types' `validate/2`
callbacks return true).

It does this by computing the fixpoint of the input object under the
parameter-constraints imposed by the decl object. Progress occurs
when either

a) a new field can be validated because we have all parameters for its
   type, or
b) we find a new implicit parameter (which must have been validated
   already).

This probably sounds more complicated than it is. Have a look at
`eon_type.erl`!

### DOCUMENTME

```
alias types
list types
sum types
check_term
&c.
```

# Manifest

```
eon.erl            -- dict API
eon.hrl            -- shared typedefs
eon_type.erl       -- type checker
eon_type_alias.erl -- behaviour for alias types
eon_type_list.erl  -- behaviour for list types
eon_type_prim.erl  -- behaviour for primitive types
eon_type_rec.erl   -- behaviour for recursive types
```
