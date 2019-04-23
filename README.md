gustav
=====

[![Build Status](https://travis-ci.org/Raphexion/gustav.svg?branch=master)](https://travis-ci.org/Raphexion/gustav)
[![codecov.io](https://codecov.io/gh/Raphexion/gustav/coverage.svg?branch=master)](https://codecov.io/gh/Raphexion/gustav?branch=master)

A helper library for two use-cases:

1. To help pack-up and un-pack data
2. To help with writing a udp-server

Examples
--------

Imagine that you have a state `#{a => 1, b => 2, c => 3, d => 4}`.
You need to pack-up this data (serialize) and send it over the wire.
Your protocol specifies that:

| Variable | Bits in payload |
|----------|-----------------|
| a        | 8               |
| b        | 16              |
| c        | 32              |
| d        | 8               |

Which can be described compactly as `[{a, 8}, {b, 16}, {c, 32}, {d, 8}]`.

When using *Gustav*, packing up the dictionary into a binary payload:

```
Dictionary = #{a => 1, b => 2, c => 3, d => 4},
Packer = packer(Dictionary),
Payload = Packer([{a, 8}, {b, 16}, {c, 32}, {d, 8}]),
Payload.
<<1:8, 2:16, 3:32, 4:8>>
```

To unpack the binary payload is similar.
We describe the binary protocol `[{a, 8}, {b, 16}, {c, 32}, {d, 8}]`
and use a unpacker function.

Normally we only want to unpack the data into an empty map `#{}`.
However, in some cases it is convenient to allow some "other" data
in the map (state).

Using *Gustav*, to unpacking a binary payload into a dictionary:

```
Payload = <<1:8, 2:16, 3:32, 4:8>>,
UnPacker = unpacker(#{}, Payload),
NewDictionary = UnPacker([{a, 8}, {b, 16}, {c, 32}, {d, 8}]),
NewDictionary.
#{a => 1, b => 2, c => 3, d => 4}
```

Or if you already have some data you want to preserve `#{q => 42}`:

```
OrgDictionary = #{q => 42}  %% q => 42 is mapping we want to
Payload = <<1:8, 2:16, 3:32, 4:8>>,
UnPacker = unpacker(OrgDictionary, Payload),
NewDictionary = UnPacker([{a, 8}, {b, 16}, {c, 32}, {d, 8}]),
NewDictionary.
#{a => 1, b => 2, c => 3, d => 4, q => 42}
```

Build
-----

    $ rebar3 compile
