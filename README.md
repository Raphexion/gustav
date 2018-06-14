gustav
=====

A helper library.

Examples
--------

Packing up a dictionary into a binary payload:

```
Dictionary = #{a => 1, b => 2, c => 3, d => 4},
Packer = packer(Dictionary),
Payload = Packer([{a, 8}, {b, 16}, {c, 32}, {d, 8}]),
Payload.
<<1:8, 2:16, 3:32, 4:8>>
```

Unpacking a binary payload into a dictionary:

```
OrgDictionary = #{q => 42},
Payload = <<1:8, 2:16, 3:32, 4:8>>,
UnPacker = unpacker(OrgDictionary, Payload),
NewDictionary = UnPacker([{a, 8}, {b, 16}, {c, 32}, {d, 8}]),
NewDictionary.
#{a => 1, b => 2, c => 3, d => 4, q => 42}
```

Build
-----

    $ rebar3 compile
