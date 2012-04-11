# d-left counting bloom filter for erlang

This is a reasonably functional d-left counting bloom filter for Erlang.

Done:

- basic dlcbf
- NIFs
- `init`, `add`, `in`, `delete`, `destroy`
- EQC tests that pass
- space efficent implementation
- counters

Needs:

- automatic sizing based on max elements and false positive probablity
- semi-sorted buckets
- dynamic bit reassignment (?)
