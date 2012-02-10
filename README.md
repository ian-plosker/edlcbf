# d-left hashing bloom filter for erlang

This will eventually be a fully functional d-left hashing bloom filter
for Erlang. Currently, it's a WORK IN PROGRESS.

Done:

- basic dlcbf
- NIFs
- `init`, `add`, `in`, `dstry`
- EQC tests that pass (mostly)
- space efficent implementation
- counters

Needs:

- automatic sizing based on max elements and false positive probablity
- `remove`, `resize`
- semi-sorted buckets
- dynamic bit reassignment (?)