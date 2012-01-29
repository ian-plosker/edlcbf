# d-left hashing bloom filter for erlang

This will eventually be a fully functional d-left hashing bloom filter
for Erlang. Currently, it's a WORK IN PROGRESS.

Done:

- NIFs
- `new`, `add`, `in`
- *extremely* basic dlht
- EQC tests that pass

Needs:

- `remove` and `resize`
- dynamic bit reassignment
- space efficent implementation
- counters
