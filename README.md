# d-left hashing bloom filter for erlang

This will eventually be a fully functional d-left hashing bloom filter
for Erlang. Currently, it's a WORK IN PROGRESS.

Done:

- NIFs
- `init`, `add`, `in`
- *extremely* basic dlht
- EQC tests that pass
- space efficent implementation

Needs:

- `remove` and `resize`
- dynamic bit reassignment
- counters
