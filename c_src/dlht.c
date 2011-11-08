#include <stdlib.h>
#include <math.h>
#include <stdio.h>

#if defined(__APPLE__)
#  define COMMON_DIGEST_FOR_OPENSSL
#  include <CommonCrypto/CommonDigest.h>
#  define SHA1 CC_SHA1
#else
#  include <openssl/sha.h>
#endif

// =============================================================================
// Types
// =============================================================================

typedef struct {
    unsigned int count;
    unsigned int *fingerprints;
} bucket;

typedef struct {
    bucket *buckets;
} table;

typedef struct {
    unsigned int d;
    unsigned int b;
    unsigned int count;
    table *tables;
} dlht;

typedef struct {
    const unsigned int *fingerprint;
} dlht_loc;

// =============================================================================
// Methods
// =============================================================================

unsigned char *make_hash(unsigned char *data, unsigned long length) {
    unsigned char *hash = (unsigned char*)malloc(20*sizeof(char));
    return SHA1(data, length, hash);
}

dlht *init(unsigned int d, unsigned int b, void*(*alloc)(unsigned int)) {
    table *tables = (table*)alloc(d * sizeof(*tables));

    unsigned int i;
    for(i = 0; i < d; i++) {
        bucket *buckets = (bucket*)malloc(b * sizeof(*buckets));
        int j;
        for (j = 0; j < b; j++) {
            unsigned int *fingerprints = (unsigned int*)alloc(8 * sizeof(int));
            bucket bucket;
            bucket.count = 0;
            bucket.fingerprints = fingerprints;
            buckets[j] = bucket;
        }
        table table;
        table.buckets = buckets;
        tables[i] = table;
    }
    dlht *t = (dlht*)alloc(sizeof *t);
    t->d = d;
    t->b = b;
    t->count = 0;
    t->tables = tables;
    return t;
}

unsigned int get_bits(const unsigned char* input, const unsigned int bits, const unsigned int n) {
    const unsigned int bitpos = n*bits;
    return (input[bitpos/8] >> bitpos%8)
        + (input[bitpos/8+1] << 8-bitpos%8)
        & (int)(pow(2,bits) - 1);
}

unsigned int* get_target_buckets(const unsigned int d, const unsigned int b, const unsigned char *hash) {
    const int bits = rint(log(b)/log(2));

    unsigned int *target_buckets = (unsigned int*)malloc(d * sizeof(*target_buckets));

    int i;
    for(i = 0; i < d; i++) {
        target_buckets[i] = get_bits(hash, bits, i * bits);
    }

    return target_buckets;
}

unsigned int *item_location(const unsigned int *fingerprint, bucket bucket) {
    unsigned int i;
    for(i = 0; i < bucket.count; i++) {
        if (bucket.fingerprints[i] == *fingerprint) return &bucket.fingerprints[i];
    }
    return NULL;
}

void add(const unsigned char *data, const unsigned int length, dlht *dlht) {
    unsigned char *hash = make_hash((unsigned char*)data, length);
    const unsigned int *target_buckets = get_target_buckets(dlht->d, dlht->b, hash);

    unsigned int target_table = 0;
    unsigned int target_bucket = 0;
    unsigned int lowest_count = 0;

    unsigned int i;
    for (i = 0; i < dlht->d; i++) {
        unsigned int bucket_i = target_buckets[i];
        unsigned int count = dlht->tables[i].buckets[bucket_i].count;

        if (i == 0 || count < lowest_count) {
            lowest_count = count;
            target_table = i;
            target_bucket = bucket_i;
        }
    }

    bucket bucket = dlht->tables[target_table].buckets[target_bucket];
    unsigned int *fingerprint = (unsigned int*)hash;

    bucket.fingerprints[bucket.count] = *fingerprint;
    bucket.count++;
    dlht->tables[target_table].buckets[target_bucket] = bucket;
    dlht->count++;
}

dlht_loc location_of(const char *data, const unsigned int length, dlht *dlht) {
    unsigned char *hash = make_hash((unsigned char*)data, length);
    unsigned int *target_buckets = get_target_buckets(dlht->d, dlht->b, hash);
    unsigned int *fingerprint = (unsigned int*)hash;

    int i;
    for(i = 0; i < dlht->d; i++) {
        const unsigned int bucket_i = target_buckets[i];
        const bucket bucket = dlht->tables[i].buckets[bucket_i];

        const unsigned int *b_loc = item_location(fingerprint, bucket);
        if (b_loc != NULL) {
            const dlht_loc loc = {b_loc};
            return loc;
        }
    }

    const dlht_loc loc = {NULL};
    return loc;
}

int member(const char *data, const unsigned int length, dlht *dlht) {
    const dlht_loc loc = location_of(data, length, dlht);
    return loc.fingerprint != NULL;
}

int main() {
    dlht *t;
    t = init(1,2,malloc);

    printf("d: %i, b: %i\n", t->d, t->b);

    add("test1", 5, t);
    add("test2", 5, t);
    add("test3", 5, t);
    add("test5", 5, t);

    printf("test1: %i\n", member("test1", 5, t));
    printf("test2: %i\n", member("test2", 5, t));
    printf("test3: %i\n", member("test3", 5, t));
    printf("test4: %i\n", member("test4", 5, t));
    printf("count: %i\n", t->count);

    return 0;
}
