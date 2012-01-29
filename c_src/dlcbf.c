#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "dlcbf.h"

#if defined(__APPLE__)
#  define COMMON_DIGEST_FOR_OPENSSL
#  include <CommonCrypto/CommonDigest.h>
#  define SHA1 CC_SHA1
#else
#  include <openssl/sha.h>
#endif

dlcbf *init(unsigned int d, unsigned int b) {
    table *tables = (table*)malloc(d * sizeof(*tables));
    unsigned int i;
    for(i = 0; i < d; i++) {
        bucket *buckets = (bucket*)malloc(b * sizeof(*buckets));
        int j;
        for (j = 0; j < b; j++) {
            unsigned int *fingerprints = (unsigned int*)malloc(8 * sizeof(int));
            bucket bucket;
            bucket.count = 0;
            bucket.fingerprints = fingerprints;
            buckets[j] = bucket;
        }
        table table;
        table.buckets = buckets;
        tables[i] = table;
    }
    dlcbf *t = malloc(sizeof(dlcbf));
    t->d = d;
    t->b = b;
    t->count = 0;
    t->tables = tables;
    return t;
}

unsigned int get_bits(const unsigned char* input, const unsigned int bits, const unsigned int n) {
    const unsigned int bitpos = n*bits;
    return ((input[bitpos/8] >> bitpos%8)
        + (input[bitpos/8+1] << (8-bitpos%8)))
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

void add(const unsigned char *data, const unsigned int length, dlcbf *dlcbf) {
    unsigned char hash[20];
    SHA1(data, length, hash);

    const unsigned int *target_buckets = get_target_buckets(dlcbf->d, dlcbf->b, hash);

    unsigned int target_table = 0;
    unsigned int target_bucket = 0;
    unsigned int lowest_count = 0;

    unsigned int i;
    for (i = 0; i < dlcbf->d; i++) {
        unsigned int bucket_i = target_buckets[i];
        unsigned int count = dlcbf->tables[i].buckets[bucket_i].count;

        if (i == 0 || count < lowest_count) {
            lowest_count = count;
            target_table = i;
            target_bucket = bucket_i;
        }
    }

    bucket *bucket = &dlcbf->tables[target_table].buckets[target_bucket];
    memcpy(&bucket->fingerprints[bucket->count], hash, sizeof(unsigned int));
    bucket->count++;
    dlcbf->count++;
}

dlcbf_loc location_of(const unsigned char *data, const unsigned int length, dlcbf *dlcbf) {
    unsigned char hash[20];
    SHA1(data, length, hash);

    unsigned int *target_buckets = get_target_buckets(dlcbf->d, dlcbf->b, hash);
    unsigned int *fingerprint = (unsigned int*)hash;

    int i;
    dlcbf_loc loc = {NULL};
    for(i = 0; i < dlcbf->d; i++) {
        const unsigned int bucket_i = target_buckets[i];
        const bucket bucket = dlcbf->tables[i].buckets[bucket_i];

        const unsigned int *b_loc = item_location(fingerprint, bucket);
        if (b_loc != NULL) {
            loc.fingerprint = b_loc;
            break;
        }
    }

    return loc;
}

int member(const unsigned char *data, const unsigned int length, dlcbf *dlcbf) {
    const dlcbf_loc loc = location_of(data, length, dlcbf);
    return loc.fingerprint != NULL;
}
