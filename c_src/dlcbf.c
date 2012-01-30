#include <stdio.h>
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
    dlcbf *dlcbf = malloc(sizeof(*dlcbf));
    dlcbf->d = d;
    dlcbf->b = b;
    dlcbf->count = 0;

    unsigned int i;
    dlcbf->tables = (table*)malloc(d*sizeof(table));
    for(i = 0; i < d; i++) {
        dlcbf->tables[i].buckets = malloc(sizeof(bucket)*b);
    }

    return dlcbf;
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

FINGERPRINT *item_location(FINGERPRINT fingerprint, bucket* bucket) {
    unsigned int i;
    for(i = 0; i < bucket->count; i++) {
        if (memcmp(&bucket->fingerprints[i], &fingerprint, sizeof(FINGERPRINT)) == 0)
            return &bucket->fingerprints[i];
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

    table *tables = (table*)dlcbf->tables;

    unsigned int i;
    for (i = 0; i < dlcbf->d; i++) {
        unsigned int bucket_i = target_buckets[i];
        unsigned int count = tables[i].buckets[bucket_i].count;

        if (i == 0 || count < lowest_count) {
            lowest_count = count;
            target_table = i;
            target_bucket = bucket_i;
        }
    }

    bucket *bucket = &tables[target_table].buckets[target_bucket];
    memcpy(&bucket->fingerprints[lowest_count], hash, sizeof(FINGERPRINT));
    bucket->count++;
    dlcbf->count++;
}

dlcbf_loc location_of(const unsigned char *data, const unsigned int length, dlcbf *dlcbf) {
    unsigned char hash[20];
    SHA1(data, length, hash);

    unsigned int *target_buckets = get_target_buckets(dlcbf->d, dlcbf->b, hash);
    FINGERPRINT fingerprint;
    memcpy(&fingerprint, hash, sizeof(FINGERPRINT));

    int i;
    dlcbf_loc loc = {NULL};
    for(i = 0; i < dlcbf->d; i++) {
        const unsigned int bucket_i = target_buckets[i];
        const bucket bucket = dlcbf->tables[i].buckets[bucket_i];

        const unsigned int *b_loc = item_location(fingerprint, &bucket);
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
