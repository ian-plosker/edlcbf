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
    dlcbf->tables = (dlcbf_table*)malloc(d*sizeof(dlcbf_table));
    for(i = 0; i < d; i++) {
        dlcbf->tables[i].buckets = malloc(sizeof(dlcbf_bucket)*b);
        unsigned int j;
        for (j = 0; j < b; j++) {
            dlcbf->tables[i].buckets[j].count = 0;
        }
    }

    return dlcbf;
}

unsigned int get_bits(const unsigned char *input, unsigned int bits, const unsigned int n) {
    unsigned int value = 0;
    unsigned int bitsleft = bits;

    if (bitsleft > 8) {
        for (; bitsleft > 8; bitsleft-=8) {
            value <<= 8;
            value += get_bits(input, 8, n + (bits - bitsleft));
        }
        value <<= bitsleft;
    }

    const unsigned int bitpos = n*bitsleft;
    value += (input[bitpos/8] >> bitpos%8)
        + (input[bitpos/8+1] << 8-bitpos%8)
        & (unsigned int)(pow(2,bitsleft) - 1);

    return value;
}

dlcbf_bucket_fingerprint *get_targets(const unsigned int d, const unsigned int b, const unsigned char *hash, dlcbf_bucket_fingerprint *targets) {
    const int bits = rint(log(b)/log(2));

    int i;
    for(i = 0; i < d; i++) {
        targets[i].bucket_i = get_bits(hash, bits, i * bits);
        targets[i].fingerprint = get_bits(hash, 32, (i+1) * bits);
    }

    return targets;
}

dlcbf_field *item_location(const FINGERPRINT fingerprint, dlcbf_bucket* bucket) {
    unsigned int i;
    unsigned int mask = pow(2, sizeof(FINGERPRINT)*8)-1;

    for(i = 0; i < bucket->count; i++) {
        FINGERPRINT fingerprint_i = bucket->fields[i].fingerprint;
        if ((fingerprint & mask) == (fingerprint_i & mask))
            return &bucket->fields[i];
    }
    return NULL;
}

void add(const unsigned char *data, const unsigned int length, dlcbf *dlcbf) {
    unsigned char hash[20];
    SHA1(data, length, hash);

    dlcbf_bucket_fingerprint targets[dlcbf->d];
    get_targets(dlcbf->d, dlcbf->b, hash, targets);

    unsigned int target_table = 0;
    unsigned int target_bucket = 0;
    unsigned int lowest_count = 0;

    dlcbf_table *tables = (dlcbf_table*)dlcbf->tables;

    unsigned int i;
    for (i = 0; i < dlcbf->d; i++) {
        unsigned int bucket_i = targets[i].bucket_i;
        unsigned int count = tables[i].buckets[bucket_i].count;

        if (i == 0 || count < lowest_count) {
            lowest_count = count;
            target_table = i;
            target_bucket = bucket_i;
        }
    }

    dlcbf_bucket *bucket = &tables[target_table].buckets[target_bucket];
    FINGERPRINT fingerprint = targets[target_table].fingerprint;
    bucket->fields[lowest_count].fingerprint = fingerprint;
    bucket->fields[lowest_count].count++;

    bucket->count = lowest_count + 1;
    dlcbf->count++;
}

dlcbf_loc location_of(const unsigned char *data, const unsigned int length, dlcbf *dlcbf) {
    unsigned char hash[20];
    SHA1(data, length, hash);

    dlcbf_bucket_fingerprint targets[dlcbf->d];
    get_targets(dlcbf->d, dlcbf->b, hash, targets);

    int i;
    dlcbf_loc loc = {NULL};
    for(i = 0; i < dlcbf->d; i++) {
        const unsigned int bucket_i = targets[i].bucket_i;
        const dlcbf_bucket bucket = dlcbf->tables[i].buckets[bucket_i];
        FINGERPRINT fingerprint = targets[i].fingerprint;
        const dlcbf_field *b_loc = item_location(fingerprint, (dlcbf_bucket*)&bucket);
        if (b_loc != NULL) {
            loc.field = b_loc;
            break;
        }
    }

    return loc;
}

int member(const unsigned char *data, const unsigned int length, dlcbf *dlcbf) {
    const dlcbf_loc loc = location_of(data, length, dlcbf);
    return loc.field != NULL;
}
