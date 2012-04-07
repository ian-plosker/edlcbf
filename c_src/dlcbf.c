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

#define HASH_BYTES 20


Dlcbf*
dlcbf_init(unsigned int d, unsigned int b)
{
    Dlcbf* dlcbf = malloc(sizeof(Dlcbf));
    *((unsigned int*)&dlcbf->d) = d;
    *((unsigned int*)&dlcbf->b) = b;
    dlcbf->count = 0;

    DlcbfTable* table = dlcbf->tables = malloc(sizeof(DlcbfTable)*d);
    DlcbfTable* tables_end;
    for (tables_end = table + d; table != tables_end; ++ table) {
        table->buckets = malloc(sizeof(DlcbfBucket)*b);
        memset(table->buckets, 0, sizeof(DlcbfBucket)*b);
    }
    return dlcbf;
}

void
dlcbf_destroy(Dlcbf* dlcbf)
{
    DlcbfTable* table = dlcbf->tables;
    DlcbfTable* tables_end = table + dlcbf->d;
    for (; table != tables_end; ++table) {
        free(table->buckets);
    }
    free(dlcbf->tables);
    free(dlcbf);
}

static unsigned int
get_bits(const unsigned char* input, unsigned int numbits, unsigned int pos)
{
    unsigned int value = 0;

    int bitsleft;
    for (bitsleft = numbits; bitsleft > 0; bitsleft -= 8) {
        const unsigned int getbits = bitsleft >= 8 ? 8 : bitsleft;
        value <<= getbits;
        const unsigned int curpos = (pos + (numbits - bitsleft));
        value += (((input[curpos / 8] >> curpos % 8)
            + (input[curpos / 8 + 1] << (8 - curpos % 8)))
            >> (8 - getbits))
            & ((1<<getbits)-1);
    }

    return value;
}

static DlcbfBucketFingerprint*
get_targets(unsigned int d, unsigned int b, const unsigned char* hash, DlcbfBucketFingerprint* targets)
{
    const int bits = rint(log(b)/log(2));
    unsigned int i;
    DlcbfBucketFingerprint* targets_end = targets + d;
    for (i = 0; targets != targets_end; ++targets, ++i) {
        targets->bucket_i = get_bits(hash, bits, i*bits);
        targets->fingerprint = get_bits(hash, FINGERPRINT_BITSIZE, (i+1)*bits);
    }
    return targets;
}

#define MASK ((1<<FINGERPRINT_BITSIZE)-1)

static DlcbfField*
item_location(const Fingerprint fingerprint, DlcbfBucket* bucket)
{
    DlcbfField* field = bucket->fields;
    DlcbfField* fields_end = field + bucket->count;
    while (field != fields_end) {
        Fingerprint fingerprint_i = field->f.fingerprint;
        if ((fingerprint & MASK) == (fingerprint_i & MASK)) {
            return field;
        }
        ++field;
    }
    return NULL;
}

void
dlcbf_add(const unsigned char* data, unsigned int length, Dlcbf* dlcbf)
{
    unsigned char hash[HASH_BYTES];
    SHA1(data, length, hash);

    DlcbfBucketFingerprint targets[dlcbf->d];
    get_targets(dlcbf->d, dlcbf->b, hash, targets);

    DlcbfTable* table = dlcbf->tables;
    DlcbfTable* tables_end = table + dlcbf->d;

    DlcbfBucketFingerprint* target = targets;
    unsigned int lowest_count = table->buckets[target->bucket_i].count;
    DlcbfBucket* bucket = &table->buckets[target->bucket_i];
    Fingerprint fingerprint = target->fingerprint;

    for (++target, ++table; table != tables_end; ++target, ++table) {
        unsigned int count = table->buckets[target->bucket_i].count;
        if (count < lowest_count) {
            lowest_count = count;
            bucket = &table->buckets[target->bucket_i];
            fingerprint = target->fingerprint;
        }
    }

    bucket->fields[lowest_count].f.fingerprint = fingerprint;
    ++bucket->fields[lowest_count].f.count;
    ++bucket->count;
    ++dlcbf->count;
}

static DlcbfLoc
location_of(const unsigned char* data, unsigned int length, Dlcbf* dlcbf, DlcbfBucket** bucket)
{
    unsigned char hash[HASH_BYTES];
    SHA1(data, length, hash);

    DlcbfBucketFingerprint targets[dlcbf->d];
    get_targets(dlcbf->d, dlcbf->b, hash, targets);

    DlcbfBucketFingerprint* target = targets;
    DlcbfBucketFingerprint* targets_end = target + dlcbf->d;
    DlcbfTable* table = dlcbf->tables;
    DlcbfLoc loc = {NULL};
    while (target != targets_end) {
        *bucket = &table++->buckets[target->bucket_i];
        DlcbfField* b_loc = item_location(target++->fingerprint, *bucket);
        if (b_loc != NULL) {
            loc.field = b_loc;
            break;
        }
    }
    if (loc.field == NULL) {
        *bucket = NULL;
    }
    return loc;
}

int
dlcbf_member(const unsigned char* data, unsigned int length, Dlcbf* dlcbf)
{
    DlcbfBucket* bucket;
    const DlcbfLoc loc = location_of(data, length, dlcbf, &bucket);
    return loc.field != NULL;
}

/*
** TODO: this delete doesn't work properly since deleting a false positive results in a false negative
*/
void
dlcbf_delete(const unsigned char* data, unsigned int length, Dlcbf* dlcbf)
{
    unsigned char hash[HASH_BYTES];
    SHA1(data, length, hash);
    DlcbfBucket* bucket;
    DlcbfLoc loc = location_of(data, length, dlcbf, &bucket);
    if (loc.field != NULL) {
        --dlcbf->count;
        --bucket->count;
        --(loc.field->f.count);
    }
}
