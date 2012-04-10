/*
** -------------------------------------------------------------------
**
** dlcbf.c: d-left counting bloom filter implementation
**
** Copyright (c) 2012 Basho Technologies, Inc. All Rights Reserved.
**
** This file is provided to you under the Apache License,
** Version 2.0 (the "License"); you may not use this file
** except in compliance with the License.  You may obtain
** a copy of the License at
**
**   http:**www.apache.org/licenses/LICENSE-2.0
**
** Unless required by applicable law or agreed to in writing,
** software distributed under the License is distributed on an
** "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
** KIND, either express or implied.  See the License for the
** specific language governing permissions and limitations
** under the License.
**
** -------------------------------------------------------------------
*/

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
dlcbf_init(unsigned d, unsigned b)
{
    Dlcbf* dlcbf = malloc(sizeof(Dlcbf));
    *((unsigned*)&dlcbf->d) = d;
    *((unsigned*)&dlcbf->b) = b;
    *((unsigned*)&dlcbf->bits) = rint(log(b)/log(2));

    DlcbfTable* table = dlcbf->tables = malloc(sizeof(DlcbfTable)*d);
    DlcbfTable* tables_end = table + d;
    const size_t numbuckets = sizeof(DlcbfBucket)*b;
    while (table != tables_end) {
        table->buckets = malloc(numbuckets);
        memset(table++->buckets, 0, numbuckets);
    }
    return dlcbf;
}

void
dlcbf_destroy(Dlcbf* dlcbf)
{
    DlcbfTable* table = dlcbf->tables;
    DlcbfTable* tables_end = table + dlcbf->d;
    while (table != tables_end) {
        free(table++->buckets);
    }
    free(dlcbf->tables);
    free(dlcbf);
}

static unsigned
get_bits(const unsigned char* input, unsigned numbits, unsigned pos)
{
    unsigned value = 0;
    unsigned posbits = pos % 8;
    unsigned i = pos/8, start = posbits > numbits ? numbits : posbits;

    if (start != 0) {
        value = input[i++] & (0xFF >> (8 - start));
        numbits -= start;
    }
    while (numbits >= 8) {
        value = (value << 8) | input[i++];
        numbits -= 8;
    }
    if (numbits != 0) {
        unsigned char last = (input[i] >> (8 - numbits));
        value = (value << numbits) | last;
    }
    return value;
}

static DlcbfBucketFingerprint*
get_targets(unsigned d, unsigned bits, const unsigned char* hash, DlcbfBucketFingerprint* targets)
{
    unsigned pos;
    DlcbfBucketFingerprint* targets_end = targets + d;
    for (pos = 0; targets != targets_end; ++targets, pos += FINGERPRINT_BITSIZE) {
        targets->bucket_i = get_bits(hash, bits, pos);
        targets->fingerprint = get_bits(hash, FINGERPRINT_BITSIZE, pos += bits);
    }
    return targets;
}

static DlcbfField*
item_location(const Fingerprint fingerprint, DlcbfBucket* bucket)
{
    DlcbfField f;
    DlcbfField* field = bucket->fields;
    DlcbfField* fields_end = field + bucket->count;
    for (f.f.fingerprint = fingerprint; field != fields_end; ++field) {
        if (f.f.fingerprint == field->f.fingerprint) {
            return field;
        }
    }
    return NULL;
}

void
dlcbf_add(const unsigned char* data, unsigned length, Dlcbf* dlcbf)
{
    unsigned char hash[HASH_BYTES];
    SHA1(data, length, hash);

    DlcbfBucketFingerprint targets[dlcbf->d];
    get_targets(dlcbf->d, dlcbf->bits, hash, targets);

    DlcbfTable* table = dlcbf->tables;
    DlcbfTable* tables_end = table + dlcbf->d;

    DlcbfBucketFingerprint* target = targets;
    unsigned lowest_count = table->buckets[target->bucket_i].count;
    DlcbfBucket* bucket = &table->buckets[target->bucket_i];
    Fingerprint fingerprint = target->fingerprint;

    for (++target, ++table; table != tables_end; ++target, ++table) {
        unsigned count = table->buckets[target->bucket_i].count;
        if (count < lowest_count) {
            lowest_count = count;
            bucket = &table->buckets[target->bucket_i];
            fingerprint = target->fingerprint;
        }
    }

    bucket->fields[lowest_count].f.fingerprint = fingerprint;
    ++bucket->fields[lowest_count].f.count;
    ++bucket->count;
}

static DlcbfLoc
location_of(const unsigned char* data, unsigned length, Dlcbf* dlcbf, DlcbfBucket** bucket)
{
    unsigned char hash[HASH_BYTES];
    SHA1(data, length, hash);

    DlcbfBucketFingerprint targets[dlcbf->d];
    get_targets(dlcbf->d, dlcbf->bits, hash, targets);

    DlcbfBucketFingerprint* target = targets;
    DlcbfBucketFingerprint* targets_end = target + dlcbf->d;
    DlcbfTable* table = dlcbf->tables;
    DlcbfLoc loc = {NULL};
    while (target != targets_end) {
        *bucket = &table++->buckets[target->bucket_i];
        if ((loc.field = item_location(target++->fingerprint, *bucket)) != NULL) {
            break;
        }
    }
    if (loc.field == NULL) {
        *bucket = NULL;
    }
    return loc;
}

int
dlcbf_member(const unsigned char* data, unsigned length, Dlcbf* dlcbf)
{
    DlcbfBucket* bucket;
    const DlcbfLoc loc = location_of(data, length, dlcbf, &bucket);
    return loc.field != NULL;
}

void
dlcbf_delete(const unsigned char* data, unsigned length, Dlcbf* dlcbf)
{
    unsigned char hash[HASH_BYTES];
    SHA1(data, length, hash);
    DlcbfBucket* bucket;
    DlcbfLoc loc = location_of(data, length, dlcbf, &bucket);
    if (loc.field != NULL) {
        --bucket->count;
        --(loc.field->f.count);
    }
}
