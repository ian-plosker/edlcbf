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

struct bucket {
    unsigned int count;
    unsigned int *fingerprints;
};


struct table {
    struct bucket *buckets;
};


struct dlht {
    unsigned int d;
    unsigned int b;
    unsigned int count;
    struct table *tables;
};

unsigned char *make_hash(unsigned char *data, unsigned long length) {
    unsigned char *hash = malloc(20*sizeof(char));
    return SHA1(data, length, hash);
}

struct dlht * init(unsigned int d, unsigned int b) {
    unsigned int i;

    struct table *tables = malloc(d * sizeof(*tables));

    for(i = 0; i < d; i++) {
        struct bucket *buckets = malloc(b * sizeof(buckets));
        int j;
        for (j = 0; j < b; j++) {
            unsigned int *fingerprints = malloc(8 * sizeof(int));
            struct bucket bucket;
            bucket.count = 0;
            bucket.fingerprints = fingerprints;
            buckets[j] = bucket;
        }
        struct table table;
        table.buckets = buckets;
        tables[i] = table;
    }
    struct dlht* t = malloc(sizeof *t);
    t->d = d;
    t->b = b;
    t->count = 0;
    t->tables = tables;
    return t;
}

unsigned char * get_target_buckets(unsigned int d, unsigned int b, unsigned char* hash) {
    double bits = log(b)/log(2);

    unsigned char *target_buckets = malloc(d * sizeof(char));

    int i;
    for(i = 0; i < d; i++) {
        target_buckets[i] = hash[i];
    }

    return target_buckets;
}

void add(unsigned char *data, unsigned int length, struct dlht *dlht) {
    unsigned char *hash = make_hash(data, length);
    unsigned char *target_buckets = get_target_buckets(dlht->d, dlht->b, hash);

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

    struct bucket bucket = dlht->tables[target_table].buckets[target_bucket];
    unsigned int *fingerprint = (unsigned int*)hash;

    bucket.fingerprints[bucket.count] = *fingerprint;
    bucket.count++;
    dlht->tables[target_table].buckets[target_bucket] = bucket;
    dlht->count++;
    free(hash);
}

int member(unsigned char *data, unsigned int length, struct dlht *dlht) {
    unsigned char *hash = make_hash(data, length);
    unsigned char *target_buckets = get_target_buckets(dlht->d, dlht->b, hash);
    unsigned int *fingerprint = (unsigned int*)hash;

    int i;
    for(i = 0; i < dlht->d; i++) {
        unsigned int bucket_i = target_buckets[i];
        struct bucket bucket = dlht->tables[i].buckets[bucket_i];
        unsigned int count = bucket.count;

        int j;
        for(j = 0; j < count; j++) {
            if (bucket.fingerprints[j] == *fingerprint) return 1;
        }
    }

    return 0;
}

int main() {
    struct dlht *t;
    t = init(2,256);

    printf("d: %i, b: %i\n", t->d, t->b);

    add("test", 4, t);
    add("rick", 4, t);
    add("rod", 3, t);

    printf("test: %i\n", member("test", 4, t));
    printf("poop: %i\n", member("poop", 4, t));
    printf("rod: %i\n", member("rod", 3, t));
    printf("count: %i\n", t->count);

    printf("a: %i\n", 4 >> 1);

    return 0;
}
