struct dlht * init(unsigned int d, unsigned int b, void*(*alloc)(unsigned int));
void add(unsigned char *data, const unsigned int length, struct dlht *dlht);
int member(unsigned char *data, unsigned int length, struct dlht *dlht);

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
