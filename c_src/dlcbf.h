typedef struct {
    unsigned char count;
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
} dlcbf;

typedef struct {
    const unsigned int *fingerprint;
} dlcbf_loc;

dlcbf * init(unsigned int d, unsigned int b);
void add(const unsigned char *data, const unsigned int length, dlcbf *dlcbf);
int member(const unsigned char *data, const unsigned int length, dlcbf *dlcbf);
