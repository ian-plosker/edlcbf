#define BUCKET_SIZE 8
#define FINGERPRINT unsigned long int
#define DLCBF_SIZE(d, b) (sizeof(bucket)*b*d)

typedef struct {
    unsigned int count;
    FINGERPRINT fingerprints[BUCKET_SIZE];
} dlcbf_bucket;

typedef struct {
    dlcbf_bucket *buckets;
} dlcbf_table;

typedef struct {
    unsigned int d;
    unsigned int b;
    unsigned int count;
    dlcbf_table *tables;
} dlcbf;

typedef struct {
    const FINGERPRINT *fingerprint;
} dlcbf_loc;

dlcbf *init(unsigned int d, unsigned int b);
void add(const unsigned char *data, const unsigned int length, dlcbf *dlcbf);
int member(const unsigned char *data, const unsigned int length, dlcbf *dlcbf);
