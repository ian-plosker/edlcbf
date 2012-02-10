#define BUCKET_SIZE 8
#define FINGERPRINT unsigned char 
#define DLCBF_SIZE(d, b) (sizeof(bucket)*b*d)

typedef struct {
    unsigned char count;
    FINGERPRINT fingerprint;
} dlcbf_field;

typedef struct {
    unsigned int bucket_i;
    FINGERPRINT fingerprint;
} dlcbf_bucket_fingerprint;

typedef struct {
    unsigned int count;
    dlcbf_field fields[BUCKET_SIZE];
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
    const dlcbf_field *field;
} dlcbf_loc;

dlcbf *init(unsigned int d, unsigned int b);
void add(const unsigned char *data, const unsigned int length, dlcbf *dlcbf);
int member(const unsigned char *data, const unsigned int length, dlcbf *dlcbf);
void dstry(dlcbf *dlcbf);
