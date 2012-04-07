#include <stdint.h>

#define BUCKET_SIZE 8
#define FINGERPRINT_BITSIZE 10

typedef uint16_t Counter;
typedef uint16_t Fingerprint;

#define COUNTER_BITSIZE (sizeof(Counter)*8 - FINGERPRINT_BITSIZE)

typedef union {
    uint16_t all;
    struct {
        Counter     count:       COUNTER_BITSIZE;
        Fingerprint fingerprint: FINGERPRINT_BITSIZE;
    } f;
} DlcbfField;

typedef struct {
    unsigned int bucket_i;
    Fingerprint fingerprint;
} DlcbfBucketFingerprint;

typedef struct {
    DlcbfField fields[BUCKET_SIZE];
    unsigned int count;
} DlcbfBucket;

typedef struct {
    DlcbfBucket *buckets;
} DlcbfTable;

typedef struct {
    unsigned long long count;
    DlcbfTable* tables;
    const unsigned int d;
    const unsigned int b;
} Dlcbf;

typedef struct {
    DlcbfField* field;
} DlcbfLoc;

extern Dlcbf* dlcbf_init(unsigned int d, unsigned int b);
extern void dlcbf_add(const unsigned char* data, const unsigned int length, Dlcbf* dlcbf);
extern void dlcbf_delete(const unsigned char* data, const unsigned int length, Dlcbf* dlcbf);
extern int dlcbf_member(const unsigned char* data, const unsigned int length, Dlcbf* dlcbf);
extern void dlcbf_destroy(Dlcbf* dlcbf);
