struct dlht * init(unsigned int d, unsigned int b);
void add(unsigned char *data, unsigned int length, struct dlht *dlht);
int member(unsigned char *data, unsigned int length, struct dlht *dlht);
