#ifndef __CHAN_H__
#define __CHAN_H__
#include <pthread.h>

typedef struct chan *chan_t;

chan_t chan_new(int elem_size, int max_elems);
void chan_read(chan_t c, void *buf);
void chan_write(chan_t c, void *buf);

#endif /* __CHAN_H__ */
