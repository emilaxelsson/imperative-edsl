#include <stdlib.h>
#include <string.h>
#include "chan.h"

typedef struct chan {
  int elem_size;
  int max_elems;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  void *elems;
  int readoff;
  int writeoff;
} *chan_t;

chan_t chan_new(int elem_size, int max_elems) {
  chan_t c = malloc(sizeof(struct chan));
  c->elem_size = elem_size;
  c->max_elems = max_elems;
  c->elems = malloc(elem_size*max_elems);
  c->readoff = c->writeoff = 0;
  pthread_mutex_init(&c->mutex, NULL);
  pthread_cond_init(&c->cond, NULL);
}

void chan_read(chan_t c, void *buf) {
  pthread_mutex_lock(&c->mutex);
  while(c->readoff == c->writeoff) {
    pthread_cond_wait(&c->cond, &c->mutex);
  }
  memcpy(buf, c->elems+c->readoff, c->elem_size);
  c->readoff = (c->readoff + c->elem_size) % (c->elem_size * c->max_elems);
  pthread_cond_signal(&c->cond);
  pthread_mutex_unlock(&c->mutex);
}

void chan_write(chan_t c, void *buf) {
  pthread_mutex_lock(&c->mutex);
  while(c->readoff == c->writeoff) {
    pthread_cond_wait(&c->cond, &c->mutex);
  }
  memcpy(c->elems+c->writeoff, buf, c->elem_size);
  c->writeoff = (c->writeoff + c->elem_size) % (c->elem_size * c->max_elems);
  pthread_cond_signal(&c->cond);
  pthread_mutex_unlock(&c->mutex);
}
