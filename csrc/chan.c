#include <stdlib.h>
#include <string.h>
#include "chan.h"

#include <stdio.h>

typedef struct chan {
  int elem_size;
  int max_elems;
  int cur_elems;
  int nbytes;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  void *elems;
  int readoff;
  int writeoff;
} *chan_t;

chan_t chan_new(int elem_size, int max_elems) {
  chan_t c = malloc(sizeof(struct chan));
  if(max_elems <= 0) {
    max_elems = 1;
  }
  c->elem_size = elem_size;
  c->max_elems = max_elems;
  c->cur_elems = 0;
  c->nbytes = elem_size * max_elems;
  c->elems = malloc(c->nbytes);
  c->readoff = c->writeoff = 0;
  pthread_mutex_init(&c->mutex, NULL);
  pthread_cond_init(&c->cond, NULL);
  return c;
}

void chan_read(chan_t c, void *buf) {
  pthread_mutex_lock(&c->mutex);
  while(c->cur_elems == 0) {
    pthread_cond_wait(&c->cond, &c->mutex);
  }
  memcpy(buf, c->elems+c->readoff, c->elem_size);
  c->readoff = (c->readoff + c->elem_size) % c->nbytes;
  --c->cur_elems;
  pthread_cond_signal(&c->cond);
  pthread_mutex_unlock(&c->mutex);
}

void chan_write(chan_t c, void *buf) {
  int nextwrite;
  pthread_mutex_lock(&c->mutex);
  while(c->cur_elems == c->max_elems) {
    pthread_cond_wait(&c->cond, &c->mutex);
  }
  memcpy(c->elems+c->writeoff, buf, c->elem_size);
  c->writeoff = (c->writeoff + c->elem_size) % c->nbytes;
  ++c->cur_elems;
  pthread_cond_signal(&c->cond);
  pthread_mutex_unlock(&c->mutex);
}
