#include <stdlib.h>
#include <string.h>
#include "chan.h"

#include <stdio.h>

typedef struct chan {
  int cur_bytes;
  int max_bytes;
  pthread_mutex_t mutex;
  pthread_cond_t cond;
  void *buf;
  size_t readoff;
  size_t writeoff;
  chan_state_t state;
  int last_read_ok;
} *chan_t;

chan_t chan_new(size_t nbytes) {
  chan_t c = malloc(sizeof(struct chan));
  if(nbytes <= 0) {
    nbytes = 1;
  }
  c->cur_bytes = 0;
  c->max_bytes = nbytes;
  c->buf = malloc(nbytes);
  c->readoff = c->writeoff = 0;
  c->state = CHAN_OPEN;
  c->last_read_ok = 1;
  pthread_mutex_init(&c->mutex, NULL);
  pthread_cond_init(&c->cond, NULL);
  return c;
}

void chan_read(chan_t c, size_t nbytes, void *buf) {
  pthread_mutex_lock(&c->mutex);
  while(c->cur_bytes < nbytes && c->state == CHAN_OPEN) {
    pthread_cond_wait(&c->cond, &c->mutex);
  }
  if(c->state == CHAN_CLOSED && c->cur_bytes < nbytes) {
    c->last_read_ok = 0;
  } else {
    memcpy(buf, c->buf+c->readoff, nbytes);
    c->readoff = (c->readoff + nbytes) % c->max_bytes;
    c->cur_bytes -= nbytes;
    pthread_cond_signal(&c->cond);
  }
  pthread_mutex_unlock(&c->mutex);
}

int chan_write(chan_t c, size_t nbytes, void *buf) {
  if(c->state == CHAN_CLOSED) {
    return 0;
  } else {
    pthread_mutex_lock(&c->mutex);
    while(c->cur_bytes+nbytes > c->max_bytes) {
      pthread_cond_wait(&c->cond, &c->mutex);
    }
    memcpy(c->buf+c->writeoff, buf, nbytes);
    c->writeoff = (c->writeoff + nbytes) % c->max_bytes;
    c->cur_bytes += nbytes;
    pthread_cond_signal(&c->cond);
    pthread_mutex_unlock(&c->mutex);
    return 1;
  }
}

void chan_close(chan_t c) {
  c->state = CHAN_CLOSED;
  pthread_cond_signal(&c->cond);
}

int chan_last_read_ok(chan_t c) {return c->last_read_ok;}
