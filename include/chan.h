#ifndef __CHAN_H__
#define __CHAN_H__
#include <pthread.h>

#define CHAN_OPEN 1
#define CHAN_CLOSED 0

typedef int chan_state_t;

/* Bounded, blocking channels.
   A channel may be in either of two states: open or closed. A closed channel
   will not admit any new elements, and reading from a closed, empty channel
   will return immediately rather than block, as the reading thread would
   otherwise be blocked indefinitely due to no new elements being able to
   enter the channel.
 */
typedef struct chan *chan_t;

/* Create a new channel with space for max_elems elements, each elem_size
   bytes in size. Elements are not cache aligned.
 */
chan_t chan_new(int elem_size, int max_elems);

/* Put a channel into the closed state. A closed channel can not be reopened.
 */
void chan_close(chan_t c);

/* Read an element from a channel into the given buffer.
   In the open state, attempting to read from an empty channel will block.
   Upon resumption, chan_read will return CHAN_OPEN, to indicate that the
   channel was open when the read was initiated.

   In the closed state, reading from an empty channel will *not* block, but
   immediately return CHAN_CLOSED, indicating that the channel has been closed,
   and that no new data will be written to it. Reading from a non-empty channel
   will return CHAN_OPEN until the channel becomes empty.
*/
chan_state_t chan_read(chan_t c, void *buf);

/* Write an element from the given buffer into a channel.
   Writing to a full channel in the open state will block until the channel is
   no longer full, and chan_write will return CHAN_OPEN upon resumption.

   Writing to a channel in the closed state will always be a non-blocking no-op
   which returns CHAN_CLOSED.
*/
chan_state_t chan_write(chan_t c, void *buf);

#endif /* __CHAN_H__ */
