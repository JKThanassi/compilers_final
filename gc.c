#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "snakeString.h"

typedef uint64_t SNAKEVAL;

void printHelp(FILE *out, SNAKEVAL val);
extern uint64_t NUM_TAG_MASK;
extern uint64_t CLOSURE_TAG_MASK;
extern uint64_t TUPLE_TAG_MASK;
extern uint64_t CLOSURE_TAG;
extern uint64_t TUPLE_TAG;
extern uint64_t FORWARDING_TAG;
extern uint64_t NIL;
extern uint64_t tupleCounter;
extern uint64_t *STACK_BOTTOM;
extern uint64_t *FROM_S;
extern uint64_t *FROM_E;
extern uint64_t *TO_S;
extern uint64_t *TO_E;

void naive_print_heap(uint64_t *heap, uint64_t *heap_end) {
  printf("In naive_print_heap from %p to %p\n", heap, heap_end);
  for (uint64_t i = 0; i < (uint64_t)(heap_end - heap); i += 1) {
    printf("  %ld/%p: %p (%ld)\n", i, (heap + i), (uint64_t *)(*(heap + i)),
           *(heap + i));
  }
}

// Implement the functions below

void smarter_print_heap(uint64_t *from_start, uint64_t *from_end,
                        uint64_t *to_start, uint64_t *to_end) {
  // Print out the entire heap (both semispaces), and
  // try to print values readably when possible
}

/*
   Copies a Garter value from the given address to the new heap,
   but only if the value is heap-allocated and needs copying.

    Arguments:
    garter_val_addr: the *address* of some Garter value, which contains a Garter
   value, i.e. a tagged word. It may or may not be a pointer to a heap-allocated
   value... heap_top: the location at which to begin copying, if any copying is
   needed

    Return value:
    The new top of the heap, at which to continue allocations

    Side effects:
    If the data needed to be copied, then this replaces the value at its old
   location with a forwarding pointer to its new location
*/
uint64_t *copy_if_needed(uint64_t *garter_val_addr, uint64_t *heap_top) {
  uint64_t garter_val = *garter_val_addr;
  // check if heap val
  uint64_t masked_garter_val = garter_val & TUPLE_TAG_MASK;
  if (masked_garter_val == TUPLE_TAG || masked_garter_val == CLOSURE_TAG) {
    uint64_t *heap_thing_addr =
        (uint64_t *)(garter_val - (masked_garter_val == TUPLE_TAG
                                       ? TUPLE_TAG
                                       : CLOSURE_TAG));

    // if heap_thing is a fwd pointer, forward the value
    if ((*heap_thing_addr & TUPLE_TAG_MASK) == FORWARDING_TAG) {
      uint64_t untagged_fwd = *heap_thing_addr - FORWARDING_TAG;
      uint64_t tagged_fwd =
          untagged_fwd +
          (masked_garter_val == TUPLE_TAG ? TUPLE_TAG : CLOSURE_TAG);
      *garter_val_addr = tagged_fwd;
      return heap_top;
    }

    uint64_t len = 0;
    if (masked_garter_val == TUPLE_TAG) {
      // tuple
      len = *heap_thing_addr + (*heap_thing_addr % 2 == 0
                                    ? 2
                                    : 1);  // account for len val and padding

      for (int i = 0; i < len; i++) {
        heap_top[i] = heap_thing_addr[i];
      }

      // step 3
      *garter_val_addr = (uint64_t)heap_top;
      *heap_thing_addr = ((uint64_t)heap_top + FORWARDING_TAG);

      heap_top += len;
    } else if (masked_garter_val == CLOSURE_TAG) {
      uint64_t num_vars =
          heap_thing_addr[2];  // number of closed over variables is the third
                               // word in the closure tuple
      len = 3 + num_vars + (num_vars % 2 == 0 ? 1 : 0);

      for (int i = 1; i < len; i++) {
        heap_top[i] = heap_thing_addr[i];
      }

      // step 3
      *garter_val_addr = (uint64_t)heap_top;
      *heap_thing_addr = ((uint64_t)heap_top + FORWARDING_TAG);
    }

    uint64_t *pre_inc_heap_top = heap_top;
    heap_top += len;

    // now do the dfs shit
    for (int i = 0; i < len; i++) {
      heap_top = copy_if_needed(pre_inc_heap_top + i, heap_top);
      // heap_thing_addr[i] = *heap_top; // I think we need to set the value
      // here ?
    }
  }
  // } else if (masked_garter_val == SNAKE_STRING_TAG) {
  //   snakeStringComponents *strC = ptrToComponents(garter_val);
  //   *heap_top = strC->len;
  //   int wordsOccupied = strLenToNumWords(strC->len);
  //   memset(heap_top + 1, 0, (wordsOccupied - 1) * 8);
  //   strncpy(heap_top + 1, strC->contents, strC->len);
  //   heap_top += wordsOccupied;
  //   return heap_top;
  // }

  return heap_top;
}

/*
   Implements Cheney's garbage collection algorithm.

    Arguments:
    bottom_frame: the base pointer of our_code_starts_here, i.e. the bottommost
   Garter frame top_frame: the base pointer of the topmost Garter stack frame
    top_stack: the current stack pointer of the topmost Garter stack frame
    from_start and from_end: bookend the from-space of memory that is being
   compacted to_start: the beginning of the to-space of memory

    Returns:
    The new location within to_start at which to allocate new data
*/
uint64_t *gc(uint64_t *bottom_frame, uint64_t *top_frame, uint64_t *top_stack,
             uint64_t *from_start, uint64_t *from_end, uint64_t *to_start) {
  uint64_t *old_top_frame = top_frame;
  do {
    for (uint64_t *cur_word = top_stack /* maybe need a +1 here? */;
         cur_word < top_frame; cur_word++) {
      to_start = copy_if_needed(cur_word, to_start);
    }
    /* Shift to next stack frame:
     * [top_frame] points to the saved RBP, which is the RBP of the next stack
     * frame, [top_frame + 8] is the return address, and [top_frame + 16] is
     * therefore the next frame's stack-top
     */
    top_stack = top_frame + 2;
    old_top_frame = top_frame;
    top_frame = (uint64_t *)(*top_frame);
  } while (old_top_frame < bottom_frame);  // Use the old stack frame to decide
                                           // if there's more GC'ing to do

  // after copying and GC'ing all the stack frames, return the new allocation
  // starting point
  return to_start;
}
