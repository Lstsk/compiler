#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"

// These extern declarations allow us to refer to the variables you made global
// in your generated assembly file.

extern uint64_t* start_of_stack;
extern uint64_t* end_of_stack;
extern uint64_t* start_of_heap;
extern uint64_t* end_of_heap;
extern uint64_t* heap_cursor;

/*
  The following macros allow you to use "debugf" in place of "printf" for
  formatted debugging.  For instance, you can write

      debugf("Pointer %p changed to pointer %p.\n", old, new);

  to print in the same way you would use printf.  The advantage is that,
  depending upon which of the two macros you are using, the debugf call either
  turns into a printf call or is erased entirely.  Use debugf to print-debug
  your garbage collector and then disable printing when you need to perform your
  unit tests.
*/

// This macro disables all debugf statements.  (They become no-ops.)
#define debugf(fmt, ...) ;


// This macro enables all debugf statements.  (They become printf statements.)
// #define debugf(fmt, ...) printf(fmt, ##__VA_ARGS__); fflush(stdout)


/**
 * This helper function will show the entire contents of your heap.  This output
 * can get very large if your heap is too big!
 */
void dump_heap() {
  debugf("HEAP:\n");
  int c = 0;
  for (uint64_t* p = (uint64_t*)((uint64_t)start_of_heap & 0xFFFFFFFFFFFFFFFC);
       p < end_of_heap; p += 1) {
    if (c==0) {
      debugf("%016"PRIx64":", p);
    }
    if (p >= start_of_heap) {
      debugf("    %016"PRIx64, *p);
    } else {
      debugf("            ");
    }
    c++;
    if (c==4) {
      debugf("\n");
      c=0;
    }
  }
  if (c!=0) {
    debugf("\n");
  }
}


bool is_closure(uint64_t* obj) {
  return (obj[0] & 0x8000000000000000) != 0;
}

bool is_pointer(uint64_t value) {
  return (value & 0x3) == 0x1;
}

size_t get_size_of_object(uint64_t* obj) {
  if (is_closure(obj)) {
    uint64_t tagged = obj[0];
    size_t applied = tagged & 0x7FFFFFFFFFFFFFFF;
    // applied count + GC + param count + code addr 
    return 4 + applied;
  } else {
    uint64_t size = obj[0];
    //  size word + GC word for a tuple
    return size + 2;
  }
}

// Mark phase: recursively mark reachable objects
void mark_object(uint64_t* obj) {
  obj[1] = 1;  // mark GC word
  size_t words = get_size_of_object(obj);
  size_t start = is_closure(obj) ? 4 : 2;
  for (size_t i = start; i < words; i++) {
    uint64_t value = obj[i];
    if (is_pointer(value)) {
      uint64_t* raw = (uint64_t*)(value & ~0x3);
      if (raw >= start_of_heap && raw < heap_cursor && raw[1] == 0) {
        mark_object(raw);
      }
    }
  }
}

void mark_phase(void) {
  // loop though stack mark objects
  for (uint64_t* slot = end_of_stack; slot < start_of_stack; slot++) {
    uint64_t value = *slot;
    if (is_pointer(value)) {
      uint64_t* raw = (uint64_t*)(value & ~0x3);
      if (raw >= start_of_heap && raw < heap_cursor && raw[1] == 0) {
        mark_object(raw);
      }
    }
  }
}

// Forward phase: assign new addresses to live objects; return new byte-cursor
uint64_t* forward_phase(void) {
  uint64_t* scan = (uint64_t*)start_of_heap;
  uint64_t* dest = (uint64_t*)start_of_heap;
  while (scan < (uint64_t*)heap_cursor) {
    uint64_t* obj = (uint64_t*)scan;
    size_t words = get_size_of_object(obj);
    if (obj[1] != 0) {
      // record new location in GC word
      obj[1] = (uint64_t)dest;
      dest += words;
    }
    scan += words;
  }
  return dest;
}

// Update phase: fix heap pointers on stack and within heap
void update_phase(void) {
  // update stack pointers
  for (uint64_t* slot = end_of_stack; slot < start_of_stack; slot++) {
    uint64_t value = *slot;
    if (is_pointer(value)) {
      uint64_t* raw = (uint64_t*)(value & ~0x3);
      if (raw >= start_of_heap && raw < heap_cursor && raw[1] != 0) {
        *slot = raw[1] | 0x1;
      }
    }
  }
  // update pointers inside heap objects
  uint64_t* heap_end = (uint64_t*)heap_cursor;
  for (uint64_t* scan = (uint64_t*)start_of_heap; scan < heap_end; ) {
    uint64_t* obj = (uint64_t*)scan;
    size_t words = get_size_of_object(obj);
    size_t start = is_closure(obj) ? 4 : 2;
    for (size_t i = start; i < words; i++) {
      uint64_t value = obj[i];
      if (is_pointer(value)) {
        uint64_t* raw = (uint64_t*)(value & ~0x3);
        if (raw >= start_of_heap && raw < heap_cursor && raw[1] != 0) {
          obj[i] = raw[1] | 0x1;
        }
      }
    }
    scan += words;
  }
}

// Compact phase: move each live object to its new location
void compact_phase(void) {
  uint64_t* start = (uint64_t*)start_of_heap;
  while (start < heap_cursor) {
    uint64_t* obj = (uint64_t*)start;
    size_t words = get_size_of_object(obj);
    if (obj[1] != 0) {
      uint64_t* dest = (uint64_t*)(obj[1]);
      memmove(dest, start, sizeof(uint64_t)*words);
    }
    start += words;
  }
}

// Unmark phase: clear GC words on live objects
void unmark_phase(void) {
  uint64_t* scan = (uint64_t*)start_of_heap;
  uint64_t* old_end = (uint64_t*)heap_cursor;
  while (scan < old_end) {
    uint64_t* obj = (uint64_t*)scan;
    obj[1] = 0;
    size_t words = get_size_of_object(obj);
    scan += words;
  }
}

void gc(int64_t desired_free) {
  // Phase 1: Mark

  debugf("stack_low = %p, stack_high = %p\n",
    (void*)end_of_stack, (void*)start_of_stack);
  mark_phase();
  debugf("After mark phase: heap_cursor= %p\n", (void*)heap_cursor);
  dump_heap();

  // Phase 2: Forward
  uint64_t* old_cursor = (uint64_t*)heap_cursor;
  uint64_t* new_cursor = forward_phase();
  debugf("After forward phase: new_cursor= %p\n", (void*)new_cursor);
  dump_heap();

  // Phase 3: Update
  update_phase();
  debugf("After update phase\n");
  dump_heap();

  // Phase 4: Compact
  compact_phase();
  debugf("After compact phase\n");
  dump_heap();

  // Phase 5: Unmark
  heap_cursor = (uint64_t*)new_cursor;
  unmark_phase();
  debugf("After unmark phase\n");
  dump_heap();


  for (uint64_t* p = heap_cursor; p < end_of_heap; p++) {
    *p = 0xbadbadffffbadbad;
  }

  dump_heap();


  int64_t free_bytes = (int64_t)((uint8_t*)end_of_heap - (uint8_t*)new_cursor);
  if (free_bytes < desired_free) {
    stopWithError(7);
  }
}
