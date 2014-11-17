#include <stdlib.h>
#include "runtime.h"

struct scm scm_print(struct scm a) {
  int i, l;
  struct scm *elt;
  if(a.tag == 1) {
    l = a.val.v->len;
    elt = a.val.v->elt;
    for(i = 0; i < l; i++) {
      if(elt[i].tag == 0) {
        putchar(elt[i].val.i);
      }
      else {
        putchar('?');
      }
    }
  }
  else {
    puts("?");
  }
}

struct scm allocate_vector(int len) {
    struct scm_vector* v;
    v = malloc(sizeof(struct scm_vector));
    v->len = len;
    v->ref = 1;
    v->elt = malloc((sizeof(struct scm) * len));
    return (struct scm){ .tag = 1, .val.v = v };
}


struct scm scm_main(struct scm env2373) {
    struct scm str2374;
    str2374 = allocate_vector(11);
    str2374.val.v->elt[0] = (struct scm){  .tag = 0, .val.i = 72 };
    str2374.val.v->elt[1] = (struct scm){ .tag = 0, .val.i = 101 };
    str2374.val.v->elt[2] = (struct scm){  .tag = 0, .val.i = 108 };
    str2374.val.v->elt[3] = (struct scm){  .tag = 0, .val.i = 108 };
    str2374.val.v->elt[4] = (struct scm){  .tag = 0, .val.i = 111 };
    str2374.val.v->elt[5] = (struct scm){  .tag = 0, .val.i = 32 };
    str2374.val.v->elt[6] = (struct scm){  .tag = 0, .val.i = 119 };
    str2374.val.v->elt[7] = (struct scm){  .tag = 0, .val.i = 111 };
    str2374.val.v->elt[8] = (struct scm){  .tag = 0, .val.i = 114 };
    str2374.val.v->elt[9] = (struct scm){  .tag = 0, .val.i = 108 };
    str2374.val.v->elt[10] = (struct scm){  .tag = 0, .val.i = 100 };
    return scm_print(str2374);
}

void main(void) {
  scm_main((struct scm){ });
}

