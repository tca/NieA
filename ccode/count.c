#include <stdlib.h>
#include <stdio.h>
#include "runtime.h"

struct scm begin(struct scm x, struct scm y) {
    return y;
}

struct scm scm_plus(struct scm a, struct scm b) {
  return (struct scm){ .tag = 0, .val.i = a.val.i+b.val.i};
}

struct scm scm_print(struct scm i) {
  printf("%d\n", i.val.i);
  return i;
}

struct scm count(struct scm i, struct scm n) {
  if((i.val.i == n.val.i))
    { return ((struct scm){ .tag = 0, .val.i = 0 }); }
    else { return (begin(scm_print(i), count((scm_plus(i,(struct scm){ .tag = 0, .val.i = 1 })), n))); }
}

struct scm scm_main() {
    return count((struct scm){ .tag = 0, .val.i = 0 }, (struct scm){ .tag = 0, .val.i = 100 });
}

void main(void) {
  scm_main((struct scm){ });
}

