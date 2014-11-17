#include "runtime.h"
#include "builtins.h"

struct scm scm_main(struct scm env2477) {
    struct scm str2478;
    str2478 = allocate_vector(11);
    str2478.val.v->elt[0] = (struct scm){ .tag = 0, .val.i = 72 };
    str2478.val.v->elt[1] = (struct scm){ .tag = 0, .val.i = 101 };
    str2478.val.v->elt[2] = (struct scm){ .tag = 0, .val.i = 108 };
    str2478.val.v->elt[3] = (struct scm){ .tag = 0, .val.i = 108 };
    str2478.val.v->elt[4] = (struct scm){ .tag = 0, .val.i = 111 };
    str2478.val.v->elt[5] = (struct scm){ .tag = 0, .val.i = 32 };
    str2478.val.v->elt[6] = (struct scm){ .tag = 0, .val.i = 119 };
    str2478.val.v->elt[7] = (struct scm){ .tag = 0, .val.i = 111 };
    str2478.val.v->elt[8] = (struct scm){ .tag = 0, .val.i = 114 };
    str2478.val.v->elt[9] = (struct scm){ .tag = 0, .val.i = 108 };
    str2478.val.v->elt[10] = (struct scm){ .tag = 0, .val.i = 100 };
    return scm_print(str2478);
}

void main(void) {
  scm_main((struct scm){});
}
