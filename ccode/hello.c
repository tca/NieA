#include "runtime.h"

struct scm scm_main(struct scm env2341) {
    struct scm str2342;
    str2342 = allocate_vector(11);
    str2342.val.v.elt[0] = (struct scm){ .ref = 1, .tag = 0, .val.i = 72 };
    str2342.val.v.elt[1] = (struct scm){ .ref = 1, .tag = 0, .val.i = 101};
    str2342.val.v.elt[2] = (struct scm){ .ref = 1, .tag = 0, .val.i = 108};
    str2342.val.v.elt[3] = (struct scm){ .ref = 1, .tag = 0, .val.i = 108};
    str2342.val.v.elt[4] = (struct scm){ .ref = 1, .tag = 0, .val.i = 111};
    str2342.val.v.elt[5] = (struct scm){ .ref = 1, .tag = 0, .val.i = 32};
    str2342.val.v.elt[6] = (struct scm){ .ref = 1, .tag = 0, .val.i = 119};
    str2342.val.v.elt[7] = (struct scm){ .ref = 1, .tag = 0, .val.i = 111};
    str2342.val.v.elt[8] = (struct scm){ .ref = 1, .tag = 0, .val.i = 114};
    str2342.val.v.elt[9] = (struct scm){ .ref = 1, .tag = 0, .val.i = 108};
    str2342.val.v.elt[10] = (struct scm){ .ref = 1, .tag = 0, .val.i = 100};
    return scm_print(str2342);
}

void main(void) {
  scm_main((struct scm){ });
}

