#include "stdlib.h"
#include "scm_fptr.h"
struct scm_vector {
    int len;
    struct scm* elt;
};
union scm_value {
    int i;
    struct scm_vector v;
    scm_fptr f;
};
struct scm {
    int ref;
    int tag;
    union scm_value val;
};
void refcount_dec(struct scm s) {
    int i;
    struct scm_vector v;
    s.ref = (s.ref - 1);
    if ((0 == s.ref)) {
        if ((2 == s.tag)) {
            i = 0;
            v = s.val.v;
            while ((i < v.len)) {
                i = (i + 1);
                refcount_dec(v.elt[i]);
            }
            free(v.elt);
        } else {
        }
    } else {
    }
}

struct scm allocate_vector(int n) {
  struct scm* elts = malloc(sizeof(struct scm)*n);
  return (struct scm){ .ref = 1, .tag = 1, .val.v = (struct scm_vector){ .len = n, .elt = elts } };
}

struct scm scm_print(struct scm v) {
  int i;
  char s[1024] = { 0 };
  for(i = 0; i < v.val.v.len; i++) {
    s[i] = v.val.v.elt[i].val.i;
  }
  puts(s);
  return v;
}
