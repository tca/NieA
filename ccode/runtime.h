#include "scm_fptr.h"
struct scm_vector {
    int len;
    int ref;
    struct scm* elt;
};
union scm_value {
    int i;
    scm_fptr f;
    struct scm_vector* v;
};
struct scm {
    int tag;
    union scm_value val;
};
void refcount_dec(struct scm s) {
    int i;
    struct scm_vector* v;
    if ((2 == s.tag)) {
        v = s.val.v;
        v->ref = (v->ref - 1);
        if ((0 == v->ref)) {
            i = 0;
            while ((i < v->len)) {
                i = (i + 1);
                refcount_dec(v->elt[i]);
            }
            free(v->elt);
        } else {
        }
    } else {
    }
}

