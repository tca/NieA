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
    if (((2 == s.tag) || (3 == s.tag))) {
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

struct scm allocate_vector(int len) {
    struct scm_vector* v;
    v = malloc(sizeof(struct scm_vector));
    v->len = len;
    v->ref = 1;
    v->elt = malloc((sizeof(struct scm) * len));
    return (struct scm){ .tag = 2, .val.v = v };
}

struct scm make_closure(struct scm c) {
    c.tag = 3;
    return c;
}

struct scm scm_print(struct scm a) {
    int i;
    struct scm* elt;
    if ((a.tag == 0)) {
        printf("%d\n", a.val.i);
    } else {
        if (((a.tag == 2) || (a.tag == 3))) {
            elt = a.val.v->elt;
            i = 0;
            while ((i < a.val.v->len)) {
                if ((elt[i].tag == 0)) {
                    putchar(elt[i].val.i);
                } else {
                    putchar('?');
                }
                i = (i + 1);
            }
        } else {
            putchar('?');
        }
    }
}

int scm_extract_truth(struct scm x) {
    return (!((0 == x.tag)) || !((0 == x.val.i)));
}

struct scm scm_vector_ref(struct scm vec, struct scm idx) {
    assert((vec.tag == 2));
    assert((idx.tag == 0));
    struct scm* v;
    int i;
    v = vec.val.v->elt;
    i = idx.val.i;
    assert((i < vec.val.v->len));
    return v[i];
}

struct scm scm_make_vector(struct scm len, struct scm gen) {
    assert((len.tag == 0));
    assert((gen.tag == 3));
    struct scm v;
    int j;
    int n;
    scm_fptr fn;
    struct scm* elt;
    fn = gen.val.v->elt[0].val.f;
    n = len.val.i;
    v = allocate_vector(n);
    elt = v.val.v->elt;
    j = 0;
    while ((j < n)) {
        elt[j] = fn(gen, (struct scm){ .tag = 0, .val.i = j });
        j = (j + 1);
    }
    return v;
}

struct scm scm_plus(struct scm a, struct scm b) {
    assert((a.tag == 0));
    assert((b.tag == 0));
    return (struct scm){ .tag = 0, .val.i = (a.val.i + b.val.i) };
}

struct scm scm_minus(struct scm a, struct scm b) {
    assert((a.tag == 0));
    assert((b.tag == 0));
    return (struct scm){ .tag = 0, .val.i = (a.val.i - b.val.i) };
}

struct scm scm_multiply(struct scm a, struct scm b) {
    assert((a.tag == 0));
    assert((b.tag == 0));
    return (struct scm){ .tag = 0, .val.i = (a.val.i * b.val.i) };
}

struct scm scm_divide(struct scm a, struct scm b) {
    assert((a.tag == 0));
    assert((b.tag == 0));
    return (struct scm){ .tag = 0, .val.i = (a.val.i / b.val.i) };
}

struct scm scm_eq(struct scm a, struct scm b) {
    if (((a.tag == 0) && (b.tag == 0))) {
        return (struct scm){ .tag = 0, .val.i = (a.val.i == b.val.i) };
    } else {
        return (struct scm){ .tag = 0, .val.i = 0 };
    }
}

struct scm scm_gt(struct scm a, struct scm b) {
    if (((a.tag == 0) && (b.tag == 0))) {
        return (struct scm){ .tag = 0, .val.i = (a.val.i > b.val.i) };
    } else {
        return (struct scm){ .tag = 0, .val.i = 0 };
    }
}

struct scm scm_lt(struct scm a, struct scm b) {
    if (((a.tag == 0) && (b.tag == 0))) {
        return (struct scm){ .tag = 0, .val.i = (a.val.i < b.val.i) };
    } else {
        return (struct scm){ .tag = 0, .val.i = 0 };
    }
}

struct scm scm_gteq(struct scm a, struct scm b) {
    if (((a.tag == 0) && (b.tag == 0))) {
        return (struct scm){ .tag = 0, .val.i = (a.val.i >= b.val.i) };
    } else {
        return (struct scm){ .tag = 0, .val.i = 0 };
    }
}

struct scm scm_lteq(struct scm a, struct scm b) {
    if (((a.tag == 0) && (b.tag == 0))) {
        return (struct scm){ .tag = 0, .val.i = (a.val.i <= b.val.i) };
    } else {
        return (struct scm){ .tag = 0, .val.i = 0 };
    }
}

