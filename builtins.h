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
    putchar('?');
  }
}

int scm_extract_truth(struct scm x) {
  if(x.tag == 0)
    if(x.val.i == 0)
      return 0;
  return 1;
}

struct scm scm_plus(struct scm a, struct scm b) {
  assert(a.tag == 0);
  assert(b.tag == 0);
  return (struct scm){ .tag = 0, .val.i = a.val.i + b.val.i };
}

struct scm scm_eq(struct scm a, struct scm b) {
  if(a.tag == 0 && b.tag == 0) {
    return (struct scm){ .tag = 0, .val.i = (a.val.i == b.val.i) };
  }
  else {
    return (struct scm){ .tag = 0, .val.i = 0 };
  }
}
