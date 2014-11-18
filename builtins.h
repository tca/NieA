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
    putchar("?");
  }
}

struct scm scm_add(struct scm a, struct scm b) {
  assert(a.tag == 0);
  assert(b.tag == 0);
  return (struct scm){ .tag = 0, .val.i = a.val.i + b.val.i };
}

struct scm scm_eqeq(struct scm a, struct scm b) {
  if(a.tag == 0 && b.tag == 0) {
    return (struct scm){ .tag = 0, .val.i = (a.val.i == b.val.i) };
  }
  else {
    return (struct scm){ .tag = 0, .val.i = 0 };
  }
}
