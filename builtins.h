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
