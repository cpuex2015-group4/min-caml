#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

typedef union {
  int32 i[2];
  float f;
} dbl;

value gethi(value v) {
  dbl d;
  d.f = Double_val(v);
  return copy_int32(d.i[0]);
}

value getlo(value v) {
  dbl d;
  d.f = Double_val(v);
  return copy_int32(d.i[1]);
}
