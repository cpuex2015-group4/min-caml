#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

typedef union {
  int32 i[1];
  float f;
} dbl;

value f2bin(value v) {
  dbl d;
  d.f = Double_val(v);
  return copy_int32(d.i[0]);
}
