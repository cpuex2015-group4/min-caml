let rec f x =
  let a = 10 in
  let rec g y =
    a + x + y in
  g 5 + a in
print_int (f 7)
