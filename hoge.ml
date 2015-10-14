let rec fib n m =
  if m <= 1 then n else
  fib (n - 1) (m - 1) + fib (n - 2) (m - 1) in
print_int (fib 30 5)
