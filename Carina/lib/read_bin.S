.text
.globl min_caml_read_float
min_caml_read_float:
	in      %at
	sw      %at, (%gp)
	lw.s    %f2, (%gp)
	jr      %ra

.globl min_caml_read_int
min_caml_read_int:
	in      %v0
	jr      %ra

