.text
.globl min_caml_read_float
min_caml_read_float:
	in      %at
	sll     %t9, %at, $8
	in      %at
	or      %t9, %t9, %at
	sll     %t9, %t9, $8
	in      %at
	or      %t9, %t9, %at
	sll     %t9, %t9, $8
	in      %at
	or      %t9, %t9, %at
	sw      %t9, (%gp)
	lw.s    %f2, (%gp)
	jr      %ra

.globl min_caml_read_int
min_caml_read_int:
	in      %at
	sll     %v0, %at, $8
	in      %at
	or      %v0, %v0, %at
	sll     %v0, %v0, $8
	in      %at
	or      %v0, %v0, %at
	sll     %v0, %v0, $8
	in      %at
	or      %v0, %v0, %at
	jr      %ra
