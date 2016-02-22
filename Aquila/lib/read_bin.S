.text
.extern min_caml_read_float
min_caml_read_float:
	in      %r1
	sll     %r18, %r1, $8
	in      %r1
	add     %r18, %r18, %r1  # check: or
	sll     %r18, %r18, $8
	in      %r1
	add     %r18, %r18, %r1  # check: or
	sll     %r18, %r18, $8
	in      %r1
	add     %r18, %r18, %r1  # check: or
	st      %r18, %r5, $0
	ld.s    %f3, %r5, $0
	jr      %r4

.globl min_caml_read_int
min_caml_read_int:
	in      %r1
	sll     %r3, %r1, $8
	in      %r1
	add     %r3, %r3, %r1   # check: or
	sll     %r3, %r3, $8
	in      %r1
	add     %r3, %r3, %r1   # check: or
	sll     %r3, %r3, $8
	in      %r1
	add     %r3, %r3, %r1   # check: or
	jr      %r4
