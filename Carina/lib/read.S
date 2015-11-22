.data
BASE:
	.long   0x3dcccccd

.text
.globl min_caml_read_float
min_caml_read_float:
	move    %a0, %ra
	move.s  %f31, %f0 # result
	li      %t0, $0   # integral part
	move.s  %f29, %f0 # decimal  part
	li      %at, BASE
	lw.s    %f28, (%at)
	li      %t6, $0   # sign: +
read_float.space:
	in      %t9
	subi    %at, %t9, $32  # ' '
	beq     %at, %zero, read_float.space
	subi    %at, %t9, $10  # '\n'
	beq     %at, %zero, read_float.space
read_float.start:
	subi    %at, %t9, $45  # '-'
	bne     %at, %zero, read_float.1
	li      %t6, $1   # sign: -
read_float.0:
	in      %t9
read_float.1:
	subi    %at, %t9, $46  # '.'
	beq     %at, %zero, read_float.integral
	li      %at, $47
	slt     %at, %at, %t9
	beq     %at, %zero, read_float.abort
	li      %at, $58
	slt     %at, %t9, %at
	beq     %at, %zero, read_float.abort
	subi    %t9, %t9, $48
	# mul10 = sll3 + sll1
	sll     %t8, %t0, $3
	sll     %t7, %t0, $1
	add     %t8, %t7, %t8
	add     %t0, %t8, %t9
	j       read_float.0
read_float.integral:
	jal     float_of_int
	move.s  %f30, %f2  # integral part
read_float.2:
	in      %t0
	li      %at, $47
	slt     %at, %at, %t0
	beq     %at, %zero, read_float.end
