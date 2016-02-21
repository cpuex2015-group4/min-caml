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
	li      %at, $58
	slt     %at, %t0, %at
	beq     %at, %zero, read_float.end
	subi    %t0, %t0, $48
	jal     float_of_int
	add.s   %f29, %f29, %f2
	mul.s   %f29, %f28, %f29
	j       read_float.2
read_float.end:
	add.s   %f2, %f29, %f30
read_float.sign:
	subi    %t6, %t6, $1
	bne     %t6, %zero, read_float.end2
	sub.s   %f2, %f0, %f2
read_float.end2:
	move    %ra, %a0
	jr      %ra
read_float.abort:
	jal     float_of_int
	j       read_float.sign

.globl min_caml_read_int
min_caml_read_int:
	li      %v0, $0   # result
	li      %t6, $0   # sign: +
	li      %t8, $45  # '-'
read_int.space:
	in      %t9
	subi    %at, %t9, $32  # ' '
	beq     %at, %zero, read_int.space
	subi    %at, %t9, $10  # '\n'
	beq     %at, %zero, read_int.space
read_int.start:
	bne     %t8, %t9, read_int.1
	li      %t6, $1   # sign: -
read_int.0:
	in      %t9
read_int.1:
	li      %at, $47
	slt     %at, %at, %t9
	beq     %at, %zero, read_int.end
	li      %at, $58
	slt     %at, %t9, %at
	beq     %at, %zero, read_int.end
	subi    %t9, %t9, $48
	# mul10 = sll3 + sll1
	sll     %t8, %v0, $3
	sll     %t7, %v0, $1
	add     %t8, %t7, %t8
	add     %v0, %t8, %t9
	j       read_int.0
read_int.end:
	subi    %t6, %t6, $1
	bne     %t6, %zero, read_int.end2
	sub     %v0, %zero, %v0
read_int.end2:
	jr      %ra

