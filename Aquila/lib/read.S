.data
BASE:
	.word   0x3dcccccd

.text
.extern min_caml_read_float
min_caml_read_float:
	mr      %r2, %r4
	mr.s    %f31, %f0 # result
	li      %r9, $0   # integral part
	mr.s    %f29, %f0 # decimal  part
	li      %r1, BASE
	ld.s    %f28, %r1, $0
	li      %r15, $0   # sign: +
read_float.space:
	in      %r18
	subi    %r1, %r18, $32  # ' '
	beq     %r1, %r0, read_float.space
	subi    %r1, %r18, $10  # '\n'
	beq     %r1, %r0, read_float.space
read_float.start:
	subi    %r1, %r18, $45  # '-'
	beq     %r1, %r0, read_float.1
	j       read_float.2
read_float.1:
	li      %r15, $1   # sign: -
read_float.0:
	in      %r18
read_float.2:
	subi    %r1, %r18, $46  # '.'
	beq     %r1, %r0, read_float.integral
	li      %r1, $47
	ble     %r18, %r1, read_float.abort
	li      %r1, $58
	ble     %r1, %r18, read_float.abort
	subi    %r18, %r18, $48
	# mul10 = sll3 + sll1
	sll     %r17, %r9, $3
	sll     %r16, %r9, $1
	add     %r17, %r16, %r17
	add     %r9, %r17, %r18
	j       read_float.0
read_float.integral:
	jal     float_of_int
	mr.s    %f30, %f3  # integral part
read_float.2:
	in      %r9
	li      %r1, $47
	ble     %r9, %r1, read_float.end
	li      %r1, $58
	ble     %r1, %r9, read_float.end
	subi    %r9, %r9, $48
	jal     float_of_int
	add.s   %f29, %f29, %f3
	mul.s   %f29, %f28, %f29
	j       read_float.2
read_float.end:
	add.s   %f3, %f29, %f30
read_float.sign:
	subi    %r15, %r15, $1
	beq     %r15, %r0, read_float.end2
	j       read_float.end3
read_float.end2:
	sub.s   %f3, %f0, %f3
read_float.end3:
	mr      %r4, %r2
	jr      %r4
read_float.abort:
	jal     float_of_int
	j       read_float.sign

.extern min_caml_read_int
min_caml_read_int:
	li      %r3, $0   # result
	li      %r15, $0   # sign: +
	li      %r17, $45  # '-'
read_int.space:
	in      %r18
	subi    %r1, %r18, $32  # ' '
	beq     %r1, %r0, read_int.space
	subi    %r1, %r18, $10  # '\n'
	beq     %r1, %r0, read_int.space
read_int.start:
	beq     %r17, %r18, read_int.1
	j       read_int.2
read_int.1:
	li      %r15, $1   # sign: -
read_int.0:
	in      %r18
read_int.2:
	li      %r1, $47
	ble     %r18, %r1, read_int.end
	li      %r1, $58
	ble     %r1, %r18, read_int.end
	subi    %r18, %r18, $48
	# mul10 = sll3 + sll1
	sll     %r17, %r3, $3
	sll     %r16, %r3, $1
	add     %r17, %r16, %r17
	add     %r3, %r17, %r18
	j       read_int.0
read_int.end:
	subi    %r15, %r15, $1
	beq     %r15, %r0, read_int.end2
	jr      %r4
	sub     %r3, %r0, %r3
read_int.end2:
	jr      %r4

