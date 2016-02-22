.data
TWO:  # 2.0
	.word 0x40000000
HALF:  # 0.5
	.word 0x3f000000

.text
div10:
	mr      %r10, %r9
	li      %r3, $0
div10.1:
	li      %r1, $10000
	blt     %r10, %r1, div10.2
	subi    %r10, %r10, $10000
	addi    %r3, %r3, $1000
	j       div10.1
div10.2:
	li      %r1, $1000
	blt     %r10, %r1, div10.3
	subi    %r10, %r10, $1000
	addi    %r3, %r3, $100
	j       div10.2
div10.3:
	li      %r1, $100
	blt     %r10, %r1, div10.4
	subi    %r10, %r10, $100
	addi    %r3, %r3, $10
	j       div10.3
div10.4:
	li      %r1, $10
	blt     %r10, %r1, div10.5
	subi    %r10, %r10, $10
	addi    %r3, %r3, $1
	j       div10.3
div10.5:
	jr      %r4

.extern min_caml_print_char
min_caml_print_char:
	out     %r9
	jr      %r4

.extern min_caml_print_newline
min_caml_print_newline:
	li      %r1, $10
	out     %r1
	jr      %r4

.extern min_caml_print_int
min_caml_print_int:
	subi    %r7, %r7, $1
	st      %r4, %r7, $0
	ble     %r0, %r9, print_int_neg.0
	li      %r18, $45  # '-'
	out     %r18
	sub     %r9, %r0, %r9
print_int_neg.0:
	beq     %r9, %r0, print_int.0
	jal     print_int.1
	j       print_int.end
print_int.1:
	subi    %r7, %r7, $1
	st      %r4, %r7, $0
	beq     %r9, %r0, print_int.2
	jal     div10
	# mul10 = sll3 + sll1
	sll     %r18, %r3, $3
	sll     %r17, %r3, $1
	add     %r18, %r18, %r17
	sub     %r18, %r9, %r18
	addi    %r18, %r18, $48
	st      %r18, %r7, $-1
	st      %r4, %r7, $-2
	subi    %r7, %r7, $2
	mr      %r9, %r3
	jal     print_int.1
	addi    %r7, %r7, $2
	ld      %r4, %r7, $-2
	ld      %r18, %r7, $-1
	out     %r18
print_int.2:
	ld      %r4, %r7, $0
	addi    %r7, %r7, $1
	jr      %r4
print_int.0:
	addi    %r9, %r9, $48
	out     %r9
print_int.end:
	ld      %r4, %r7, $0
	addi    %r7, %r7, $1
	jr      %r4

.text
.extern min_caml_print_float
min_caml_print_float:
	mr      %r2, %r4
	addi    %r5, %r5, $1
	st.s    %f1, %r5, $-1
	ld      %r9, %r5, $-1
	mr      %r18, %r9
	srl     %r9, %r9, $28
	jal     min_caml_print_hex4
	srl     %r9, %r18, $24
	jal     min_caml_print_hex4
	srl     %r9, %r18, $20
	jal     min_caml_print_hex4
	srl     %r9, %r18, $16
	jal     min_caml_print_hex4
	srl     %r9, %r18, $12
	jal     min_caml_print_hex4
	srl     %r9, %r18, $8
	jal     min_caml_print_hex4
	srl     %r9, %r18, $4
	jal     min_caml_print_hex4
	mr      %r9, %r18
	jal     min_caml_print_hex4
	jal     min_caml_print_newline
	mr      %r4, %r2
	jr      %r4

.extern min_caml_print_hex4
min_caml_print_hex4:
	sll     %r9, %r9, $17
	srl     %r9, %r9, $17
	li      %r1, $10
	ble     %r1, %r9, ph8.0
	addi    %r9, %r9, $48
	j       ph8.1
ph8.0:
	addi    %r9, %r9, $55
ph8.1:
	out     %r9
	jr      %r4

.extern min_caml_print_hex32
min_caml_print_hex32:
	mr      %r2, %r4
	mr      %r18, %r9
	srl     %r9, %r9, $28
	jal     min_caml_print_hex4
	srl     %r9, %r18, $24
	jal     min_caml_print_hex4
	srl     %r9, %r18, $20
	jal     min_caml_print_hex4
	srl     %r9, %r18, $16
	jal     min_caml_print_hex4
	srl     %r9, %r18, $12
	jal     min_caml_print_hex4
	srl     %r9, %r18, $8
	jal     min_caml_print_hex4
	srl     %r9, %r18, $4
	jal     min_caml_print_hex4
	mr      %r9, %r18
	jal     min_caml_print_hex4
	jal     min_caml_print_newline
	mr      %r4, %r2
	jr      %r4
	
.data
PI:  # pi
	.word 0x40490fdb
D_PI:  # 2*pi
	.word 0x40c90fdb
H_PI:  # pi/2
	.word 0x3fc90fdb
Q_PI:  # pi/4
	.word 0x3f490fdb
S3:
	.word 0xbe2aaaac
S5:
	.word 0x3c088666
S7:
	.word 0xb94d64b6
S1:
C0:
	.word 0x3f800000
C2:
	.word 0xbf000000
C4:
	.word 0x3d2aa789
C6:
	.word 0xbab38106

.text
kernel_sin:
	mul.s   %f30, %f1, %f1
	li      %r1, S7
	ld.s    %f3, %r1, $0
	li      %r1, S5
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	li      %r1, S3
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	li      %r1, S1
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	mul.s   %f3, %f3, %f1
	jr      %r4

kernel_cos:
	mul.s   %f30, %f1, %f1
	li      %r1, C6
	ld.s    %f3, %r1, $0
	li      %r1, C4
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	li      %r1, C2
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	li      %r1, C0
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	jr      %r4

reduction:
	li      %r1, D_PI
	ld.s    %f31, %r1, $0
	mr.s    %f30, %f31
reduction.0:
	blt.s   %f1, %f31, reduction.1
	li      %r1, TWO
	ld.s    %f29, %r1, $0
	mul.s   %f31, %f31, %f29
	j       reduction.0
reduction.1:
	li      %r1, HALF
	ld.s    %f29, %r1, $0
reduction.2:
	blt.s   %f1, %f30, reduction.3
	blt.s   %f1, %f31, reduction.4
	sub.s   %f1, %f1, %f31
reduction.4:
	mul.s   %f31, %f31, %f29
	j       reduction.2
reduction.3:
	mr.s    %f3, %f1
	jr      %r4

.extern min_caml_sin
min_caml_sin:
	li      %r10, $1
	ble.s   %f1, %f0, sin_neg.0
	li      %r11, $0
	j       sin_neg.1
sin_neg.0:
	li      %r11, $1
sin_neg.1:
	mr      %r2, %r4
	jal     min_caml_fabs
	mr.s    %f1, %f3
	# [sin] reduction-I
	jal     reduction
	mr.s    %f1, %f3
	# [sin] reduction-II
	li      %r1, PI
	ld.s    %f31, %r1, $0
	blt.s   %f1, %f31, sin.0
	sub.s   %f1, %f1, %f31
	sub     %r11, %r10, %r11
sin.0:
	li      %r1, H_PI
	ld.s    %f30, %r1, $0
	blt.s   %f1, %f30, sin.1
	sub.s   %f1, %f31, %f1
sin.1:
	li      %r1, Q_PI
	ld.s    %f31, %r1, $0
	blt.s   %f31, %f1, sin.2
	jal     kernel_sin
	j       sin.3
sin.2:
	sub.s   %f1, %f30, %f1
	jal     kernel_cos
sin.3:
	beq     %r11, %r0, sin.4
	sub.s   %f3, %f0, %f3
sin.4:
	mr      %r4, %r2
	jr      %r4

.extern min_caml_cos
min_caml_cos:
	li      %r10, $1
	li      %r11, $0  # sign
	mr      %r2, %r4
	jal     min_caml_fabs
	mr.s    %f1, %f3
	# [cos] reduction-I
	jal     reduction
	mr.s    %f1, %f3
	# [cos] reduction-II
	li      %r1, PI
	ld.s    %f31, %r1, $0
	blt.s   %f1, %f31, cos.0
	sub.s   %f1, %f1, %f31
	sub     %r11, %r10, %r11
cos.0:
	li      %r1, H_PI
	ld.s    %f30, %r1, $0
	blt.s   %f1, %f30, cos.1
	sub.s   %f1, %f31, %f1
	sub     %r11, %r10, %r11
cos.1:
	li      %r1, Q_PI
	ld.s    %f31, %r1, $0
	blt.s   %f31, %f1, cos.2
	jal     kernel_cos
	j       cos.3
cos.2:
	sub.s   %f1, %f30, %f1
	jal     kernel_sin
cos.3:
	beq     %r11, %r0, cos.4
	sub.s   %f3, %f0, %f3
cos.4:
	mr      %r4, %r2
	jr      %r4

.data
ATAN_THR1:
	.word   0x3ee00000
ATAN_THR2:
	.word   0x401c0000
AT1:
	.word   0x3f800000
AT3:
	.word   0xbeaaaaaa
AT5:
	.word   0x3e4ccccd
AT7:
	.word   0xbe124925
AT9:
	.word   0x3de38e38
AT11:
	.word   0xbdb7d66e
AT13:
	.word   0x3d75e7c5

.text
kernel_atan:
	mul.s   %f30, %f1, %f1
	li      %r1, AT13
	ld.s    %f3, %r1, $0
	li      %r1, AT11
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	li      %r1, AT9
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	li      %r1, AT7
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	li      %r1, AT5
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	li      %r1, AT3
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	li      %r1, AT1
	ld.s    %f31, %r1, $0
	mul.s   %f3, %f3, %f30
	add.s   %f3, %f3, %f31
	mul.s   %f3, %f3, %f1
	jr      %r4

.extern min_caml_atan
min_caml_atan:
	mr      %r2, %r4
	ble.s   %f1, %f0, atan_neg.0
	li      %r11, $0
	j       atan_neg.1
atan_neg.0:
	li      %r11, $1
	sub.s   %f1, %f0, %f1
atan_neg.1:
	# [atan] reduction
	li      %r1, ATAN_THR1
	ld.s    %f31, %r1, $0
	ble.s   %f31, %f1, atan.0
	jal     kernel_atan
	j       atan.2
atan.0:
	li      %r1, ATAN_THR2
	ld.s    %f31, %r1, $0
	ble.s   %f31, %f1, atan.1
	li      %r1, AT1
	ld.s    %f30, %r1, $0
	add.s   %f31, %f1, %f30
	inv.s   %f31, %f31
	sub.s   %f1, %f1, %f30
	mul.s   %f1, %f1, %f31
	jal     kernel_atan
	li      %r1, Q_PI
	ld.s    %f31, %r1, $0
	add.s   %f3, %f3, %f31
	j       atan.2
atan.1:
	inv.s   %f1, %f1
	jal     kernel_atan
	li      %r1, H_PI
	ld.s    %f31, %r1, $0
	sub.s   %f3, %f31, %f3
atan.2:
	beq     %r11, %r0, atan.3
	sub.s   %f3, %f0, %f3
atan.3:
	mr      %r4, %r2
	jr      %r4

.text
.extern min_caml_fabs
.extern min_caml_abs_float
min_caml_abs_float:
min_caml_fabs:
	blt.s   %f0, %f1, fabs.1
	sub.s   %f1, %f0, %f1
fabs.1:
	mr.s    %f3, %f1
	jr      %r4

.data
FLOAT_THR:
	.word   0x4b000000
FLOAT_THRI:
	.word   0x00800000

.text
.extern min_caml_float_of_int
float_of_int:
# CAUTION: DO NOT TOUCH %r4 !!
min_caml_float_of_int:
	li      %r17, $0
	ble     %r0, %r9, itof.cont
	addi    %r17, %r17, $1
	sub     %r9, %r0, %r9
itof.cont:
	li      %r1, FLOAT_THR
	ld.s    %f31, %r1, $0
	ld      %r18, %r1, $0
	blt     %r9. %r18, itof.1
	add.s   %f1, %f0, %f31
	sub     %r9, %r9, %r18
itof.0:
	blt     %r9, %r18, itof.1
	add.s   %f1, %f1, %f31
	sub     %r9, %r9, %r18
	j       itof.0
itof.1:
	add     %r9, %r9, %r18
	mr      %r1, %r5
	addi    %r5, %r5, $1
	st      %r9, %r1, $0
	ld.s    %f1, %r1, $0
	sub.s   %f3, %f1, %f31
	beq     %r17, %r0, itof.2
	sub.s   %f3, %f0, %f3
itof.2:
	jr      %r4

.text
.extern min_caml_truncate
min_caml_truncate:
.extern min_caml_int_of_float
# CAUTION: DO NOT TOUCH %r4 !!
min_caml_int_of_float:
	li      %r15, $0
	ble.s   %f0, %f1, ftoi.cont
	addi    %r15, %r15, $1
	sub.s   %f1, %f0, %f1
ftoi.cont:
	li      %r1, FLOAT_THR
	ld.s    %f31, %r1, $0
	ld      %r18, %r1, $0
	li      %r1, FLOAT_THRI
	ld      %r17, %r1, $0
	li      %r16, $0
ftoi.0:
	blt.s   %f1, %f31, ftoi.1
	sub.s   %f1, %f1, %f31
	add     %r16, %r16, %r17
	j       ftoi.0
ftoi.1:
	add.s   %f1, %f1, %f31
	mr      %r1, %r5
	addi    %r5, %r5, $1
	st.s    %f1, %r1, $0
	ld      %r3, %r1, $0
	sub     %r3, %r3, %r18
	add     %r3, %r3, %r16
	beq     %r15, %r0, ftoi.2
	sub     %r3, %r0, %r3
ftoi.2:
	jr      %r4

.text
.extern min_caml_floor
min_caml_floor:
	li      %r1, FLOAT_THR
	ld.s    %f31, %r1, $0
	ble.s   %f31, %f1, floor.0
	li      %r1, HALF
	ld.s    %f31, %r1, $0
	sub.s   %f1, %f1, %f31
	mr      %r2, %r4
	jal     min_caml_int_of_float
	mr      %r9, %r3
	jal     min_caml_float_of_int
	mr      %r4, %r2
floor.0:
	jr      %r4

.data
SQRT_MAGIC:
	.word   0x5f3759df
F1_5:
	.word   0x3fc00000

.text
.extern min_caml_sqrt
min_caml_sqrt:
	beq.s   %f1, %f0, sqrt.r0
	li      %r1, SQRT_MAGIC
	ld      %r18, %r1, $0
	li      %r1, F1_5
	ld.s    %f30, %r1, $0
	li      %r1, HALF
	ld.s    %f29, %r1, $0
	# fast inverse square root
	st.s    %f1, %r5, $0
	ld      %r1, %r5, $0
	srl     %r1, %r1, $1
	sub     %r1, %r18, %r1
	st      %r1, %r5, $0
	ld.s    %f3, %r5, $0
	# iteration 1
	mul.s   %f28, %f3, %f3
	mul.s   %f28, %f28, %f29
	mul.s   %f28, %f28, %f1
	sub.s   %f28, %f30, %f28
	mul.s   %f3, %f3, %f28
	# iteration 2
	mul.s   %f28, %f3, %f3
	mul.s   %f28, %f28, %f29
	mul.s   %f28, %f28, %f1
	sub.s   %f28, %f30, %f28
	mul.s   %f3, %f3, %f28
	# iteration 3
	mul.s   %f28, %f3, %f3
	mul.s   %f28, %f28, %f29
	mul.s   %f28, %f28, %f1
	sub.s   %f28, %f30, %f28
	mul.s   %f3, %f3, %f28
	# get sqrt from inverse sqrt
	mul.s   %f3, %f3, %f1
	jr      %r4
sqrt.r0:
	mr.s    %f3, %f0
	jr      %r4

.text
.extern min_caml_create_array
min_caml_create_array:
	mr      %r18, %r5
	mr      %r3, %r5
	add     %r5, %r5, %r9
create_array.0:
	beq     %r9, %r0, create_array.1
	st      %r10, %r18, $0
	addi    %r18, %r18, $1
	subi    %r9, %r9, $1
	j       create_array.0
create_array.1:
	jr      %r4

.text
.extern min_caml_create_float_array
min_caml_create_float_array:
	mr      %r18, %r5
	mr      %r3, %r5
	add     %r5, %r5, %r9
create_float_array.0:
	beq     %r9, %r0, create_float_array.1
	st.s    %f1, %r18, $0
	addi    %r18, %r18, $1
	subi    %r9, %r9, $1
	j       create_float_array.0
create_float_array.1:
	jr      %r4
