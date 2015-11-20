.data
TWO:  # 2.0
	.long 0x40000000
HALF:  # 0.5
	.long 0x3f000000

.text
.globl min_caml_fequal
min_caml_fequal:
	c.eq.s  %f1, %f3
	bclt    fequal.0
	li      %v0, $0
	jr      %ra
fequal.0:
	li      %v0, $1
	jr      %ra

.globl min_caml_fless
min_caml_fless:
	c.lt.s  %f1, %f3
	bclt    fless.0
	li      %v0, $0
	jr      %ra
fless.0:
	li      %v0, $1
	jr      %ra

.globl min_caml_fispos
min_caml_fispos:
	c.lt.s  %f0, %f1
	bclt    fispos.0
	li      %v0, $0
	jr      %ra
fispos.0:
	li      %v0, $1
	jr      %ra

.globl min_caml_fisneg
min_caml_fisneg:
	c.lt.s  %f1, %f0
	bclt    fisneg.0
	li      %v0, $0
	jr      %ra
fisneg.0:
	li      %v0, $1
	jr      %ra

.globl min_caml_fiszero
min_caml_fiszero:
	c.eq.s  %f1, %f0
	bclt    fiszero.0
	li      %v0, $0
	jr      %ra
fiszero.0:
	li      %v0, $1
	jr      %ra

.globl min_caml_fhalf
min_caml_fhalf:
	li      %at, HALF
	lw.s    %f31, (%at)
	mul.s   %f2, %f1, %f31
	jr      %ra

.globl min_caml_fsqr
min_caml_fsqr:
	mul.s   %f2, %f1, %f1
	jr      %ra

.globl min_caml_fneg
min_caml_fneg:
	sub.s   %f2, %f0, %f1
	jr      %ra

# Reference:
# http://homepage.cs.uiowa.edu/~jones/bcd/divide.html
# http://www.piclist.com/techref/language/ccpp/convertbase.htm
.text
#div10:
#	# calculate "approximate" int(A/10)
#	# result will be either A/10 or (A+1)/10
#	sw      %t0, -1(%sp)
#div10.0:
#	srl     %v0, %t0, $1
#	add     %v0, %v0, %t0
#	addi    %v0, %v0, $1
#	srl     %v0, %v0, $1
#	srl     %at, %v0, $4
#	add     %v0, %v0, %at
#	srl     %at, %v0, $8
#	add     %v0, %v0, %at
#	srl     %at, %v0, $16
#	add     %v0, %v0, %at
#	srl     %v0, %v0, $3
#	lw      %t0, -1(%sp)
#	# if result = (A+1)/10 then re-calculate
#	sll     %t9, %v0, $3
#	sll     %t8, %v0, $1
#	add     %t9, %t9, %t8
#	sub     %t9, %t0, %t9
#	slti    %at, %t9, $10
#	bne     %at, %zero, div10.1
#	addi    %t0, %t0, $1
#	j       div10.0
#div10.1:
#	jr      %ra
div10:
	move    %t1, %t0
	li      %v0, $0
div10.1:
	slti    %at, %t1, $10000
	bne     %at, %zero, div10.2
	subi    %t1, %t1, $10000
	addi    %v0, %v0, $1000
	j       div10.1
div10.2:
	slti    %at, %t1, $1000
	bne     %at, %zero, div10.3
	subi    %t1, %t1, $1000
	addi    %v0, %v0, $100
	j       div10.2
div10.3:
	slti    %at, %t1, $100
	bne     %at, %zero, div10.4
	subi    %t1, %t1, $100
	addi    %v0, %v0, $10
	j       div10.3
div10.4:
	slti    %at, %t1, $10
	bne     %at, %zero, div10.5
	subi    %t1, %t1, $10
	addi    %v0, %v0, $1
	j       div10.3
div10.5:
	jr      %ra

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

.globl min_caml_print_char
min_caml_print_char:
	out     %t0
	jr      %ra

.globl min_caml_print_newline
min_caml_print_newline:
	li      %at, $10
	out     %at
	jr      %ra

.globl min_caml_print_int
min_caml_print_int:
	subi    %sp, %sp, $1
	sw      %ra, (%sp)
	slt     %at, %t0, %zero
	beq     %at, %zero, print_int_neg.0
	li      %t9, $45  # '-'
	out     %t9
	sub     %t0, %zero, %t0
print_int_neg.0:
	beq     %t0, %zero, print_int.0
	jal     print_int.1
	j       print_int.end
print_int.1:
	subi    %sp, %sp, $1
	sw      %ra, (%sp)
	beq     %t0, %zero, print_int.2
	jal     div10
	# mul10 = sll3 + sll1
	sll     %t9, %v0, $3
	sll     %t8, %v0, $1
	add     %t9, %t9, %t8
	sub     %t9, %t0, %t9
	addi    %t9, %t9, $48
	sw      %t9, -1(%sp)
	sw      %ra, -2(%sp)
	subi    %sp, %sp, $2
	move    %t0, %v0
	jal     print_int.1
	addi    %sp, %sp, $2
	lw      %ra, -2(%sp)
	lw      %t9, -1(%sp)
	out     %t9
print_int.2:
	lw      %ra, (%sp)
	addi    %sp, %sp, $1
	jr      %ra
print_int.0:
	addi    %t0, %t0, $48
	out     %t0
print_int.end:
	lw      %ra, (%sp)
	addi    %sp, %sp, $1
	jr      %ra

.text
.globl min_caml_print_float
min_caml_print_float:
	move    %a0, %ra
	addi    %gp, %gp, $1
	sw.s    %f1, -1(%gp)
	lw      %t0, -1(%gp)
	move    %t9, %t0
	srl     %t0, %t0, $28
	jal     min_caml_print_hex4
	srl     %t0, %t9, $24
	jal     min_caml_print_hex4
	srl     %t0, %t9, $20
	jal     min_caml_print_hex4
	srl     %t0, %t9, $16
	jal     min_caml_print_hex4
	srl     %t0, %t9, $12
	jal     min_caml_print_hex4
	srl     %t0, %t9, $8
	jal     min_caml_print_hex4
	srl     %t0, %t9, $4
	jal     min_caml_print_hex4
	move    %t0, %t9
	jal     min_caml_print_hex4
	jal     min_caml_print_newline
	move    %ra, %a0
	jr      %ra

.globl min_caml_print_hex4
min_caml_print_hex4:
	andi    %t0, %t0, $15
	slti    %at, %t0, $10
	beq     %at, %zero, ph8.0
	addi    %t0, %t0, $48
	j       ph8.1
ph8.0:
	addi    %t0, %t0, $55
ph8.1:
	out     %t0
	jr      %ra

.globl min_caml_print_hex32
min_caml_print_hex32:
	move    %a0, %ra
	move    %t9, %t0
	srl     %t0, %t0, $28
	jal     min_caml_print_hex4
	srl     %t0, %t9, $24
	jal     min_caml_print_hex4
	srl     %t0, %t9, $20
	jal     min_caml_print_hex4
	srl     %t0, %t9, $16
	jal     min_caml_print_hex4
	srl     %t0, %t9, $12
	jal     min_caml_print_hex4
	srl     %t0, %t9, $8
	jal     min_caml_print_hex4
	srl     %t0, %t9, $4
	jal     min_caml_print_hex4
	move    %t0, %t9
	jal     min_caml_print_hex4
	jal     min_caml_print_newline
	move    %ra, %a0
	jr      %ra
	
.data
PI:  # pi
	.long 0x40490fdb
D_PI:  # 2*pi
	.long 0x40c90fdb
H_PI:  # pi/2
	.long 0x3fc90fdb
Q_PI:  # pi/4
	.long 0x3f490fdb
S3:
	.long 0xbe2aaaac
S5:
	.long 0x3c088666
S7:
	.long 0xb94d64b6
S1:
C0:
	.long 0x3f800000
C2:
	.long 0xbf000000
C4:
	.long 0x3d2aa789
C6:
	.long 0xbab38106

.text
kernel_sin:
	mul.s   %f30, %f1, %f1
	li      %at, S7
	lw.s    %f2, (%at)
	li      %at, S5
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	li      %at, S3
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	li      %at, S1
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	mul.s   %f2, %f2, %f1
	jr      %ra

kernel_cos:
	mul.s   %f30, %f1, %f1
	li      %at, C6
	lw.s    %f2, (%at)
	li      %at, C4
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	li      %at, C2
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	li      %at, C0
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	jr      %ra

reduction:
	li      %at, D_PI
	lw.s    %f31, (%at)
	move.s  %f30, %f31
reduction.0:
	c.le.s  %f31, %f1
	bclf    reduction.1
	li      %at, TWO
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f29
	j       reduction.0
reduction.1:
	li      %at, HALF
	lw.s    %f29, (%at)
reduction.2:
	c.le.s  %f30, %f1
	bclf    reduction.3
	c.le.s  %f31, %f1
	bclf    reduction.4
	sub.s   %f1, %f1, %f31
reduction.4:
	mul.s   %f31, %f31, %f29
	j       reduction.2
reduction.3:
	move.s  %f2, %f1
	jr      %ra

.globl min_caml_sin
min_caml_sin:
	li      %t1, $1
	c.le.s  %f1, %zero
	bclt    sin_neg.0
	li      %t2, $0
	j       sin_neg.1
sin_neg.0:
	li      %t2, $1
sin_neg.1:
	move    %a0, %ra
	jal     min_caml_fabs
	move.s  %f1, %f2
	# [sin] reduction-I
	jal     reduction
	move.s  %f1, %f2
	# [sin] reduction-II
	li      %at, PI
	lw.s    %f31, (%at)
	c.le.s  %f31, %f1
	bclf    sin.0
	sub.s   %f1, %f1, %f31
	sub     %t2, %t1, %t2
sin.0:
	li      %at, H_PI
	lw.s    %f30, (%at)
	c.le.s  %f30, %f1
	bclf    sin.1
	sub.s   %f1, %f31, %f1
sin.1:
	li      %at, Q_PI
	lw.s    %f31, (%at)
	c.le.s  %f1, %f31
	bclf    sin.2
	jal     kernel_sin
	j       sin.3
sin.2:
	sub.s   %f1, %f30, %f1
	jal     kernel_cos
sin.3:
	beq     %t2, %zero, sin.4
	sub.s   %f2, %f0, %f2
sin.4:
	move    %ra, %a0
	jr      %ra

.globl min_caml_cos
min_caml_cos:
	li      %t1, $1
	li      %t2, $0  # sign
	move    %a0, %ra
	jal     min_caml_fabs
	move.s  %f1, %f2
	# [cos] reduction-I
	jal     reduction
	move.s  %f1, %f2
	# [cos] reduction-II
	li      %at, PI
	lw.s    %f31, (%at)
	c.le.s  %f31, %f1
	bclf    cos.0
	sub.s   %f1, %f1, %f31
	sub     %t2, %t1, %t2
cos.0:
	li      %at, H_PI
	lw.s    %f30, (%at)
	c.le.s  %f30, %f1
	bclf    cos.1
	sub.s   %f1, %f31, %f1
	sub     %t2, %t1, %t2
cos.1:
	li      %at, Q_PI
	lw.s    %f31, (%at)
	c.le.s  %f1, %f31
	bclf    cos.2
	jal     kernel_cos
	j       cos.3
cos.2:
	sub.s   %f1, %f30, %f1
	jal     kernel_sin
cos.3:
	beq     %t2, %zero, cos.4
	sub.s   %f2, %f0, %f2
cos.4:
	move    %ra, %a0
	jr      %ra

.data
ATAN_THR1:
	.long   0x3ee00000
ATAN_THR2:
	.long   0x401c0000
AT1:
	.long   0x3f800000
AT3:
	.long   0xbeaaaaaa
AT5:
	.long   0x3e4ccccd
AT7:
	.long   0xbe124925
AT9:
	.long   0x3de38e38
AT11:
	.long   0xbdb7d66e
AT13:
	.long   0x3d75e7c5

.text
kernel_atan:
	mul.s   %f30, %f1, %f1
	li      %at, AT13
	lw.s    %f2, (%at)
	li      %at, AT11
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	li      %at, AT9
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	li      %at, AT7
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	li      %at, AT5
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	li      %at, AT3
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	li      %at, AT1
	lw.s    %f31, (%at)
	mul.s   %f2, %f2, %f30
	add.s   %f2, %f2, %f31
	mul.s   %f2, %f2, %f1
	jr      %ra

.globl min_caml_atan
min_caml_atan:
	move    %a0, %ra
	c.le.s  %f1, %zero
	bclt    atan_neg.0
	li      %t2, $0
	j       atan_neg.1
atan_neg.0:
	li      %t2, $1
	sub.s   %f1, %f0, %f1
atan_neg.1:
	# [atan] reduction
	li      %at, ATAN_THR1
	lw.s    %f31, (%at)
	c.lt.s  %f1, %f31
	bclf    atan.0
	jal     kernel_atan
	j       atan.2
atan.0:
	li      %at, ATAN_THR2
	lw.s    %f31, (%at)
	c.lt.s  %f1, %f31
	bclf    atan.1
	li      %at, AT1
	lw.s    %f30, (%at)
	add.s   %f31, %f1, %f30
	inv.s   %f31, %f31
	sub.s   %f1, %f1, %f30
	mul.s   %f1, %f1, %f31
	jal     kernel_atan
	li      %at, Q_PI
	lw.s    %f31, (%at)
	add.s   %f2, %f2, %f31
	j       atan.2
atan.1:
	inv.s   %f1, %f1
	jal     kernel_atan
	li      %at, H_PI
	lw.s    %f31, (%at)
	sub.s   %f2, %f31, %f2
atan.2:
	beq     %t2, %zero, atan.3
	sub.s   %f2, %f0, %f2
atan.3:
	move    %ra, %a0
	jr      %ra

.text
.globl min_caml_fabs
.globl min_caml_abs_float
min_caml_abs_float:
min_caml_fabs:
	c.lt.s  %f0, %f1
	bclt    fabs.1
	sub.s   %f1, %f0, %f1
fabs.1:
	move.s  %f2, %f1
	jr      %ra

.data
FLOAT_THR:
	.long   0x4b000000
FLOAT_THRI:
	.long   0x00800000

.text
.globl min_caml_float_of_int
float_of_int:
# CAUTION: DO NOT TOUCH %ra !!
min_caml_float_of_int:
	li      %t8, $0
	slt     %at, %t0, %zero
	beq     %at, %zero, itof.cont
	addi    %t8, %t8, $1
	sub     %t0, %zero, %t0
itof.cont:
	li      %at, FLOAT_THR
	lw.s    %f31, (%at)
	lw      %t9, (%at)
	slt     %at, %t0, %t9
	bne     %at, %zero, itof.1
	add.s   %f1, %f0, %f31
	sub     %t0, %t0, %t9
itof.0:
	slt     %at, %t0, %t9
	bne     %at, %zero, itof.1
	add.s   %f1, %f1, %f31
	sub     %t0, %t0, %t9
	j       itof.0
itof.1:
	add     %t0, %t0, %t9
	move    %at, %gp
	addi    %gp, %gp, $1
	sw      %t0, (%at)
	lw.s    %f1, (%at)
	sub.s   %f2, %f1, %f31
	beq     %t8, %zero, itof.2
	sub.s   %f2, %f0, %f2
itof.2:
	jr      %ra

.text
.globl min_caml_truncate
min_caml_truncate:
.globl min_caml_int_of_float
# CAUTION: DO NOT TOUCH %ra !!
min_caml_int_of_float:
	li      %t6, $0
	c.lt.s  %f1, %f0
	bclf    ftoi.cont
	addi    %t6, %t6, $1
	sub.s   %f1, %f0, %f1
ftoi.cont:
	li      %at, FLOAT_THR
	lw.s    %f31, (%at)
	lw      %t9, (%at)
	li      %at, FLOAT_THRI
	lw      %t8, (%at)
	li      %t7, $0
ftoi.0:
	c.lt.s  %f1, %f31
	bclt    ftoi.1
	sub.s   %f1, %f1, %f31
	add     %t7, %t7, %t8
	j       ftoi.0
ftoi.1:
	add.s   %f1, %f1, %f31
	move    %at, %gp
	addi    %gp, %gp, $1
	sw.s    %f1, (%at)
	lw      %v0, (%at)
	sub     %v0, %v0, %t9
	add     %v0, %v0, %t7
	beq     %t6, %zero, ftoi.2
	sub     %v0, %zero, %v0
ftoi.2:
	jr      %ra

.text
.globl min_caml_floor
min_caml_floor:
	li      %at, FLOAT_THR
	lw.s    %f31, (%at)
	c.lt.s  %f1, %f31
	bclf    floor.0
	li      %at, HALF
	lw.s    %f31, (%at)
	sub.s   %f1, %f1, %f31
	move    %a0, %ra
	jal     min_caml_int_of_float
	move    %t0, %v0
	jal     min_caml_float_of_int
	move    %ra, %a0
floor.0:
	jr      %ra

.text
.globl min_caml_sqrt
min_caml_sqrt:
	li      %at, HALF
	lw.s    %f31, (%at)
	mul.s   %f30, %f1, %f31
	li      %t0, $5
sqrt.0:
	beq     %t0, %zero, sqrt.1
	inv.s   %f29, %f30
	mul.s   %f29, %f29, %f1
	add.s   %f30, %f30, %f29
	mul.s   %f30, %f30, %f31
	subi    %t0, %t0, $1
	j       sqrt.0
sqrt.1:
	move.s  %f2, %f30
	jr      %ra

.text
.globl min_caml_create_array
min_caml_create_array:
	move    %t9, %gp
	move    %v0, %gp
	add     %gp, %gp, %t0
create_array.0:
	beq     %t0, %zero, create_array.1
	sw      %t1, (%t9)
	addi    %t9, %t9, $1
	subi    %t0, %t0, $1
	j       create_array.0
create_array.1:
	jr      %ra

.text
.globl min_caml_create_float_array
min_caml_create_float_array:
	move    %t9, %gp
	move    %v0, %gp
	add     %gp, %gp, %t0
create_float_array.0:
	beq     %t0, %zero, create_float_array.1
	sw.s    %f1, (%t9)
	addi    %t9, %t9, $1
	subi    %t0, %t0, $1
	j       create_float_array.0
create_float_array.1:
	jr      %ra
