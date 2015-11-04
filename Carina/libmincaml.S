.data
TWO:  # 2.0
	.long 0x40000000
HALF:  # 0.5
	.long 0x3f000000

.text
.globl min_caml_fequal
min_caml_fequal:
	c.eq.s  %f1, %f2
	bclt    fequal.0
	li      %v0, $0
	jr      %ra
fequal.0:
	li      %v0, $1
	jr      %ra

.globl min_caml_fless
min_caml_fless:
	c.lt.s  %f1, %f2
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
	li      %v0, 0
	jr      %ra
fispos.0:
	li      %v0, 1
	jr      %ra

.globl min_caml_fisneg
min_caml_fisneg:
	c.lt.s  %f1, %f0
	bclt    fisneg.0
	li      %v0, 0
	jr      %ra
fisneg.0:
	li      %v0, 1
	jr      %ra

.globl min_caml_fiszero
min_caml_fiszero:
	c.eq.s  %f1, %f0
	bclt    fiszero.0
	li      %v0, 0
	jr      %ra
fiszero.0:
	li      %v0, 1
	jr      %ra

.globl min_caml_fhalf
min_caml_fhalf:
	li      %at, HALF
	lw.s    %f31, (%at)
	mul.s   %f1, %f1, %f31
	jr      %ra

.globl min_caml_fsqr
min_caml_fsqr:
	mul.s   %f1, %f1, %f1
	jr      %ra

.globl min_caml_fneg
min_caml_fneg:
	sub.s   %f1, %f0, %f1
	jr      %ra

# TODO
.globl min_caml_read_float
min_caml_read_float:
	jr      %ra

# TODO
.globl min_caml_read_int
min_caml_read_int:
	jr      %ra

# TODO
.globl min_caml_print_char
min_caml_print_char:
	jr      %ra

# TODO
.globl min_caml_print_int
min_caml_print_int:
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
	.long 0x3f000000
C4:
	.long 0x3d2aa789
C6:
	.long 0xbab38106

.text
kernel_sin:
	mul.s   %f30, %f1, %f1
	sub.s   %f30, %f0, %f30
	li      %at, S5
	lw.s    %f31, (%at)
	li      %at, S7
	lw.s    %f29, (%at)
	mul.s   %f29, %f29, %f30
	add.s   %f31, %f31, %f29
	li      %at, S3
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	li      %at, S1
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	mul.s   %f1, %f31, %f1
	jr      %ra

kernel_cos:
	mul.s   %f30, %f1, %f1
	sub.s   %f30, %f0, %f30
	li      %at, C6
	lw.s    %f29, (%at)
	mul.s   %f31, %f29, %f30
	li      %at, C4
	lw.s    %f29, (%at)
	add.s   %f31, %f31, %f29
	mul.s   %f31, %f31, %f30
	li      %at, C2
	lw.s    %f29, (%at)
	add.s   %f31, %f31, %f29
	mul.s   %f31, %f31, %f30
	li      %at, C0
	lw.s    %f29, (%at)
	add.s   %f1, %f31, %f29
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
	jr      %ra

.globl min_caml_sin
min_caml_sin:
	c.le.s  %f1, %zero
	bclt    sin_neg.0
	li      %t2, $0
	j       sin_neg.1
sin_neg.0:
	li      %t2, $1
sin_neg.1:
	subi    %sp, %sp, $1
	sw      %ra, (%sp)
	jal     min_caml_fabs
	# [sin] reduction-I
	jal     reduction
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
	sub.s   %f1, %f0, %f1
sin.4:
	lw      %ra, (%sp)
	addi    %sp, %sp, $1
	jr      %ra

.globl min_caml_cos
min_caml_cos:
	li      %t1, $1
	li      %t2, $0  # sign
	subi    %sp, %sp, $1
	sw      %ra, (%sp)
	jal     min_caml_fabs
	# [cos] reduction-I
	jal     reduction
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
	sub.s   %f1, %f0, %f1
cos.4:
	lw      %ra, (%sp)
	addi    %sp, %sp, $1
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
	sub.s   %f30, %f0, %f30
	li      %at, AT13
	lw.s    %f29, (%at)
	mul.s   %f31, %f30, %f29
	li      %at, AT11
	lw.s    %f29, (%at)
	add.s   %f31, %f31, %f29
	li      %at, AT9
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	li      %at, AT7
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	li      %at, AT5
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	li      %at, AT3
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	li      %at, AT1
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	mul.s   %f1, %f31, %f1
	jr      %ra

.globl min_caml_atan
min_caml_atan:
	c.le.s  %f1, %zero
	bclt    atan_neg.0
	li      %t2, $0
	j       atan_neg.1
atan_neg.0:
	li      %t2, $1
atan_neg.1:
	subi    %sp, %sp, $1
	sw      %ra, (%sp)
	jal     min_caml_fabs
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
	add.s   %f1, %f1, %f31
	j       atan.2
atan.1:
	inv.s   %f1, %f1
	jal     kernel_atan
	li      %at, H_PI
	lw.s    %f31, (%at)
	sub.s   %f1, %f31, %f1
atan.2:
	beq     %t2, %zero, atan.3
	sub.s   %f1, %f0, %f1
atan.3:
	lw      %ra, (%sp)
	addi    %sp, %sp, $1
	jr      %ra

.text
.globl min_caml_fabs
min_caml_fabs:
	c.lt.s  %f0, %f1
	bclt    fabs.1
	sub.s   %f1, %f0, %f1
fabs.1:
	jr      %ra

.data
FLOAT_THR:
	.long   0x4b000000
FLOAT_THR2:
	.long   0x4affffff
FLOAT_THRI:
	.long   0x00800000

.text
.globl min_caml_float_of_int
min_caml_float_of_int:
	li      %at, FLOAT_THR
	lw.s    %f31, (%at)
	lw      %t9, FLOAT_THR
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
	sw      %t0, -1(%sp)
	lw.s    %f1, -1(%sp)
	sub.s   %f1, %f1, %f31
	jr      %ra

.text
.globl min_caml_int_of_float
min_caml_int_of_float:
	li      %at, FLOAT_THR
	lw.s    %f31, (%at)
	li      %at, FLOAT_THR2
	lw.s    %f30, (%at)
	lw      %t9, FLOAT_THR
	lw      %t8, FLOAT_THRI
	li      %t7, $0
ftoi.0:
	c.lt.s  %f1, %f31
	bclt    ftoi.1
	sub.s   %f1, %f1, %f31
	add     %t7, %t7, %t8
	j       ftoi.0
ftoi.1:
	add.s   %f1, %f1, %f30
	sw.s    %f1, -1(%sp)
	lw      %v0, -1(%sp)
	sub     %v0, %v0, %t9
	add     %v0, %v0, %t7
	jr      %ra

.text
.globl min_caml_truncate
min_caml_truncate:
	j       min_caml_floor

.text
.globl min_caml_floor
min_caml_floor:
	li      %at, FLOAT_THR
	lw.s    %f31, (%at)
	c.lt.s  %f1, %f31
	bclf    floor.0
	subi    %sp, %sp, $1
	sw      %ra, (%sp)
	jal     min_caml_int_of_float
	move    %t0, %v0
	jal     min_caml_float_of_int
	lw      %ra, (%sp)
	addi    %sp, %sp, $1
floor.0:
	jr      %ra

.text
.globl min_caml_sqrt
min_caml_sqrt:
	li      %at, HALF
	lw.s    %f31, (%at)
	mul.s   %f30, %f1, %f31
	li      %t0, $10
sqrt.0:
	beq     %t0, %zero, sqrt.1
	inv.s   %f29, %f30
	mul.s   %f29, %f29, %f1
	add.s   %f30, %f30, %f29
	mul.s   %f30, %f30, %f31
	subi    %t0, %t0, $1
	j       sqrt.0
sqrt.1:
	move.s  %f1, %f30
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
create_array.0:
	beq     %t0, %zero, create_array.1
	sw.s    %f1, (%t9)
	addi    %t9, %t9, $1
	subi    %t0, %t0, $1
	j       create_array.0
create_array.1:
	jr      %ra
