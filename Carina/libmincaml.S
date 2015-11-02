.data
TWO:  # 2.0
	.long 0x40000000
HALF:  # 0.5
	.long 0x3f000000
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
	add     %at, %at, %gp
	lw.s    %f31, (%at)
	li      %at, S7
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	mul.s   %f29, %f29, %f30
	add.s   %f31, %f31, %f29
	li      %at, S3
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	li      %at, S1
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	mul.s   %f1, %f31, %f1
	jr      %ra

kernel_cos:
	mul.s   %f30, %f1, %f1
	sub.s   %f30, %f0, %f30
	li      %at, C6
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	mul.s   %f31, %f29, %f30
	li      %at, C4
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	add.s   %f31, %f31, %f29
	mul.s   %f31, %f31, %f30
	li      %at, C2
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	add.s   %f31, %f31, %f29
	mul.s   %f31, %f31, %f30
	li      %at, C0
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	add.s   %f1, %f31, %f29
	jr      %ra

reduction:
	li      %at, D_PI
	add     %at, %at, %gp
	lw.s    %f31, (%at)
	move.s  %f30, %f31
reduction.0:
	c.le.s  %f31, %f1
	bclf    reduction.1
	li      %at, TWO
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f29
	j       reduction.0
reduction.1:
	li      %at, HALF
	add     %at, %at, %gp
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
	add     %at, %at, %gp
	lw.s    %f31, (%at)
	c.le.s  %f31, %f1
	bclf    sin.0
	sub.s   %f1, %f1, %f31
	sub     %t2, %t1, %t2
sin.0:
	li      %at, H_PI
	add     %at, %at, %gp
	lw.s    %f30, (%at)
	c.le.s  %f30, %f1
	bclf    sin.1
	sub.s   %f1, %f31, %f1
sin.1:
	li      %at, Q_PI
	add     %at, %at, %gp
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
	add     %at, %at, %gp
	lw.s    %f31, (%at)
	c.le.s  %f31, %f1
	bclf    cos.0
	sub.s   %f1, %f1, %f31
	sub     %t2, %t1, %t2
cos.0:
	li      %at, H_PI
	add     %at, %at, %gp
	lw.s    %f30, (%at)
	c.le.s  %f30, %f1
	bclf    cos.1
	sub.s   %f1, %f31, %f1
	sub     %t2, %t1, %t2
cos.1:
	li      %at, Q_PI
	add     %at, %at, %gp
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
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	mul.s   %f31, %f30, %f29
	li      %at, AT11
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	add.s   %f31, %f31, %f29
	li      %at, AT9
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	li      %at, AT7
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	li      %at, AT5
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	li      %at, AT3
	add     %at, %at, %gp
	lw.s    %f29, (%at)
	mul.s   %f31, %f31, %f30
	add.s   %f31, %f31, %f29
	li      %at, AT1
	add     %at, %at, %gp
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
	add     %at, %at, %gp
	lw.s    %f31, (%at)
	c.lt.s  %f1, %f31
	bclf    atan.0
	jal     kernel_atan
	j       atan.2
atan.0:
	li      %at, ATAN_THR2
	add     %at, %at, %gp
	lw.s    %f31, (%at)
	c.lt.s  %f1, %f31
	bclf    atan.1
	li      %at, AT1
	add     %at, %at, %gp
	lw.s    %f30, (%at)
	add.s   %f31, %f1, %f30
	inv.s   %f31, %f31
	sub.s   %f1, %f1, %f30
	mul.s   %f1, %f1, %f31
	jal     kernel_atan
	li      %at, Q_PI
	add     %at, %at, %gp
	lw.s    %f31, (%at)
	add.s   %f1, %f1, %f31
	j       atan.2
atan.1:
	inv.s   %f1, %f1
	jal     kernel_atan
	li      %at, H_PI
	add     %at, %at, %gp
	lw.s    %f31, (%at)
	sub.s   %f1, %f31, %f1
atan.2:
	beq     %t2, %zero, atan.3
	sub.s   %f1, %f0, %f1
atan.3:
	lw      %ra, (%sp)
	addi    %sp, %sp, $1
	jr      %ra

.globl min_caml_fabs
min_caml_fabs:
	c.lt.s  %f0, %f1
	bclt    fabs.1
	sub.s   %f1, %f0, %f1
fabs.1:
	jr      %ra
