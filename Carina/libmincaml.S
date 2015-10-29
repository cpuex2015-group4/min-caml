.data
DOUBLE_PI: .long 0x40c90fdb

.text
#reduction:
#	li      %at, DOUBLE_PI
#	lw.s    %f31, (%at)
#	move.s  %f30, %f31
#reduction.2:
#	c.le.s  %f31, %f1
#	bclf    reduction.3
#	mul.s   %f31, %f31, $2
#	j       reduction.2
#reduction.3:
#	c.le.s  %f30, %f1
#	bclf    reduction.4
#	c.le.s  %f31, %f0
#	bclf    reduction.5
#	sub.s   %f1, %f1, %f31
#reduction.5:
#	div.s   %f31, %f31, $2
#	j       reduction.3
#reduction.4:
#	jr      %ra

.globl min_caml_cos
min_caml_cos:
	# [cos] reduction-I

.globl min_caml_fabs
min_caml_fabs:
	c.lt.s  %f0, %f1
	bclt    fabs.1
	sub.s   %f1, %f0, %f1
fabs.1:
	jr      %ra
