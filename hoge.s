.data
.text
	.globl  _min_caml_start
fib.15:
	slti    %at, %t1, $1
	bne     %at, %zero, beq_else.36
	move    %v0, %t0
	jr      %ra
beq_else.36:
	li      %t2, $1
	sub     %t2, %t0, %t2
	li      %t3, $1
	sub     %t3, %t1, %t3
	sw      %t1, 0(%sp)
	sw      %t0, 1(%sp)
	move    %t1, %t3
	move    %t0, %t2
	subi    %sp, %sp, $2
	sw      %ra, 1(%sp)
	jal     fib.15
	lw      %ra, 1(%sp)
	addi    %sp, %sp, $2
	move    %t0, %v0
	lw      %t1, 1(%sp)
	li      %t1, $2
	sub     %t1, %t1, %t1
	lw      %t2, 0(%sp)
	li      %t2, $1
	sub     %t2, %t2, %t2
	sw      %t0, 2(%sp)
	move    %t0, %t1
	move    %t1, %t2
	subi    %sp, %sp, $3
	sw      %ra, 2(%sp)
	jal     fib.15
	lw      %ra, 2(%sp)
	addi    %sp, %sp, $3
	move    %t0, %v0
	lw      %t1, 2(%sp)
	add     %v0, %t1, %t0
	jr      %ra
_min_caml_start: # main entry point
	subi    %sp, %sp, $2
	sw      %ra, 1(%sp)
	sw      %fp, 0(%sp)
	move    %fp, %sp
	# main program start
	li      %t0, $30
	li      %t1, $5
	subi    %sp, %sp, $0
	sw      %ra, -1(%sp)
	jal     fib.15
	lw      %ra, -1(%sp)
	addi    %sp, %sp, $0
	move    %t0, %v0
	subi    %sp, %sp, $0
	sw      %ra, -1(%sp)
	jal     min_caml_print_int
	lw      %ra, -1(%sp)
	addi    %sp, %sp, $0
	# main program end
	move    %sp, %fp
	lw      %fp, 0(%sp)
	lw      %ra, -1(%sp)
	addi    %sp, %sp, $2
	hlt
