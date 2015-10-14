.data
.text
	.globl  _min_caml_start
test.21:
	add     %t0, %t0, %t1
	add     %t0, %t0, %t2
	add     %t0, %t0, %t3
	add     %t0, %t0, %t4
	add     %t0, %t0, %t5
	add     %t0, %t0, %t6
	add     %t0, %t0, %t7
	add     %t0, %t0, %t8
	add     %v0, %t0, %t9
	jr      %ra
_min_caml_start: # main entry point
	subi    %sp, %sp, $2
	sw      %ra, 1(%sp)
	sw      %fp, 0(%sp)
	# main program start
	li      %t0, $1
	li      %t1, $2
	li      %t2, $3
	li      %t3, $4
	li      %t4, $5
	li      %t5, $6
	li      %t6, $7
	li      %t7, $8
	li      %t8, $9
	li      %t9, $10
	subi    %sp, %sp, $0
	sw      %ra, -1(%sp)
	jal     test.21
	lw      %ra, -1(%sp)
	addi    %sp, %sp, $0
	move    %t0, %v0
	addi    %t0, %t0, $1000
	subi    %sp, %sp, $0
	sw      %ra, -1(%sp)
	jal     min_caml_print_int
	lw      %ra, -1(%sp)
	addi    %sp, %sp, $0
	# main program end
	lw      %fp, (%sp)
	lw      %ra, -1(%sp)
	addi    %sp, %sp, $2
	hlt
