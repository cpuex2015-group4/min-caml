.text
.globl print_newline
print_newline:
	li      %a0, $10
	syscall $4
	jr      %ra

.globl print_int
print_int:
	addi    %a0, %a0, $0x30
	syscall $4
	jr      %ra

.globl print_byte
print_byte:
	syscall $4
	jr      %ra

# TODO: output to stderr
.globl prerr_int
prerr_int:
	subi    %sp, %sp, $1
	sw      %ra, (%sp)
	jal     print_int
	lw      %ra, (%sp)
	addi    %sp, %sp, $1
	jr      %ra

# TODO: output to stderr
.globl prerr_byte
prerr_byte:
	subi    %sp, %sp, $1
	sw      %ra, (%sp)
	jal     print_byte
	lw      %ra, (%sp)
	addi    %sp, %sp, $1
	jr      %ra

#.globl prerr_float
#prerr_float:
#	pushl	%ebp
#	movl	%esp, %ebp
#	ALIGNSTACK0
#	subl	$8, %esp
#	movsd	%xmm0, (%esp)
#	pushl	$format_float
#	pushl	U(stderr)
#	call	U(fprintf)
#	movl	%ebp, %esp
#	popl	%ebp
#	ret

.globl read_int
read_int:
	syscall $3
	subi    %v0, %v0, $0x30
	jr      %ra

#.globl read_float
#read_float:
#	pushl	%ebp
#	movl	%esp, %ebp
#	ALIGNSTACK0
#	subl	$8, %esp
#	leal	-8(%ebp), %eax
#	pushl	%eax
#	pushl	$format_float
#	call	U(scanf)
#	movsd	-8(%ebp), %xmm0
#	movl	%ebp, %esp
#	popl	%ebp
#	ret

#.globl create_array
#create_array:
#	pushl	%ebp
#	movl	%esp, %ebp
#	pushl	%ecx
#	pushl	%edx
#	movl	%eax, %ecx
#	movl	hp, %eax
#	movl	%ecx, %edx
#	shll	$2, %edx
#	addl	%edx, hp
#create_array_loop:
#	cmpl	$0, %ecx
#	jne	create_array_cont
#create_array_exit:
#	popl	%edx
#	popl	%ecx
#	movl	%ebp, %esp
#	popl	%ebp
#	ret
#create_array_cont:
#	decl	%ecx
#	movl	%ebx, (%eax,%ecx,4)
#	jmp	create_array_loop
#.globl create_float_array
#create_float_array:
#	pushl	%ebp
#	movl	%esp, %ebp
#	pushl	%ecx
#	pushl	%edx
#	movl	%eax, %ecx
#	movl	hp, %eax
#	movl	%ecx, %edx
#	shll	$3, %edx
#	addl	%edx, hp
#create_float_array_loop:
#	cmpl	$0, %ecx
#	jne	create_float_array_cont
#create_float_array_exit:
#	popl	%edx
#	popl	%ecx
#	movl	%ebp, %esp
#	popl	%ebp
#	ret
#create_float_array_cont:
#	decl	%ecx
#	movsd	%xmm0, (%eax,%ecx,8)
#	jmp	create_float_array_loop
#.globl abs_float
#abs_float:
#	pushl	%ebp
#	movl	%esp, %ebp
#	ALIGNSTACK2
#	subl	$8, %esp
#	movsd	%xmm0, (%esp)
#	call	U(fabs)
#	fstpl	(%esp)
#	movsd	(%esp), %xmm0
#	movl	%ebp, %esp
#	popl	%ebp
#	ret
#.globl sqrt
#sqrt:
#	sqrtsd	%xmm0, %xmm0
#	ret
#.globl floor
#floor:
#	pushl	%ebp
#	movl	%esp, %ebp
#	ALIGNSTACK2
#	subl	$8, %esp
#	movsd	%xmm0, (%esp)
#	call	U(floor)
#	fstpl	(%esp)
#	movsd	(%esp), %xmm0
#	movl	%ebp, %esp
#	popl	%ebp
#	ret
#.globl int_of_float
#int_of_float:
#.globl truncate
#truncate:
#	cvttsd2si	%xmm0, %eax
#	ret
#.globl float_of_int
#float_of_int:
#	cvtsi2sd	%eax, %xmm0
#	ret
#.globl cos
#cos:
#	pushl	%ebp
#	movl	%esp, %ebp
#	ALIGNSTACK2
#	subl	$8, %esp
#	movsd	%xmm0, (%esp)
#	call	U(cos)
#	fstpl	(%esp)
#	movsd	(%esp), %xmm0
#	movl	%ebp, %esp
#	popl	%ebp
#	ret
#.globl sin
#sin:
#	pushl	%ebp
#	movl	%esp, %ebp
#	ALIGNSTACK2
#	subl	$8, %esp
#	movsd	%xmm0, (%esp)
#	call	U(sin)
#	fstpl	(%esp)
#	movsd	(%esp), %xmm0
#	movl	%ebp, %esp
#	popl	%ebp
#	ret
#.globl atan
#atan:
#	pushl	%ebp
#	movl	%esp, %ebp
#	ALIGNSTACK2
#	subl	$8, %esp
#	movsd	%xmm0, (%esp)
#	call	U(atan)
#	fstpl	(%esp)
#	movsd	(%esp), %xmm0
#	movl	%ebp, %esp
#	popl	%ebp
#	ret
#.data
#format_int:
#	.asciz	"%d"
#format_float:
#	.asciz	"%lf"
#.balign 8
#float_0:
#	.long	0x0
#	.long	0x0
#float_1:
#	.long	0x0
#	.long	0x3ff00000
#.balign 16
#.globl fnegd
#fnegd:
#	.long	0
#	.long	0x80000000
#	.long	0
#	.long	0
