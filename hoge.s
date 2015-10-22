.data
.balign	8
.text
g.11:
	addl	%ecx, %ebx
	addl	%ebx, %eax
	ret
f.6:
	movl	$10, %ebx
	movl	$5, %ecx
	movl	%ecx, 0(%ebp)
	movl	%eax, %ecx
	movl	0(%ebp), %eax
	call	g.11
	addl	$10, %eax
	ret
.globl	min_caml_start
min_caml_start:
.globl	_min_caml_start
_min_caml_start: # for cygwin
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	movl	32(%esp),%ebp
	movl	36(%esp),%eax
	movl	%eax,min_caml_hp
	movl	$7, %eax
	call	f.6
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
