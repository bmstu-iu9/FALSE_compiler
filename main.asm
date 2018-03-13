.modeL TINY
.DATA

filename   			DB	'1.txt',0
execfile   			DB	'1.com',0
exechandle 			DW	?
handle     			DW	?
fbuff      			DB	?
number				DW 	?
bytes				DB  6 dup(?)
address_pointer 	DW	100h
flags      			DB	0
jmp_labels  		DW	32 dup(?)
jmp_labels_pointer 	DW	0
jmp_offset			DW  0
vars_offset			DW 	104h
cond 				DW  ?
body 				DW  ?
proc_addr			DW 	?

OPCODE_EOF			DB  '$'
OPCODE_JMP  		DB	0EBh, 000H
OPCODE_MOV_DX		DB  0BAh
OPCODE_PRINT		DB  0B4h, 009h, 0CDh, 021h
OPCODE_END			DB  0B4h, 04Ch ,0CDh, 021h
OPCODE_MOV_AX		DB	0B8h
OPCODE_MOV_AX_BX	DB	08Bh, 0C3h
OPCODE_CALL_ABS		DB	0FFh, 015h
OPCODE_XOR_MOV_DI	DB	033h, 0FFh, 089h, 01Dh, 083h, 0EEh, 002h, 08Bh, 01Ch
OPCODE_RUNTIME_CALL	DB 	033h, 0FFh, 089h, 015h
OPCODE_RET			DB 	0C3h
OPCODE_INIT_SE 		DB	0BEh, 0AAh, 0AAh

len dw (?)

simple_commands_table DW (?)
dw 3A00h, offset runtime_assign
dw 3B00h, offset runtime_get_value 
dw 2E00h, offset runtime_print_top_stack
dw 3D00h, offset runtime_eq
dw 3F00h, offset runtime_if
dw 2300h, offset runtime_while
dw 2B00h, offset runtime_add
dw 2D00h, offset runtime_sub
dw 2A00h, offset runtime_mul
dw 2F00h, offset runtime_div
dw 7E00h, offset runtime_neg
dw 3E00h, offset runtime_gt
dw 2600h, offset runtime_and
dw 7C00h, offset runtime_or
dw 2500h, offset runtime_pop
dw 5C00h, offset runtime_swap
dw 4000h, offset runtime_rot
dw 4F00h, offset runtime_pick
dw 2C00h, offset runtime_comma
dw 5E00h, offset runtime_input
dw 5F00h, offset runtime_unary_minus
end_commands_table DW (?)

.CODE
.STARTUP
begin_runtime:
	jmp end_runtime
	VARS: dw 26 dup(0)

runtime_if proc near
	mov di, bx
	call runtime_pop
	cmp bx, 0
	jnz call_lambda
	ret

call_lambda:
	call near ptr di
	ret	
runtime_if endp
	
runtime_while proc near
	mov body, bx
	call runtime_pop
	mov cond, bx
	call runtime_pop
_loop:
	mov di, cond
	call near ptr di
	cmp bx, 0
	jnz call_body
	ret

call_body:
	mov di, body
	call near ptr di
	jmp _loop

runtime_while endp
	
runtime_add proc near
	mov ax, bx
	add ax, [si]
	jmp push_value
runtime_add endp

runtime_and proc near
	mov ax, bx
	and ax, [si]
	jmp push_value
runtime_and endp
	
runtime_assign proc near
	mov di, bx;
	call runtime_pop
	mov [di], bx
	call runtime_pop
	ret
runtime_assign endp

runtime_comma proc near
	mov  ah, 2
	mov  dx, bx
	int  21H 
	ret
runtime_comma endp

runtime_div proc near
	mov ax, bx
	div byte ptr [si]
	jmp push_value
runtime_div endp

runtime_gt proc near
	cmp bx, [si]
	ja _gt
	mov ax, 0000h
	jmp push_value
	_gt:
	mov ax, 0001h
	jmp push_value
runtime_gt endp

runtime_input proc near
	mov ah, 01h
	int 21h
	mov ah, 00h
	call runtime_push
	ret
runtime_input endp

runtime_mul proc near
	mov ax, bx
	mul byte ptr [si]
	jmp push_value
runtime_mul endp

runtime_neg proc near
	cmp bx, 0000h
	jz false
	mov bx, 0000h
	false:
	mov bx, 0001h
	ret
runtime_neg endp

runtime_or proc near
	mov ax, bx
	or ax, [si]
	jmp push_value
runtime_or endp

runtime_pick proc near
	mov cx, bx
	mov di, si
	sub di, cx
	sub di, cx
	mov bx, [di]
	ret
runtime_pick endp

runtime_rot proc near
	mov dx, bx
	call runtime_pop
	mov ax, bx
	call runtime_pop
	mov cx, bx
	call runtime_pop
	call runtime_push
	mov ax, dx
	call runtime_push
	mov ax, cx
	call runtime_push
	ret
runtime_rot endp

runtime_sub proc near
	mov ax, bx
	sub ax, [si]
	
	jmp push_value
	
runtime_sub endp

runtime_swap proc near
	xchg bx, [si]
	ret
runtime_swap endp

runtime_get_value proc near
	mov bx, [bx]
	ret
runtime_get_value endp
	
runtime_push proc near 
	add si, 2;
	mov [si], bx;
	mov bx, ax
	ret
runtime_push endp

runtime_pop proc near
	mov bx, [si]
	sub si, 2
	ret
runtime_pop endp

runtime_print_top_stack proc near
	mov  ah, 2 
	mov  ax, bx
	mov  di, 6
	mov	 cx, 10
	
	mov bytes[di], '$'
	
	itoa:
	cmp	ax, 0
	jz	print
	dec di
	xor  dx, dx
	div cx
	add dl, 30H
	mov bytes[di], dl
	jmp itoa
	
	print:
	mov ah, 09h
	lea dx, bytes[di] 
	int  21H 
	ret
runtime_print_top_stack endp

runtime_eq proc near
	cmp bx, [si]
	jz ax_true
	mov ax, 0
	jmp push_value
	ax_true:
	mov ax, 1
	
	push_value:
	call runtime_pop
	call runtime_pop
	call runtime_push
	ret

runtime_eq endp

runtime_unary_minus proc near
	neg bx
	ret
runtime_unary_minus endp
	
end_runtime:

	xor di, di
	call openfile  
	call createfile
	call init_runtime
    call readfile         
    call closefile 


openfile proc near
    mov  ax,3D00H        
    lea  dx,filename    
    int  21H            
    mov  handle,ax      
    ret
openfile endp

createfile proc near
    mov  ah, 3ch
    mov  cx, 0
    mov  dx, offset execfile
    int  21h
	mov exechandle, ax
	
	ret	
createfile endp

init_runtime proc near
		
	mov cx, end_runtime - begin_runtime
	lea dx, begin_runtime
	call write
	
	mov cx, 3
	lea dx, OPCODE_INIT_SE
	call write
	
	mov dx, address_pointer
	sub dx, 100h 
	call set_fp
	ret
init_runtime endp

readfile proc near
         mov  ah,3FH         
         mov  bx,handle      
         lea  dx,fbuff       
         mov  cx,1           
         int  21H            	
         cmp  ax,0           
         jz   eoff           
         mov  dl,fbuff       
         cmp  dl,1ah         
         jz   eoff           
         call proc_symbol           
         jmp  readfile       
eoff:    ret
readfile endp

proc_symbol proc near
	cmp fbuff, 7Bh
	jnz _end_comm
	mov flags, 02h
	ret
	
	_end_comm:
	cmp fbuff, 7Dh
	jnz _comm
	mov flags, 00h
	ret
	
	_comm:
	cmp flags, 02h
	jnz _0
	ret
		
	_0:
	cmp fbuff, 22h ;22H -- "
	jnz _1
	jmp call_proc_quotation
	
	_1:
	cmp flags, 01h;
	jnz _cmp_num
	mov cx, 1 
    lea dx, fbuff
    call write
	ret
	
	_cmp_num:
	cmp fbuff, 2Fh; 30 -- 0
	jna _not_num
	jmp check_is_num
	
	_num:
	mov flags, 03H
	mov al, fbuff
	sub al, 30H
	mov bytes[di], al
	inc di
	ret
		
	_not_num:
	cmp flags, 03H
	jnz _2 
	mov flags, 00H ; clear flags

	mov cx, 1
	lea dx, OPCODE_MOV_AX
	call write
	
	xor ax, ax
	mov dx, 10
	mov cx, di
	xor di, di
	
	proc_byte:
	cmp di, cx
	jz write_num
	mov bl, bytes[di]
	mul dx
	mov dx, 10
	add ax, bx
	inc di
	jmp proc_byte
	
	write_num:
	mov number, ax
	mov cx, 2
	lea dx, number
	call write
	
	call _push
	xor di, di
	
	
	_2:
	cmp fbuff, 5Bh ; -- [
	jnz _6
	
	mov cx, 2 
    lea dx, OPCODE_JMP
    call write
	
	mov si, jmp_labels_pointer;
	mov bx, address_pointer;
	dec bx;
	mov jmp_labels[si], bx;  move to the stack of labels current address for backtracing
	add jmp_labels_pointer, 2
	ret;

	
	_6:
	cmp fbuff, 5Dh ; -- ]
	jnz _7
	jmp call_proc_end
	
	_7:
	cmp fbuff, 21h ; -- !
	jnz _8
	
	mov cx, 9
	lea dx, OPCODE_XOR_MOV_DI
	call write
	
	mov cx, 2
	lea dx, OPCODE_CALL_ABS
	call write
	ret
	
	_8:
	cmp fbuff, 3Dh ; -- =
	jnz _9
	
	_9:
	cmp fbuff, 24h
	jnz _20
	
	mov cx, 2
	lea dx, OPCODE_MOV_AX_BX
	mov ax, 4000h
	mov bx, exechandle
	int 21h
	add address_pointer, cx
	
	call _push
	ret
		
	_20:
	mov si, offset simple_commands_table
	mov bx, offset end_commands_table
	sub bx, si
	mov len, bx
	_iter_table:
	xor bx, bx
	mov bh, fbuff
	cmp [si], bx
	jz _print_function
	add si, 2
	mov cx, si
	sub cx, offset simple_commands_table
	cmp cx, len
	ja _21
	jmp _iter_table

	_print_function:
	add si, 2
	mov dx, [si]
	mov proc_addr, dx
	call write_exec
	ret
	
	_21:
	cmp fbuff, 60h ; 61 -- a
	ja check_is_var
	ret

check_is_var:
	cmp fbuff, 7Bh
	jnb _ret 
	xor ax, ax;
	mov al, fbuff;
	sub al, 61H;
	mov bx, 0002H;
	mul bx
	add ax, vars_offset;
	mov number, ax
	
	mov cx, 1
	lea dx, OPCODE_MOV_AX
	call write
	
	mov cx, 2
	lea dx, number
	call write
	
	call _push
	_ret:
	ret

check_is_num:
	cmp fbuff, 3Ah
	jnb _fail
	jmp _num
	_fail:
	jmp _not_num
	
call_proc_quotation: 
    call proc_quotation
	ret
		
call_proc_end:
	call proc_end
	ret
	
proc_symbol endp

proc_quotation proc near
	cmp flags, 00H;
	jnz  end_string
	
	mov flags, 01H;
	
	mov cx, 2 
    lea dx, OPCODE_JMP
    call write
	
	mov si, jmp_labels_pointer;
	mov bx, address_pointer;
	dec bx;
	mov jmp_labels[si], bx;  move to the stack of labels current address for backtracing
	inc jmp_labels_pointer
	ret;
	
end_string:
	mov flags, 00H; clear flags 
	
	mov cx, 1
	lea dx, OPCODE_EOF
	call write
	
	mov cx, 1
    lea dx, OPCODE_MOV_DX
	call write
	
	dec jmp_labels_pointer; backtracing

	lea bx, jmp_labels[si]
	mov dx, [bx]
	sub dx, 100h
	
	call set_fp
	
	mov si, jmp_labels_pointer;
	lea bx, jmp_labels[si]
	mov ax, address_pointer
	sub ax, [bx];
	sub ax, 2;
	mov jmp_offset, ax;
	lea dx, jmp_offset
	mov bx, exechandle
	mov ax,	4000h
	mov cx, 1
	int 21h ; write backtracing
	
	call ret_fp
	
	mov si, jmp_labels_pointer;
	mov ax, jmp_labels[si]
	inc ax;
	mov jmp_labels[si], ax;
	
	lea dx, jmp_labels[si]
	mov cx, 2
	call write
	
	mov cx, 4
	lea dx, OPCODE_PRINT
	call write
	
	ret;
	
proc_quotation endp


proc_end proc near

    mov cx, 1
	lea dx, OPCODE_RET
	call write
	
	sub jmp_labels_pointer, 2; backtracing
	mov si, jmp_labels_pointer
	lea bx, jmp_labels[si]
	mov dx, [bx]
	sub dx, 100h
	
	call set_fp
	
	mov si, jmp_labels_pointer;
	lea bx, jmp_labels[si]
	mov ax, address_pointer
	sub ax, [bx];
	dec ax
	mov jmp_offset, ax;
	lea dx, jmp_offset
	mov bx, exechandle
	mov ax,	4000h
	mov cx, 1
	int 21h
	
	call ret_fp
	
	;datapush address
	mov ax, jmp_labels[si]
	inc ax
	mov number, ax
	
	mov cx, 1
	lea dx, OPCODE_MOV_AX
	call write
	
	mov cx, 2
	lea dx, number
	call write
	
	call _push
	ret

proc_end endp

write_call proc near
	call write
	
	mov cx, 2
	lea dx, OPCODE_CALL_ABS
	call write
	
	ret
write_call endp

write_exec proc near
	
	mov cx, 1
	lea dx, OPCODE_MOV_DX
	call write
	
	mov cx, 2
	lea dx, proc_addr
	call write

	mov cx, 4
	lea dx, OPCODE_RUNTIME_CALL
	call write
	
	mov cx, 2
	lea dx, OPCODE_CALL_ABS
	call write

	ret
write_exec endp

_push proc near
	lea dx, runtime_push
	mov proc_addr, dx
	call write_exec
	ret
_push endp

write proc near	
	mov ah, 40h
	mov bx, exechandle
	int 21h
	
	add address_pointer, cx
	ret
write endp

ret_fp proc near
	mov ax, 4202h
	xor cx, cx;
	xor dx, dx;
	int 21h; return file pointer back to the eof
	ret
ret_fp endp

set_fp proc near
	mov bx, exechandle
	mov ax, 4200h
	xor cx, cx;
	int 21h
	ret
set_fp endp

closefile proc near
	mov cx, 4 
    lea dx, OPCODE_END
	call write
	
	mov  ah,3EH        
    mov  bx,handle     
    int  21H  
	mov  bx,exechandle 
    int  21h
    ret
	
closefile endp

END

