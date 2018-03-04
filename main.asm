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

OPCODE_EOF			DB  '$'
OPCODE_JMP  		DB	0EBh, 000H
OPCODE_MOV_DX		DB  0BAh
OPCODE_PRINT		DB  0B4h, 009h, 0CDh, 021h
OPCODE_END			DB  0B4h, 04Ch ,0CDh, 021h
OPCODE_MOV_AX		DB	0B8h
OPCODE_MOV_AX_BX	DB	08Bh, 0C3h
OPCODE_CALL_ABS		DB	0FFh, 015h
OPCODE_XOR_MOV_DI	DB	033h, 0FFh, 089h, 01Dh, 083h, 0EEh, 002h, 08Bh, 01Ch
OPCODE_RET			DB 	0C3h
OPCODE_INIT_SE 		DB	0BEh, 0AAh, 0AAh


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

call_runtime_and:
	xor di, di
	lea dx, runtime_and
	mov [di], dx
end_call_runtime_and:
	
call_runtime_comma:
	xor di, di
	lea dx, runtime_comma
	mov [di], dx
end_call_runtime_comma:
	
call_runtime_if:
	xor di, di
	lea dx, runtime_if
	mov [di], dx
end_call_runtime_if:

call_runtime_input:
	xor di, di
	lea dx, runtime_input
	mov [di], dx
end_call_runtime_input:

call_runtime_pick:
	xor di, di
	lea dx, runtime_pick
	mov [di], dx
end_call_runtime_pick:

call_runtime_pop:
	xor di, di
	lea dx, runtime_pop
	mov [di], dx
end_call_runtime_pop:

call_runtime_rot:
	xor di, di
	lea dx, runtime_rot
	mov [di], dx
end_call_runtime_rot:

call_runtime_while:
	xor di, di
	lea dx, runtime_while
	mov [di], dx
end_call_runtime_while:
	
call_runtime_assign:
	xor di, di
	lea dx, runtime_assign
	mov [di], dx
end_call_runtime_assign:

call_runtime_get_value:
	xor di, di
	lea dx, runtime_get_value
	mov [di], dx
end_call_runtime_get_value:

call_runtime_push:
	xor di, di
	lea dx, runtime_push
	mov [di], dx
end_call_runtime_push:

call_runtime_print_top_stack:
	xor di, di
	lea dx, runtime_print_top_stack
	mov [di], dx
end_call_runtime_print_top_stack:

call_runtime_or:
	xor di, di
	lea dx, runtime_or
	mov [di], dx
end_call_runtime_or:

call_runtime_swap:
	xor di, di
	lea dx, runtime_swap
	mov [di], dx
end_call_runtime_swap:

call_runtime_eq:
	xor di, di
	lea dx, runtime_eq
	mov [di], dx
end_call_runtime_eq:

call_runtime_add:
	xor di, di
	lea dx, runtime_add
	mov [di], dx
end_call_runtime_add:

call_runtime_sub:
	xor di, di
	lea dx, runtime_sub
	mov [di], dx
end_call_runtime_sub:

call_runtime_mul:
	xor di, di
	lea dx, runtime_mul
	mov [di], dx
end_call_runtime_mul:

call_runtime_div:
	xor di, di
	lea dx, runtime_div
	mov [di], dx
end_call_runtime_div:
	
call_runtime_neg:
	xor di, di
	lea dx, runtime_neg
	mov [di], dx
end_call_runtime_neg:

call_runtime_gt:
	xor di, di
	lea dx, runtime_gt
	mov [di], dx
end_call_runtime_gt:

call_runtime_unary_minus:
	xor di, di
	lea dx, runtime_unary_minus
	mov [di], dx
end_call_runtime_unary_minus:
	
runtime_add proc near
	mov ax, bx
	call runtime_pop
	add ax, bx
	call runtime_pop
	call runtime_push
	ret
runtime_add endp

runtime_and proc near
	mov ax, bx
	call runtime_pop
	and ax, bx
	call runtime_pop
	call runtime_push
	ret
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
	call runtime_pop
	div bl
	call runtime_pop
	call runtime_push
 	ret
runtime_div endp

runtime_gt proc near
	mov ax, bx
	call runtime_pop
	cmp ax, bx
	ja _gt
	mov ax, 0000h
	jmp replace
	_gt:
	mov ax, 0001h
	jmp replace
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
	call runtime_pop
	mul bx
	call runtime_pop
	call runtime_push
	ret
runtime_mul endp

runtime_neg proc near
	cmp bx, 0000h
	jz false
	mov ax, 0000h
	jmp replace
	false:
	mov ax, 0001h
	replace:
	call runtime_pop
	call runtime_push
	ret
runtime_neg endp

runtime_or proc near
	mov ax, bx
	call runtime_pop
	or ax, bx
	call runtime_pop
	call runtime_push
	ret
runtime_or endp

runtime_pick proc near
	mov cx, bx
	call runtime_pop
	mov di, si
	sub di, cx
	sub di, cx
	mov ax, [di]
	call runtime_push
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
	call runtime_pop
	sub ax, bx
	call runtime_pop
	call runtime_push
	ret
runtime_sub endp

runtime_swap proc near
	mov ax, bx
	call runtime_pop
	mov dx, bx
	call runtime_pop
	call runtime_push 
	mov ax, dx
	call runtime_push
	ret
runtime_swap endp

runtime_get_value proc near
	mov di, bx
	call runtime_pop
	mov ax, [di]
	call runtime_push
	ret
runtime_get_value endp
	
runtime_push proc near ;pushes ax value to the stack
	add si, 2;
	mov bx, ax
	mov [si], ax;
	ret
runtime_push endp

runtime_pop proc near
	sub si, 2
	mov bx, [si]
	ret
runtime_pop endp

runtime_print_top_stack proc near
	mov  ah,2 
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
	mov ax, bx
	call runtime_pop
	cmp ax, bx
	jz	push_true
	mov ax, 0
	push_value:
	call runtime_pop
	call runtime_push
	ret
	
	push_true:
		mov ax, 1
		jmp push_value
runtime_eq endp

runtime_unary_minus proc near
	mov ax, bx
	call runtime_pop
	neg ax
	call runtime_push
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
		
	mov bx, exechandle
	mov ax, 4000h
	mov cx, end_runtime - begin_runtime
	lea dx, begin_runtime
	int 21h
   
	mov bx, exechandle
	mov ax, 4000h
	mov cx, 3
	lea dx, OPCODE_INIT_SE
	int 21h
	
	mov ax, address_pointer
	mov cx, end_runtime - begin_runtime 
	add ax, cx
	mov address_pointer, ax
	add address_pointer, 3
	
	
	mov ax, 4200h
	xor cx, cx;
	mov dx, address_pointer
	sub dx, 100h 
	mov bx, exechandle
	int 21h; set file pointer, skip 52 bytes for vars
	
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
	jmp default
		
	_0:
	cmp fbuff, 22h ;22H -- "
	jnz _1
	jmp call_proc_quotation
	
	_1:
	cmp flags, 01h;
	jnz _cmp_num
	jmp	call_generate_string
	
	_cmp_num:
	cmp fbuff, 2Fh; 30 -- 0
	jna _not_num
	jmp check_is_num
	
	_num:
	mov flags, 03H
	call proc_num
	ret
		
	_not_num:
	cmp flags, 03H
	jnz _2 
	call parse_num
	ret
	
	_2:
	cmp fbuff, 3Ah ;3Ah -- :
	jnz _3
	
	mov cx, end_call_runtime_assign - call_runtime_assign
	lea dx, call_runtime_assign
	call write_call
	ret
	
	_3:
	cmp fbuff, 3Bh
	jnz _4
	
	mov cx, end_call_runtime_get_value - call_runtime_get_value
	lea dx, call_runtime_get_value
	call write_call
	ret
	
	_4:
	cmp fbuff, 2Eh
	jnz _5
	
	mov cx, end_call_runtime_print_top_stack - call_runtime_print_top_stack
	lea dx, call_runtime_print_top_stack
	call write_call
	ret
	
	_5:
	cmp fbuff, 5Bh ; -- [
	jnz _6
	
	mov ah,	40h 
    mov bx, exechandle 
    mov cx, 2 
    lea dx, OPCODE_JMP
    int 21h ; write 'eb00' to the com-file
	add address_pointer, 2h;
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
	
	mov ax, 4000h
	mov bx, exechandle
	mov cx, 9
	lea dx, OPCODE_XOR_MOV_DI
	int 21h
	add address_pointer, 9
	
	mov ax, 4000h
	mov bx, exechandle
	mov cx, 2
	lea dx, OPCODE_CALL_ABS
	int 21h
	add address_pointer, 2
	ret
	
	_8:
	cmp fbuff, 3Dh ; -- =
	jnz _9
	
	mov cx, end_call_runtime_eq - call_runtime_eq
	lea dx, call_runtime_eq
	call write_call
	ret
	
	_9:
	cmp fbuff, 3Fh ; -- ?
	jnz _10
	
	mov cx, end_call_runtime_if - call_runtime_if
	lea dx, call_runtime_if
	call write_call
	ret
	
	_10:
	cmp fbuff, 23h ; -- #
	jnz _11
	
	mov cx, end_call_runtime_while - call_runtime_while
	lea dx, call_runtime_while
	call write_call
	ret
	
	_11:
	cmp fbuff, 2Bh
	jnz _12
	
	mov cx, end_call_runtime_add - call_runtime_add
	lea dx, call_runtime_add
	call write_call
	ret
	
	_12:
	cmp fbuff, 2Dh
	jnz _13 
	
	mov cx, end_call_runtime_sub - call_runtime_sub
	lea dx, call_runtime_sub
	call write_call
	ret
	
	_13:
	cmp fbuff, 2Ah
	jnz _14 
	
	mov cx, end_call_runtime_mul - call_runtime_mul
	lea dx, call_runtime_mul
	call write_call
	ret
	
	_14:
	cmp fbuff, 2Fh
	jnz _15
	
	mov cx, end_call_runtime_div - call_runtime_div
	lea dx, call_runtime_div
	call write_call
	ret
	
	_15:
	cmp fbuff, 7Eh
	jnz _16
	
	mov cx, end_call_runtime_neg - call_runtime_neg
	lea dx, call_runtime_neg
	call write_call
	ret
	
	_16:
	cmp fbuff, 3Eh
	jnz _17
	
	mov cx, end_call_runtime_gt - call_runtime_gt
	lea dx, call_runtime_gt
	call write_call
	ret
	
	_17:
	cmp fbuff, 26h
	jnz _18
	
	mov cx, end_call_runtime_and - call_runtime_and
	lea dx, call_runtime_and
	call write_call
	ret
	
	_18:
	cmp fbuff, 7Ch
	jnz _19
	
	mov cx, end_call_runtime_or - call_runtime_or
	lea dx, call_runtime_or
	call write_call
	ret
	
	_19:
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
	cmp fbuff, 25h
	jnz _21
	
	mov cx, end_call_runtime_pop - call_runtime_pop
	lea dx, call_runtime_pop
	call write_call
	ret
	
	_21:
	cmp fbuff, 5Ch
	jnz _22
	
	mov cx, end_call_runtime_swap - call_runtime_swap
	lea dx, call_runtime_swap
	call write_call
	ret
	
	_22:
	cmp fbuff, 40h
	jnz _23
	
	mov cx, end_call_runtime_rot - call_runtime_rot
	lea dx, call_runtime_rot
	call write_call
	ret
	
	_23:
	cmp fbuff, 4Fh
	jnz _24
	
	mov cx, end_call_runtime_pick - call_runtime_pick
	lea dx, call_runtime_pick
	call write_call
	ret
	
	_24:
	cmp fbuff, 2Ch
	jnz _25
	
	mov cx, end_call_runtime_comma - call_runtime_comma
	lea dx, call_runtime_comma
	call write_call
	ret
	
	_25:
	cmp fbuff, 5Eh
	jnz _26
	
	mov cx, end_call_runtime_input - call_runtime_input
	lea dx, call_runtime_input
	call write_call
	ret
	
	_26:
	cmp fbuff, 5Fh
	jnz _27
	mov cx, end_call_runtime_unary_minus - call_runtime_unary_minus
	lea dx, call_runtime_unary_minus
	call write_call
	ret
	
	_27:
	cmp fbuff, 60h ; 61 -- a
	ja check_is_var
default:
	mov  ah,2
    int  21H 
	ret

check_is_var:
	cmp fbuff, 7Bh
	jb call_proc_var
	jmp default

check_is_num:
	cmp fbuff, 3Ah
	jnb _fail
	jmp _num
	_fail:
	jmp _not_num
	
call_proc_quotation: 
    call proc_quotation
	ret
	
call_generate_string: 
	call generate_string
	ret
	
call_proc_var:
	call proc_var
	ret
		
call_proc_num:
	call proc_num
	ret
		
call_proc_end:
	call proc_end
	ret
	
proc_symbol endp

proc_quotation proc near
	cmp flags, 00H;
	jnz  end_string
	
	mov flags, 01H;
	mov ah,	40h 
    mov bx, exechandle 
    mov cx, 2 
    lea dx, OPCODE_JMP
    int 21h ; write 'eb00' to the com-file
	add address_pointer, 2h;
	mov si, jmp_labels_pointer;
	mov bx, address_pointer;
	dec bx;
	mov jmp_labels[si], bx;  move to the stack of labels current address for backtracing
	inc jmp_labels_pointer
	ret;
	
end_string:
	mov flags, 00H; clear flags 
	
	mov ah,	40h 
    mov bx, exechandle 
    mov cx, 1
	lea dx, OPCODE_EOF
	int 21h;
	add address_pointer, 1h;
	
	mov ax, 4000h
	mov cx, 1
    lea dx, OPCODE_MOV_DX
	int 21h; write mov dx to the file 
	add address_pointer, 1h;
	
	dec jmp_labels_pointer; backtracing
	mov ax, 4200h
	xor cx, cx;
	lea bx, jmp_labels[si]
	mov dx, [bx]
	sub dx, 100h
	mov bx, exechandle
	int 21h; set file pointer to the unset jmp_label dx should contain 35(!) then 40
	
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
	
	mov ax, 4202h
	xor cx, cx;
	xor dx, dx;
	int 21h; return file pointer back to the eof
	
	mov si, jmp_labels_pointer;
	mov ax, jmp_labels[si]
	inc ax;
	mov jmp_labels[si], ax;
	lea dx, jmp_labels[si]
	mov ax,	4000h
	mov cx, 2
	int 21h; write data address to the file
	add address_pointer, 2h;
	
	mov ah,	40h 
	mov cx, 4
	lea dx, OPCODE_PRINT
	int 21h; write dos int 21h 
	add address_pointer, 4h;
	
	ret;
	
proc_quotation endp

generate_string proc near
	mov ah,	40h 
    mov bx, exechandle 
    mov cx, 1 
    lea dx, fbuff
    int 21h 
	add address_pointer, 1h ;address_pointer to the last written byte
	ret
generate_string endp

proc_var proc near
	
	xor ax, ax;
	mov al, fbuff;
	sub al, 61H;
	mov bx, 0002H;
	mul bx
	add ax, vars_offset;
	mov number, ax
	
	mov bx, exechandle
	mov ax, 4000h
	mov cx, 1
	lea dx, OPCODE_MOV_AX
	int 21h
	
	mov bx, exechandle
	mov ax, 4000h
	mov cx, 2
	lea dx, number
	int 21h
	add address_pointer, 3
	
	call _push
	ret
	
proc_var endp

proc_num proc near

	mov al, fbuff
	sub al, 30H
	mov bytes[di], al
	inc di
	ret
	
proc_num endp 

parse_num proc near
	mov flags, 00H ; clear flags

	mov bx, exechandle
	mov ax, 4000h
	mov cx, 1
	lea dx, OPCODE_MOV_AX
	int 21h
	
	xor ax, ax
	mov dx, 10
	mov cx, di
	xor di, di
	
	proc_byte:
	cmp di, cx
	jz write
	mov bl, bytes[di]
	mul dx
	mov dx, 10
	add ax, bx
	inc di
	jmp proc_byte
	
	write:
	mov number, ax
	mov bx, exechandle
	mov ax, 4000h
	mov cx, 2
	lea dx, number
	int 21h
	
	add address_pointer, 3
	
	call _push
	xor di, di
	ret
parse_num endp

proc_end proc near

	mov ah,	40h 
    mov bx, exechandle 
    mov cx, 1
	lea dx, OPCODE_RET
	int 21h;
	add address_pointer, 1h;
	
	sub jmp_labels_pointer, 2; backtracing
	mov ax, 4200h
	xor cx, cx;
	mov si, jmp_labels_pointer
	lea bx, jmp_labels[si]
	mov dx, [bx]
	sub dx, 100h
	mov bx, exechandle
	int 21h; set file pointer to the unset jmp_label 
	
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
	int 21h ; write backtracing
	
	mov ax, 4202h
	xor cx, cx;
	xor dx, dx;
	int 21h; return file pointer back to the eof

	;datapush address
	mov ax, jmp_labels[si]
	inc ax
	mov number, ax
	
	mov bx, exechandle
	mov ax, 4000h
	mov cx, 1
	lea dx, OPCODE_MOV_AX
	int 21h
	
	mov bx, exechandle
	mov ax, 4000h
	mov cx, 2
	lea dx, number
	int 21h
	
	add address_pointer, 3
	
	call _push
	ret

proc_end endp

write_call proc near
	mov ax, 4000h
	mov bx, exechandle
	int 21h
	add address_pointer, cx
	
	mov ax, 4000h
	mov cx, 2
	lea dx, OPCODE_CALL_ABS
	int 21h
	add address_pointer, cx
	ret
write_call endp

_push proc near
	mov cx, end_call_runtime_push - call_runtime_push
	lea dx, call_runtime_push
	call write_call
	ret
_push endp

closefile proc near
	mov ah,	40h 
    mov bx, exechandle 
    mov cx, 4 
    lea dx, OPCODE_END
	add address_pointer, 4h
    int 21h
    mov  ah,3EH        
    mov  bx,handle     
    int  21H  
	mov  bx,exechandle 
    int  21h
    ret
closefile endp

END

