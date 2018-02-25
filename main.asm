.modeL TINY
.DATA

filename   			DB	'1.txt',0
execfile   			DB	'1.com',0
exechandle 			DW	?
handle     			DW	?
fbuff      			DB	?
number				DW 	?
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
OPCODE_CALL_ABS		DB	0FFh, 015h
OPCODE_XOR_MOV_DI	DB	033h, 0FFh, 089h, 01Dh, 083h, 0EEh, 002h, 08Bh, 01Ch
OPCODE_RET			DB 	0C3h


.CODE
.STARTUP
begin_runtime:
	jmp end_runtime
	VARS: dw 26 dup(0)

call_runtime_add:
	xor di, di
	lea dx, runtime_add
	mov [di], dx
end_call_runtime_add:
	
call_runtime_assign:
	xor di, di
	lea dx, runtime_assign
	mov [di], dx
end_call_runtime_assign:

call_runtime_div:
	xor di, di
	lea dx, runtime_div
	mov [di], dx
end_call_runtime_div:

call_runtime_eq:
	xor di, di
	lea dx, runtime_eq
	mov [di], dx
end_call_runtime_eq:

call_runtime_get_value:
	xor di, di
	lea dx, runtime_get_value
	mov [di], dx
end_call_runtime_get_value:

call_runtime_if:
	xor di, di
	lea dx, runtime_if
	mov [di], dx
end_call_runtime_if:

call_runtime_mul:
	xor di, di
	lea dx, runtime_mul
	mov [di], dx
end_call_runtime_mul:

call_runtime_print_top_stack:
	xor di, di
	lea dx, runtime_print_top_stack
	mov [di], dx
end_call_runtime_print_top_stack:

call_runtime_push:
	xor di, di
	lea dx, runtime_push
	mov [di], dx
end_call_runtime_push:

call_runtime_sub:
	xor di, di
	lea dx, runtime_sub
	mov [di], dx
end_call_runtime_sub:

call_runtime_while:
	xor di, di
	lea dx, runtime_while
	mov [di], dx
end_call_runtime_while:
	
runtime_add proc near
	mov ax, bx
	call runtime_pop
	add ax, bx
	call runtime_pop
	call runtime_push
	ret
runtime_add endp
	
runtime_assign proc near
	mov di, bx;
	call runtime_pop
	mov [di], bx
	call runtime_pop
	ret
runtime_assign endp

runtime_div proc near
	mov ax, bx
	call runtime_pop
	div bl
	call runtime_pop
	call runtime_push
	ret
runtime_div endp

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

runtime_get_value proc near
	mov di, bx
	call runtime_pop
	mov ax, [di]
	call runtime_push
	ret
runtime_get_value endp

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

runtime_mul proc near
	mov ax, bx
	call runtime_pop
	mul bx
	call runtime_pop
	call runtime_push
	ret
runtime_mul endp

runtime_pop proc near
	sub si, 2
	mov bx, [si]
	ret
runtime_pop endp

runtime_print_top_stack proc near
	mov  ah,2
	mov  dx, bx
	add  dx, 30h
    int  21H 
	ret
runtime_print_top_stack endp

runtime_push proc near ;pushes ax value to the stack
	add si, 2;
	mov bx, ax
	mov [si], ax;
	ret
runtime_push endp

runtime_sub proc near
	mov ax, bx
	call runtime_pop
	sub ax, bx
	call runtime_pop
	call runtime_push
	ret
runtime_sub endp
	
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
	
end_runtime:

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
    lea  dx, execfile
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
   
	mov ax, address_pointer
	mov cx, end_runtime - begin_runtime 
	add ax, cx
	mov address_pointer, ax
	
	mov ax, 4200h
	xor cx, cx;
	mov dx, address_pointer
	sub dx, 100h ; if dos file is loaded to 100h
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
	cmp fbuff, 22h ;22H -- "
	jnz _1
	jmp call_proc_quotation
	_1:
	cmp flags, 01h;
	jnz _2
	jmp call_generate_string
	_2:
	cmp fbuff, 3Ah ;3Ah -- :
	jnz _3
	jmp call_proc_colon
	_3:
	cmp fbuff, 3Bh
	jnz _4
	jmp call_proc_semicolon
	_4:
	cmp fbuff, 2Eh
	jnz _5
	jmp call_proc_dot
	_5:
	cmp fbuff, 5Bh ; -- [
	jnz _6
	jmp call_proc_begin
	_6:
	cmp fbuff, 5Dh ; -- ]
	jnz _7
	jmp call_proc_end
	_7:
	cmp fbuff, 21h ; -- !
	jnz _8
	jmp call_proc_apply
	_8:
	cmp fbuff, 3Dh ; -- =
	jz call_proc_eq
	cmp fbuff, 3Fh ; -- ?
	jz call_proc_if
	cmp fbuff, 23h ; -- #
	jz call_proc_while
	cmp fbuff, 2Bh ; -- +
	jz call_proc_add
	cmp fbuff, 2Dh
	jz call_proc_sub
	cmp fbuff, 2Ah
	jz call_proc_mul
	cmp fbuff, 2Fh
	jz call_proc_div
	cmp fbuff, 60h ; 61 -- a
	ja check_is_var
	cmp fbuff, 2Fh; 30 -- 0
	ja check_is_num
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
	jb call_proc_num
	jmp default

call_proc_quotation: 
    call proc_quotation
	ret	
	
call_proc_colon:
	call proc_colon
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
	
call_proc_semicolon:
	call proc_semicolon
	ret
	
call_proc_dot:
	call proc_dot
	ret
	
call_proc_begin:
	call proc_begin
	ret
	
call_proc_end:
	call proc_end
	ret

call_proc_apply:
	call proc_apply
	ret

call_proc_eq:	
	call proc_eq
	ret
	
call_proc_if:
	call proc_if
	ret
	
call_proc_while:
	call proc_while
	ret
	
call_proc_add:
	call proc_add
	ret

call_proc_sub:
	call proc_sub
	ret

call_proc_mul:
	call proc_mul
	ret

call_proc_div:
	call proc_div
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
	add address_pointer, 1h
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

	xor ax, ax;
	mov al, fbuff;
	sub al, 30H;
	mov number, ax;
	
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
	
proc_num endp 

proc_colon proc near
	
	mov cx, end_call_runtime_assign - call_runtime_assign
	lea dx, call_runtime_assign
	call write_call
	ret
	
proc_colon endp

proc_semicolon proc near
	
	mov cx, end_call_runtime_get_value - call_runtime_get_value
	lea dx, call_runtime_get_value
	call write_call
	ret
	
proc_semicolon endp

proc_dot proc near

	mov cx, end_call_runtime_print_top_stack - call_runtime_print_top_stack
	lea dx, call_runtime_print_top_stack
	call write_call
	ret
	
proc_dot endp

proc_begin proc near

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

	ret
proc_begin endp

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

proc_apply proc near
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
	
proc_apply endp

proc_eq proc near
	mov cx, end_call_runtime_eq - call_runtime_eq
	lea dx, call_runtime_eq
	call write_call
	ret
proc_eq endp

proc_if proc near
	mov cx, end_call_runtime_if - call_runtime_if
	lea dx, call_runtime_if
	call write_call
	ret
proc_if endp

proc_while proc near
	mov cx, end_call_runtime_while - call_runtime_while
	lea dx, call_runtime_while
	call write_call
	ret
proc_while endp

proc_add proc near
	mov cx, end_call_runtime_add - call_runtime_add
	lea dx, call_runtime_add
	call write_call
	ret
proc_add endp

proc_sub proc near
	mov cx, end_call_runtime_sub - call_runtime_sub
	lea dx, call_runtime_sub
	call write_call
	ret
proc_sub endp

proc_mul proc near
	mov cx, end_call_runtime_mul - call_runtime_mul
	lea dx, call_runtime_mul
	call write_call
	ret
proc_mul endp

proc_div proc near
	mov cx, end_call_runtime_div - call_runtime_div
	lea dx, call_runtime_div
	call write_call
	ret
proc_div endp

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

