.model tiny
.code
org 100h
start:
;---clear screen-------------------------------------------------
	mov ah, 06h
	xor al, al
	xor bh, bh
	xor cx, cx
	mov dh, 25 - 2
	mov dl, 80 - 2
	int 10h
;----------------------------------------------------------------

;---init random numbers generation-----------------
	mov al, 0B4h
	out 43h, al
	mov al, 0FFh
	out 42h, al
	out 42h, al
	in al, 61h
	or al, 1
	out 61h, al
;----------------------------------------------------------------

;---install keyboard interrupt handler---------------------------
	;mov ax, 3509h
	;int 21h
	;mov WORD PTR old_keyboard_handler + 2, es
	;mov WORD PTR old_keyboard_handler, bx
	;
	;lea dx, keyboard_handler
	;mov ah, 25h
	;int 21h
;----------------------------------------------------------------

	mov ax, 0B800h
	mov es, ax
	
;---make fence---------------------------------------------------
	xor bx, bx
	xor cx, cx
	mov cl, road_pos
	fence_loop_0:
		mov es:[bx], BLOCK
		add bx, 2
	loop fence_loop_0
	
	add bx, road_w * 2
	mov cx, 80 - road_w
	sub cl, road_pos
	fence_loop_1:
		mov es:[bx], BLOCK
		add bx, 2
	loop fence_loop_1
;----------------------------------------------------------------

	main_loop:
		call draw_road
		call draw_obstacles
		call draw_car
		call check_loss 
		call keyboard_handler
		call move
		call draw_score
		cmp loss, 1
		je car_broken
	jmp main_loop
	
	car_broken:
	
	mov ax, 2509h
	mov ds, WORD PTR old_keyboard_handler + 2
	mov dx, WORD PTR cs:old_keyboard_handler
	int 21h
		
	mov ax, 4C00h
	int 21h
	
rand PROC	; ax - random number
	mov al, 80h
	out 43h, al
	in al, 42h
	shl ax, 8
	in al, 42h
	xchg ah, al
	ret
rand ENDP

draw_road PROC
	push ax
	push bx

;---get random number from -1 to 1-------------------------------
	call rand
	xor ah, ah
	mov bl, 3
	div bl
	sub ah, 1
;----------------------------------------------------------------

	add road_pos, ah

;---bounds checks------------------------------------------------
	cmp road_pos, 0
	jge draw_road_l_bound
		mov road_pos, 0
	draw_road_l_bound:
	cmp road_pos, 80 - road_w
	jl draw_road_r_bound
		mov road_pos, 80 - road_w - 1
	draw_road_r_bound:
;----------------------------------------------------------------
	
	mov ax, BLOCK
	xor bx, bx
	mov bl, road_pos
	shl bl, 1
	mov es:[bx], ax
	mov es:[bx] + 2 * road_w, ax
	
	pop bx
	pop ax
	ret
draw_road ENDP

draw_car PROC
	push bx
	
;---check moving-------------------------------------------------
	cmp moving_l, 1
	jne not_moving_l 
	    mov moving_l, 0
		dec car_pos
	not_moving_l:
	
	cmp moving_r, 1
	jne not_moving_r
	    mov moving_r, 0
		inc car_pos
	not_moving_r:
;----------------------------------------------------------------

;---bounds check-------------------------------------------------------------
	cmp car_pos, 0
	jge draw_car_l_bound
		mov car_pos, 0
	draw_car_l_bound:
	cmp car_pos, 80 - car_w
	jle draw_car_r_bound
		mov car_pos, 80 - car_w
	draw_car_r_bound:
;----------------------------------------------------------------
	
	mov bx, 80 * CAR_ROW
	add bx, car_pos
	shl bx, 1
	
	mov es:[bx],     03C00h
	mov es:[bx] + 2, 03C1Eh
	mov es:[bx] + 4, 03C00h
	
	pop bx
	ret
draw_car ENDP

move PROC
	push ax
	push bx
	push cx
	push dx

;---delay (~24 fps)----------------------------------------------
	mov ah, 86h
	mov dx, 0C2C2h
	mov cx, 0
	int 15h
;----------------------------------------------------------------
	
;---remove car from screen---------------------------------------
	mov ax, 00FEh
	mov bx, 80 * CAR_ROW
	add bx, car_pos
	shl bx, 1
	mov es:[bx], ax
	mov es:[bx] + 2, ax
	mov es:[bx] + 4, ax
;----------------------------------------------------------------

;---move screen--------------------------------------------------
	mov ah, 07h
	mov al, 1
	xor bh, bh
	xor cx, cx
	mov dh, 25 - 1
	mov dl, 80 - 1
	int 10h
;----------------------------------------------------------------
	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
move ENDP
;------------------------------------- XT-scan codes-----------------------
keyboard_handler PROC
	push ax
	
	A_KEY EQU 1Eh
	D_KEY EQU 20h
	           
	mov ax, 0100h
	int 16h 
	mov ax, 0000h    
	jz buf_empty      
	mov ax, 0000h
	int 16h
	buf_empty:
	cmp ah, A_KEY
	jne not_a
		mov moving_l, 1
		jmp keyboard_handler_end
	not_a:
	
	cmp ah, D_KEY
	jne not_d
		mov moving_r, 1
		jmp keyboard_handler_end
	not_d:
	
	;cmp al, A_KEY + 80h
	;jne not_a_released
		;mov moving_l, 0
		;jmp keyboard_handler_end
	;not_a_released:
	
	;cmp al, D_KEY + 80h
	;jne not_d_released
		;mov moving_r, 0
		;jmp keyboard_handler_end
	;not_d_released:
	
	keyboard_handler_end:
	mov al, 20h
	out 20h, al
	pop ax
	ret
keyboard_handler ENDP

check_loss PROC
	push ax
	push bx
	
	mov bx, 80 * (CAR_ROW - 1)
	add bx, car_pos
	shl bx, 1
	
	mov ax, es:[bx]
	cmp ax, BLOCK
	je check_loss_ouch
	mov ax, es:[bx] + 2
	cmp ax, BLOCK
	je check_loss_ouch
	
	mov ax, es:[bx] + 4
	cmp ax, BLOCK
	jne check_loss_end
	
	check_loss_ouch:
		mov loss, 1
	
	check_loss_end:
	pop bx
	pop ax
	ret
check_loss ENDP

draw_obstacles PROC
	push ax
	push bx
	push cx

;---set bx to the first char after left border of road-----------
	xor bx, bx
	mov bl, road_pos
	inc bx
	shl bx, 1         ; fast mutiply to x2
;----------------------------------------------------------------

;----------------------------------------------------------------
;draw obstacle between road borders
;with chance 1/128 for each char between borders
;----------------------------------------------------------------
	mov cx, road_w - 1
	draw_obstacles_loop:
		call rand
		and ax, 01111111b
		jnz no_obstacle
			mov es:[bx], BLOCK
		no_obstacle:
		add bx, 2
	loop draw_obstacles_loop
;----------------------------------------------------------------
	
	pop cx
	pop bx
	pop ax
	ret
draw_obstacles ENDP

draw_score PROC
	push ax
	push bx
	push cx
	push di
	
	mov bx, 80 * (CAR_ROW + 1) * 2
	mov cx, 80
	draw_score_loop_0:
		cmp WORD PTR es:[bx], BLOCK
		jne no_cost
			inc score
		no_cost:
		add bx, 2
	loop draw_score_loop_0
	
	mov ax, score
	mov bx, 10
	mov cx, 5
	mov di, (80 * 25 - 1) * 2
	draw_score_loop_1:
		xor dx, dx
		div bx
		add dl, '0'
		mov dh, 074h
		mov es:[di], dx
		sub di, 2
	loop draw_score_loop_1
	
	mov ax, offset sscore
	mov si, ax
	lea bx, sscore
	mov ah, 074h 
	sub di, 0Ch  
	mov cx, 14   
	REP MOVSB                    ; String operation!
	
	draw_score_end:
	pop di
	pop cx
	pop bx
	pop ax
	ret
draw_score ENDP

BLOCK						EQU		44FEh
road_pos					db		30
road_w						EQU		20
car_pos						dw		40
car_w						EQU		3
CAR_ROW						EQU		20
old_keyboard_handler		dd		(?)
moving_l					db		0
moving_r					db		0
loss						db		0
score						dw		0
sscore						db	    "S",074h,"c",074h,"o",074h,"r",074h,"e",074h,":",074h," ",074h, 0
score_msg					EQU		$ - 1    


END start
