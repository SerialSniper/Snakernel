[org 0x7c00]

%define gridWidth 40
%define gridHeight 25
%define snake 0x1000

; invert x and y because endianness
mov ah, (gridHeight - 1) / 2 ; y
mov al, (gridWidth - 1) / 2 ; x
mov [snake], ax

call create_apple

main:
    call sleep
    call handle_keys
    call clear_screen
    call update_pos
    
    call draw_apple
    call draw_snake
    
    jmp main

draw_snake:
    mov bx, snake
    xor dx, dx

    draw_loop:
        mov dx, [bx]

        cmp dx, 0
            je return

        push bx
            mov bl, dl
            push gridWidth
            call wrap
            mov dl, bl
        pop bx
        
        push bx
            mov bl, dh
            push gridHeight
            call wrap
            mov dh, bl
        pop bx

        call draw_block

        add bx, 2
        jmp draw_loop

create_apple:
    xor ah, ah
    int 1Ah

    mov ax, 25173
    mul dx
    add ax, 13849

    mov bx, ax
    xor dx, dx
    xor ax, ax

    mov al, bh
    mov ch, gridWidth
    idiv ch
    mov bh, ah

    xor dx, dx
    xor ax, ax

    mov al, bl
    mov ch, gridHeight
    idiv ch
    mov bl, ah

    ; jmp $

    mov [apple_x], bh
    mov [apple_y], bl
    ret

handle_keys:
    ; get key
    mov ah, 1
    int 16h
        jz return
    xor ah, ah
    int 16h
    
    call up
    call down
    call left
    call right
    mov [direction_x], ah
    mov [direction_y], al

    jmp handle_keys

up:
    cmp ah, 0x48
        jne return
    ; ah = direction, al = axis
    mov ax, 0xFF02
    ret

down:
    cmp ah, 0x50
        jne return
    ; ah = direction, al = axis
    mov ax, 0x0102
    ret

left:
    cmp ah, 0x4B
        jne return
    ; ah = direction, al = axis
    mov ax, 0xFF01
    ret

right:
    cmp ah, 0x4D
        jne return
    ; ah = direction, al = axis
    mov ax, 0x0101
    ret

check_collisions:
    mov ah, [snake]
    mov al, [snake + 1]
    cmp ah, [apple_x]
        jne check_collisions_return
    cmp al, [apple_y]
        je eat_apple

    check_collisions_return:
        call pop_block
        ret

    eat_apple:
        call create_apple
        ret

update_pos:
    call push_block

    mov ah, [direction_x]
    mov al, [direction_y]
    cmp al, 1
        je update_x
    cmp al, 2
        je update_y

    update_pos_continue:
        call check_collisions
        ret

push_block:
    pusha
        mov bx, snake
        mov al, [snakeLength]
        xor ah, ah
        mov dl, 2
        mul dl
        add bx, ax
        
        shift_loop:
            cmp bx, snake
                je push_block_continue
            sub bx, 2
                mov ax, word [bx]
            add bx, 2
                mov [bx], ax
            sub bx, 2
            jmp shift_loop
        
        push_block_continue:
            mov bl, [snakeLength]
            inc bl
            mov [snakeLength], bl

            popa
            ret

pop_block:
    pusha
        mov bx, snake
        mov al, [snakeLength]
        xor ah, ah
        dec al
        mov dl, 2
        mul dl
        add bx, ax

        ; mov dx, [bx]
        ; mov bl, 0xC0
        ; call draw_square

        mov [bx], word 0
        
        mov bl, [snakeLength]
        dec bl
        mov [snakeLength], bl
    popa
    ret

update_x:
    mov bl, [snake]
    xor bh, bh
    add bl, ah

    push gridWidth
    call wrap

    mov [snake], bl
    jmp update_pos_continue

update_y:
    mov bl, [snake + 1]
    xor bh, bh
    add bl, ah

    push gridHeight
    call wrap

    mov [snake + 1], bl
    jmp update_pos_continue

wrap:
push bp
mov bp, sp
mov bh, [bp + 4]

wrap_zero:
    cmp bl, 0
        jl wrap_zero_stagetwo
    
    wrap_max:
        sub bh, 1
        cmp bl, bh
            jg wrap_max_stagetwo
        jmp wrap_return

        wrap_max_stagetwo:
            push dx
            push ax
                xor dx, dx
                xor ah, ah
                mov al, bl
                idiv bh
                sub ah, 1
                mov bl, ah
            pop ax
            pop dx
            jmp wrap_return

    wrap_zero_stagetwo:
        add bl, bh
        jmp wrap_zero

    wrap_return:
        leave
        ret 2

clear_screen:
    mov ah, 0x00
    mov al, 0x03
    int 10h
    ; hide cursor
    mov ah, 0x01
    mov cx, 2607h
    int 10h
    ret

sleep:
    mov cx, 0x0001
    mov dx, 0xE848
    mov ah, 86h
    int 15h
    ret

draw_block:
    pusha
        mov bl, 0xF0
        call draw_square
    popa
    ret

draw_apple:
    mov dl, [apple_x] ; grid x
    mov dh, [apple_y] ; grid y

    pusha
        mov bl, 0xC0
        call draw_square
    popa
    ret

draw_square:
    ; scale x
    mov al, 2
    mul dl
    mov dl, al

    ; move cursor
    mov ah, 0x02
    xor bh, bh
    int 10h

    ; actually fill
    mov ah, 0x09
    mov al, ' '
    xor bh, bh
    mov cx, 2
    int 10h

    ret

return:
    ret

; x: db 39/2
; y: db 24/2

apple_x: db 0
apple_y: db 0

direction_x: db 0
direction_y: db 0

snakeLength: db 1

times 510-($-$$) db 0
dw 0xAA55