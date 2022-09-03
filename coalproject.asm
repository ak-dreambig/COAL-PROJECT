;Connect 4


;                                   TO-DO

;**********************CHECK ( HORIZONTAL, VERTICAL, DIAGONAL) AFTER USER INPUTS**********************
;**********************VERICAL DIFFERENCE IS +-14 **********************

[org 0x0100]

jmp start

    clrscr:     
                push es
                push ax
                push di

                mov  ax, 0xb800
                mov  es, ax
                mov  di, 0

                nextloc:
                            mov  word [es:di], 0x0720
                            add  di, 2
                            cmp  di, 4000
                            jne  nextloc

                pop  di 
                pop  ax
                pop  es
                ret

    printnum: 
                push bp
                mov bp, sp

                push es
                push ax
                push bx
                push cx
                push dx

                mov ax, 0xb800
                mov es, ax              ; point es to video base
                mov ax, [bp+4]          ; load number in ax
                mov bx, 10              ; use base 10 for division
                mov cx, 0               ; initialize count of digits

                cmp ax, 0
                je blue

                cmp ax, 1
                je green

                cmp ax, 2
                je red

                red:

                    nextdigit1: 
                                mov dx, 0           ; zero upper half of dividend
                                div bx              ; divide by 10
                                add dl, 0x30        ; convert digit into ascii value
                                push dx             ; save ascii value on stack
                                inc cx              ; increment count of values
                                cmp ax, 0           ; is the quotient zero
                                jnz nextdigit1       ; if no divide it again

                    nextpos1: 
                                pop dx              ; remove a digit from the stack
                                mov dh, 0x44        ; use normal attribute
                                mov [es:di], dx     ; print char on screen
                                add di, 2           ; move to next screen location
                                loop nextpos1        ; repeat for all digits on stack
                                jmp stop

                green:
                
                    nextdigit2: 
                                mov dx, 0           ; zero upper half of dividend
                                div bx              ; divide by 10
                                add dl, 0x30        ; convert digit into ascii value
                                push dx             ; save ascii value on stack
                                inc cx              ; increment count of values
                                cmp ax, 0           ; is the quotient zero
                                jnz nextdigit2       ; if no divide it again

                    nextpos2: 
                                pop dx              ; remove a digit from the stack
                                mov dh, 0x22      ; use normal attribute
                                mov [es:di], dx     ; print char on screen
                                add di, 2           ; move to next screen location
                                loop nextpos2        ; repeat for all digits on stack
                                jmp stop

                blue:

                    nextdigit3: 
                                mov dx, 0           ; zero upper half of dividend
                                div bx              ; divide by 10
                                add dl, 0x30        ; convert digit into ascii value
                                push dx             ; save ascii value on stack
                                inc cx              ; increment count of values
                                cmp ax, 0           ; is the quotient zero
                                jnz nextdigit3       ; if no divide it again

                    nextpos3: 
                                pop dx              ; remove a digit from the stack
                                mov dh, 0x010        ; use normal attribute
                                mov [es:di], dx     ; print char on screen
                                add di, 2           ; move to next screen location
                                loop nextpos3        ; repeat for all digits on stack
                                jmp stop
                stop:
                    pop dx
                    pop cx
                    pop bx
                    pop ax
                    pop es
                    pop bp
                    ret 2

    printstr: 
                push bp
                mov bp, sp

                push es
                push ax
                push cx
                push si
                push di

                mov ax, 0xb800
                mov es, ax          ; point es to video base
                mov di, 2250 
                mov si, [bp+6]      ; point si to string
                mov cx, [bp+4]      ; load length of string in cx
                mov ah, 0x07        ; normal attribute fixed in al

                nextchar: 
                            mov al, [si]        ; load next char of string
                            mov [es:di], ax     ; show this char on screen
                            add di, 2           ; move to next screen location
                            add si, 1           ; move to next char in string
                            loop nextchar       ; repeat the operation cx times

                pop di
                pop si
                pop cx
                pop ax
                pop es
                pop bp
                ret 4

    printerror: 
                push bp
                mov bp, sp

                push es
                push ax
                push cx
                push si
                push di

                mov ax, 0xb800
                mov es, ax          ; point es to video base
                mov di, 2570 
                mov si, [bp+6]      ; point si to string
                mov cx, [bp+4]      ; load length of string in cx
                mov ah, 0x07        ; normal attribute fixed in al

                nextchar1: 
                            mov al, [si]        ; load next char of string
                            mov [es:di], ax     ; show this char on screen
                            add di, 2           ; move to next screen location
                            add si, 1           ; move to next char in string
                            loop nextchar1       ; repeat the operation cx times

                pop di
                pop si
                pop cx
                pop ax
                pop es
                pop bp
                ret 4

    print_loop:

                push ax
                push bx
                push cx
                push di

                mov bx, 0
                mov cx, 7
                mov di, 350

                L1:
                    mov ax, [row1 + bx] 
                    push ax 
                    call printnum

                    add bx, 2
                    add di, 10
                    
                    loop L1

                mov bx, 0
                mov cx, 7
                mov di, 670

                L2:
                    mov ax, [row2 + bx] 
                    push ax 
                    call printnum

                    add bx, 2
                    add di, 10
                    
                    loop L2

                mov bx, 0
                mov cx, 7
                mov di, 990

                L3:
                    mov ax, [row3 + bx] 
                    push ax 
                    call printnum

                    add bx, 2
                    add di, 10
                    
                    loop L3
                
                mov bx, 0
                mov cx, 7
                mov di, 1310

                L4:
                    mov ax, [row4 + bx] 
                    push ax 
                    call printnum

                    add bx, 2
                    add di, 10
                    
                    loop L4
                
                mov bx, 0
                mov cx, 7
                mov di, 1630

                L5:
                    mov ax, [row5 + bx] 
                    push ax 
                    call printnum

                    add bx, 2
                    add di, 10
                    
                    loop L5

                mov bx, 0
                mov cx, 7
                mov di, 1950

                L6:
                    mov ax, [row6 + bx] 
                    push ax 
                    call printnum

                    add bx, 2
                    add di, 10
                    
                    loop L6

                pop di
                pop cx
                pop bx
                pop ax

                ret

    insertion:

                push bp
                mov bp, sp

                push ax
                push bx
                push cx
                push dx
                push di

                mov ax, [bp+4]          ; translate user input to column number
                mov dx, 2
                mul dx
                sub ax, 2

                mov bx, ax              ; column number
                mov dx, [bp+6]          ; disc ( 1 = player1, 2 = player2)

                mov ax, 0

                r6:

                    cmp [row6+bx], ax
                    jnz r5

                    mov [row6+bx], dx
                    mov di, row6
                    mov [row_n], di
                    jmp horizontal_check

                r5:

                    cmp [row5+bx], ax
                    jnz r4

                    mov [row5+bx], dx
                    mov di, row5
                    jmp horizontal_check

                r4:

                    cmp [row4+bx], ax
                    jnz r3

                    mov [row4+bx], dx
                    mov di, row4
                    jmp horizontal_check

                r3:

                    cmp [row3+bx], ax
                    jnz r2

                    mov [row3+bx], dx
                    mov di, row3
                    jmp vertical_check3

                r2:

                    cmp [row2+bx], ax
                    jnz r1

                    mov [row2+bx], dx
                    mov di, row2
                    jmp vertical_check2

                r1:

                    cmp [row1+bx], ax
                    jnz filled

                    mov [row1+bx], dx
                    mov di, row1
                    jmp vertical_check1
                
                filled:

                        ; **********************CODE FOR FILLED COLUMN**********************
                        mov ax, error_message
                        push ax
                        push word [error_message_length]
                        call printerror

                        ; Taking user input again
                        xor ax, ax

                        mov ah, 0x1
                        int 0x21

                        ; Storing values and jumping to start of subroutine checks again

                        sub al, 48
                        xor dx, dx
                        mov dl, al
                        xor ax, ax
                        mov ax, dx
                        mov dx, 2
                        mul dx
                        sub ax, 2
                        mov bx, ax
                        mov dx, [bp+6]
                        mov ax, 0
                        jmp r6

                vertical_win:
                        mov bx, ax
                        cmp cx, 3
                        jnz horizontal_check
                        
                        win:
                        call clrscr

                        cmp dx, 1
                        jnz p2wins

                            mov ax, player_1_win_message
                            push ax
                            push word [player_1_win_message_length]
                            call printstr
                            mov ah, 0x1
                            int 0x21
                            jmp end

                        p2wins:

                            mov ax, player_2_win_message
                            push ax
                            push word [player_2_win_message_length]
                            call printstr
                            mov ah, 0x1
                            int 0x21
                            jmp end

                        

                end:
                        pop di
                        pop dx
                        pop cx
                        pop bx
                        pop ax
                        pop bp

                        ret 4

                horizontal_check:
                        xor cx, cx
                        xor bx, bx

                        column1_loop:
                            cmp [di+bx], dx
                            jnz column1_check
                            inc cx
                            add bx, 2
                            jmp column1_loop
                        
                        column1_check:
                            cmp cx, 4
                            jnz column2
                            jmp win

                        column2:
                            xor cx, cx
                            xor bx, bx

                        column2_loop:
                            cmp [di+bx+2], dx
                            jnz column2_check
                            inc cx
                            add bx, 2
                            jmp column2_loop

                        column2_check:
                            cmp cx, 4
                            jnz column3
                            jmp win

                        column3:
                            xor cx, cx
                            xor bx, bx

                        column3_loop:
                            cmp [di+bx+4], dx
                            jnz column3_check
                            inc cx
                            add bx, 2
                            jmp column3_loop

                        column3_check:
                            cmp cx, 4
                            jnz column4
                            jmp win

                        column4:
                            xor cx, cx
                            xor bx, bx

                        column4_loop:
                            cmp [di+bx+6], dx
                            jnz column4_check
                            inc cx
                            add bx, 2
                            jmp column4_loop

                        column4_check:
                            cmp cx, 4
                            jnz end
                            jmp win

                vertical_check1:
                        xor cx, cx
                        mov ax, bx              ; Store bx in ax

                        row1_loop:
                            add bx, 14
                            cmp dx, [row1+bx]
                            jnz vertical_win
                            inc cx
                            jmp row1_loop
                
                vertical_check2:
                        xor cx, cx
                        mov ax, bx              ; Store bx in ax

                        row2_loop:
                            add bx, 14
                            cmp dx, [row2+bx]
                            jnz vertical_win
                            inc cx
                            jmp row2_loop

                vertical_check3:
                        xor cx, cx
                        mov ax, bx              ; Store bx in ax

                        row3_loop:
                            add bx, 14
                            cmp dx, [row3+bx]
                            jnz vertical_win
                            inc cx
                            jmp row3_loop
                        

start: 

        xor cx, cx
        mov cx, 0

        outerloop:

            call clrscr
            call print_loop

            mov ax, input_message
            push ax
            push word [input_message_length]
            call printstr

            xor ax, ax

            ; wait for keypress 
            mov ah, 0x1         ; input char is 0x1 in ah 
            int 0x21            ; input is stored in al

            sub al, 48          ; ascii to int

            cmp al, 0
            je exit             ; quit program if user inputs zero

            xor dx, dx
            mov dl, al          ; Column Number
            
            mov bx, cx          ; player 1 or player 2
            shr bx, 1           ; even = player 1, odd = player 2
            jc player2

            player1:
                mov ax, 1
                jmp insert

            player2:
                mov ax, 2
                jmp insert

            insert:
                push ax
                push dx
                call insertion

            inc cx
            jmp outerloop

        exit:

            mov ax, 0x4c00 
            int 0x21


    row1:   dw 0, 0, 0, 0, 0, 0, 0
    row2:   dw 0, 0, 0, 0, 0, 0, 0
    row3:   dw 0, 0, 0, 0, 0, 0, 0
    row4:   dw 0, 0, 0, 0, 0, 0, 0
    row5:   dw 0, 0, 0, 0, 0, 0, 0
    row6:   dw 0, 0, 0, 0, 0, 0, 0

    input_message:                  db 'Where do you want to drop the disc. 1 - 7 (Input column number). 0 (Exit).'
    input_message_length:           dw 74
    
    error_message:                  db 'Try again. Column is already filled!'
    error_message_length:           dw 36

    player_1_win_message:           db 'Player 1 Wins!'
    player_1_win_message_length:    dw 14

    player_2_win_message:           db 'Player 2 Wins!'
    player_2_win_message_length:    dw 14