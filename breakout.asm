;
; Breakout in a Boot Sector for the MSX
; intended for the Glass Z80 assembler
;
; Joe Kennedy 2023
;

; 
; left and right cursor keys to move
; spacebar to restart the game after winning/losing
;
; the ball speeds up after 8, 16, 32 and 64 blocks have been broken
;

WRTVRM: equ 0x004d
SETWRT: equ 0x0053
CLRSPR: equ 0x0069
INIT32: equ 0x006f
SNSMAT: equ 0x0141
PHYDIO: equ 0x144


PADDLE_Y: equ 184
BLOCK_COUNT: equ 16 * 8

VDP.DW: equ 0x0007
NEWKEY: equ 0xfbe5

; 720k disk
SECTOR_SIZE: equ 512			
MEDIA_DESCRIPTOR: equ 0xf9
DISK_SECTORS: equ 1440
DISK_SIZE_BYTES: equ DISK_SECTORS * SECTOR_SIZE
HEAD_COUNT: equ 2
SECTORS_PER_TRACK: equ 9
	
SECTOR_ALIGN: MACRO
	ds SECTOR_SIZE - 1 - ($ + SECTOR_SIZE - 1) % SECTOR_SIZE
	ENDM


    org 0xc000
	
        HEADER: ds 30
        CODE: ds 0x200 - 30
        
    org 0xf000
	
        RAM: ds virtual 0x100

	org 0x0000
	
		SECTORS: ds ((DISK_SECTORS - 1) * SECTOR_SIZE)

    SECTION RAM

        blocks: ds BLOCK_COUNT
        
        ball_dy: ds 2
        ball_dx: ds 2
		ball_y: ds 2
        ball_x: ds 2
        paddle_x: ds 1
		
		lives: ds 1
		break_count: ds 1
		next_break: ds 1
		ball_speed: ds 1
		
    ENDS

	SECTION SECTORS
	
		SECTOR_ALIGN
		
		; code/data/files go here and can be loaded with PHYDIO
		; use SECTOR_ALIGN to keep things aligned for ease of loading with PHYDIO
		; as that reads data in terms of sectors
		
	ENDS

    SECTION HEADER

		; disk header for a 720k dd disk
		; header details from https://www.win.tue.nl/~aeb/linux/fs/fat/fat-1.html
		
		; 8086 jump relative -2 then nop
		; stops PCs from running disk?
		db 0xeb, 0xfe, 0x90
		
		game_over_text:
		db "WIN!LOSE"	; oem name
		dw SECTOR_SIZE	; sector size
		db 2			; sectors per cluster
		dw 1			; number of reserved sectors
		
		db 2	 		; number of FAT copies
		dw 0x0070		; number of root directory entries (FAT)
		dw DISK_SECTORS	; number of sectors in filesystem (1440 * 512 = 720k)
		db MEDIA_DESCRIPTOR	; media descriptor type (this one is for a 720k floppy)
		dw 3			; number of sectors per FAT
		dw SECTORS_PER_TRACK ; sectors per track
		dw HEAD_COUNT	; number of heads (2 for a double-sided diskette)
		dw 0			; number of hidden sectors

    ENDS

	; 256 bytes of the boot sector (including the header) are copied to 0xc000 
	; and this code at 0xc01e is executed:
    SECTION CODE

        di

        ; set stack pointer
        ld sp, 0xf100

		; copy the top 256 of the bytes of boot sector to 0xc100
		; the full 512 byte sector is loaded to a place in memory but only
		; the first 256 bytes are copied to 0xc000 by the bios
		ld hl, (0xf351)
		inc h
		ld bc, 0x0100
		ld de, 0xc100
		ldir

init:

        ; screen 1, 32x24 tiles
        call INIT32

		;call CLRSPR
		
        ; get vdp data write port
        ld a, (VDP.DW)
        ld c, a
        
        ; write to character colour table
        ld hl, 0x2018
        call SETWRT

		; the number 8 will be used a lot in this section!
		; save a few bytes by keeping it in e for the time being
		ld e, 8

		; write out character colours
		; the last 64 characters will be turned into single colour (colours 8 through 15)
        ld b, e
        ld a, 0x88
    write_character_colours:
        out (c), a
        add a, 0x11
        djnz write_character_colours

        ; writing to sprite generator
        ld hl, 0x3800
        call SETWRT

		; setup ball/paddle sprite
        ld b, e
        ld hl, ball_gfx
		otir

		; setup block data
		; should have a different colour every horizontal block
        ld hl, blocks
		ld b, BLOCK_COUNT
	write_block_data:
		add a, e
		or 11000000b
		ld (hl), a
		inc hl
		djnz write_block_data
		
		; point hl at lives variable
		; (don't need to update h as the high byte is the same as the blocks table)
		ld l, lives & 0xff
		ld (hl), 5

		; break_count
		inc hl
		ld (hl), 0
		
		; next_break
		inc hl
		ld (hl), e
		
		; ball_speed
		inc hl
		ld (hl), 1

		; reset ball velocity and position
		call ball_out


		ei
		  
    main_loop:

        halt

		; read keyboard matrix
        ld a, (NEWKEY + 8)

		; update paddle position
        ld hl, paddle_x

    ; check if the left cursor is being pressed
    check_left:
        bit 4, a
        jr nz, check_right

            ; don't move if we're already as left as we can be
            ld a, (hl)
            or a
            jr z, paddle_move_done

                ; move left
                dec (hl)
                dec (hl)
                jr paddle_move_done

    ; check if the right cursor is being pressed
    check_right:
        bit 7, a
        jr nz, paddle_move_done

            ; don't move if we're already as right as we can be
            ld a, (hl)
            cp 256 - 32
            jr z, paddle_move_done

                ; move right
                inc (hl)
                inc (hl)

    paddle_move_done:

		; update ball_y
        ; add ball_dy to ball_y
        ld bc, (ball_dy)
		ld hl, (ball_y)
		add hl, bc
		ld (ball_y), hl
		; keep upper byte of ball_y in d
		ld a, h
		ld d, a
		
        ; collision with top of screen
        cp 8
        jr nc, no_top_collision
		
			; check if ball_dy in bc is negative, invert if it is
			bit 7, b
			call nz, invert_dy

	no_top_collision:			
	
		; check if ball has left play
        cp 192
        call nc, ball_out

		; update ball_x
        ; add ball_dx to ball_x
        ld bc, (ball_dx)
		ld hl, (ball_x)
		add hl, bc
		ld (ball_x), hl
		; keep upper byte of ball_x in e
		ld a, h
		ld e, a
		
        ; collision with left hand side of screen
	check_left_collision:
        cp 8
		jr nc, check_right_collision
		
			; check if ball_dx in bc is negative, invert if it is
			; as we want dx to be positive to push us back into play
			bit 7, b
			call nz, invert_dx
		
        ; collision with right hand side of screen
	check_right_collision:
        cp 248
        jr c, check_paddle_hit
		
			; check if ball_dx in bc is positive, invert if it is
			; as we want dx to be negative to push us back into play
			bit 7, b
			call z, invert_dx

        ; check for a paddle hit
	check_paddle_hit:
        ld a, d
        cp PADDLE_Y
        jr c, no_paddle_hit

			; no hit if ball_x < paddle_x
            ld a, (paddle_x)
            cp e
            jr nc, no_paddle_hit
			
			; no hit if ball_x > paddle_x + 31
            add a, 31
            cp e
            jr c, no_paddle_hit

				; subtract ball x from the end of the paddle x
				; should now be a value between 0 and 31
				; divide by 4 and discard the lowest bit to get the address in the paddle ball angle table
                sub e
				srl a
				srl a
				and 00111110b
				add a, paddle_bounce_angles & 0xff
				
				; point hl at the dy angle address
				ld l, a
				ld h, paddle_bounce_angles >> 8
				
				; keep ball_y and ball_x around in de
				push de
				
				; get bounce dx and store it on the stack
				ld a, (hl)
				inc hl
				push af
				
				; get bounce dy
				ld a, (hl)
				
				; writing to ball_dy/dx
				ld de, ball_dy
				
				bounce_velocity_loop:
				
					; load new ball_dy/dx into bc
					; sign extend c to bc
					ld c, a
					add a, a
					sbc a, a
					ld b, a
					
					; multiply ball_dy/dx by ball_speed
					; and store for later use
					ld a, (ball_speed)
					
					; use hl as 16 bit accumulator
					push bc
					pop hl
				speed_mul:
					add hl, bc
					dec a
					jr nz, speed_mul
					
					; store new ball_dy/dx
					ld a, l
					ld (de), a
					inc de 
					ld a, h
					ld (de), a
					inc de
					
					; check if we're done updating both ball_dy and ball_dx
					ld a, e
					cp (ball_dy + 4) & 0xff
					jr z, bounce_velocity_loop_done
					
					; get bounce dx into a and loop again
					pop af
					jr bounce_velocity_loop
				
			bounce_velocity_loop_done:
			
				; restore ball_y and ball_x to de
				pop de

    no_paddle_hit:
	check_block_hit:
    
        ; get the y coord
        ld a, d

		; y coord of the start of the blocks
		ld b, 2 * 8

        ; are we above the blocks?
        cp b
        jr c, ball_not_in_blocks
		
        ; are we below the blocks?
        cp (2 + 8) * 8
        jr nc, ball_not_in_blocks

            ; subtract the space above the blocks
            and 11111000b
            sub b

            ; make it so the y coord in blocks is in the upper nibble
            rla
			ld b, a
			
            ; divide x by 16 to get x coord in blocks
			ld a, e
            rrca
            rrca
            rrca
            rrca
			and 0xf
            
            ; combine to get address
            or b

            ; address of block now in hl
            ld h, blocks >> 8
            ld l, a

            ; if the block is already 0, do nothing
            ld a, (hl)
            or a
            jr z, ball_not_in_blocks
            
                ; clear block at that location
                xor a
                ld (hl), a

				; increase the number of blocks broken and keep it around for a sec
				; point hl at break_count (h is already set to what we need)
				ld l, break_count & 0xff
				inc (hl)
				ld b, (hl)
				
				; check if break_count == 128
				; in which case we've won
				bit 7, b
				jr nz, game_won
				
				; point at next_break
				inc hl
				
				; check if next_break == break_count
				ld a, (hl)
				cp b
				jr nz, next_break_not_reached
					
					; multiply next_break by 2 and store it
					rla
					ld (hl), a
					
					; point at ball_speed
					inc hl
					; increase ball speed
					inc (hl)
				
				next_break_not_reached:

                ; did we hit the sides or top/bottom of a block?
                ; get the y coordinate of the ball
                ld a, d

                ; if y % 8 == 0 we assume we've hit the top of the block
                and 00000111b
                jr z, blocks_invert_dy

                ; if y % 8 == 7 we assume we've hit the bottom of the block
                cp 00000111b
                jr z, blocks_invert_dy

                ; else we assume we've hit the sides
            blocks_invert_dx:

				call invert_dx
                jr ball_not_in_blocks

            blocks_invert_dy:

				call invert_dy

    ball_not_in_blocks:

        ; point at start of blocks 
        ld hl, 0x1800 + (32 * 2)
        call SETWRT

		ld a, (VDP.DW)
        ld c, a

		; write out the blocks, repeating each block colour once
        ld hl, blocks
        ld b, BLOCK_COUNT * 3
		draw_blocks:
			outi
			dec hl
			outi
			djnz draw_blocks

        ; point at sprite attribute table
        ld hl, 0x1b00
        call SETWRT

        ; ball sprite

		; sprite x & y offset
		; also paddle sprite width counter
		ld b, 4
		
        ; ball y
        ld a, d
        sub b
        out (c), a

        ; ball x
		ld a, e
        sub b
        out (c), a
        
        ; ball tile
        xor a
        out (c), a

        ; palette
        ld a, 0xf
        out (c), a

        ; paddle sprites
		
        ld a, (paddle_x)        
		draw_paddle:
			
			; y coord
			ld d, PADDLE_Y
			out (c), d
			
			; x coord
			nop
			nop
			out (c), a
			
			; sprite pattern
			ld d, 0
			out (c), d
			
			; sprite palette
			ld d, 0xf
			out (c), d
			
			; increase x coord by 8
			add a, 8
			
			djnz draw_paddle		

        jp main_loop

game_lost:

	; this code is jumped to from inside a call to ball_out
	; remove ball_out's return address from stack
	pop hl
	
	ld hl, game_over_text + 4
	jr game_done

game_won:

	ld hl, game_over_text

game_done:

	; keep address of game over text for later
	push hl

	; point at middle of screen
	ld hl, 0x1800 + 14 + (12 * 32)
	call SETWRT
	
	; write message
	ld a, (VDP.DW)
	ld c, a
	ld b, 4
	; restore address of game over text
	pop hl
	otir
	
	game_done_wait:
	
		; read keyboard
		ld a, (NEWKEY + 8)
		
		; wait for space to be pressed
		bit 0, a
		jr nz, game_done_wait
	
	; new game!
	jp init

ball_out:

	; decrease number of lives and game over if it's 0
	ld hl, lives
	dec (hl)
	jr z, game_lost
	
	; copy ball_dy/dx/y/x/paddle_x data
	ld de, ball_dy
	ld hl, ball_out_data
	ld bc, 0x0009
	ldir
	
	; update de with new ball x and y
	ld d, (hl)
	ld e, (hl)

    ret
	
invert_dx:

	ld hl, 0
	ld bc, (ball_dx)
	sbc hl, bc
	ld (ball_dx), hl
	
	ret

invert_dy:

	ld hl, 0
	ld bc, (ball_dy)
	sbc hl, bc
	ld (ball_dy), hl

	ret

	; angles go from hitting the right to hitting the left!
	paddle_bounce_angles:
		;	dx	 dy
		db  118, -48
		db   90, -90
		db  -90, -90
		db -118, -48

    ball_gfx:
        db 00111100b
        db 01111110b
        db 11111111b
        db 11111111b
        db 11111111b
        db 11111111b
        db 01111110b
        db 00111100b
		
	ball_out_data:
		; ball_dy, ball_dx, ball_y, ball_x
		dw 0x0100, 0x0000, 128 << 8, 128 << 8
		; paddle_x, upper byte of ball_y and ball_x
		db 114, 128

    ENDS