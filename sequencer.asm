;
; Three Channel Step Sequencer in a Boot Sector for the MSX
; intended for the Glass Z80 assembler
;
; Joe Kennedy 2023
;

; controls:
; move edit cursor: cursor keys
; change note: hold space and press Up or Down
; change octave: hold space and press Left or Right
; change playback speed: Ins or Home to speed playback up or down
; change pattern length: hold space and press Ins or Home to change between 2/4/8/16/32 steps

FORCLR equ 0xf3e9
BAKCLR equ 0xf3ea
BDRCLR equ 0xf3eb

ENASLT equ 0x0024
RDVRM equ 0x4a
WRTVRM equ 0x4d
SETWRT equ 0x53
CHGCLR equ 0x62
INIT32 equ 0x6f
SETGRP equ 0x7e
GRPPRT equ 0x8d
CHPUT  equ 0xa2
CLS equ 0xc3
POSIT  equ 0xc6
RSLREG equ 0x138
SNSMAT equ 0x141
PHYDIO equ 0x144

GICINI equ 0x0090
WRTPSG equ 0x0093

EXPTBL: equ 0xfcc1
SLTTBL: equ 0xfcc5

IO_VDP_DATA equ 0x98
IO_VDP_CONTROL equ 0x99

BIOS_IO_VDP_DATA equ 0x0007

SIZE_CHANNEL equ 64
SIZE_ROW equ 16

BOOTSTRAP_ORIGIN equ 0xc000

SPRITE_ATTRIBUTE equ 0x1b00
SPRITE_GENERATOR equ 0x3800

VRAM_SEQUENCER equ 0x1820
NOTE_REST equ 12

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
	
		; bootstrap sector is 512 bytes long
		; disk boot header is 30 bytes long
		HEADER: ds 0x1e
		CODE: ds 0x200 - 0x1e
		
	org 0xd000
	
		RAM: ds virtual 0x1000
		
	org 0x0000
	
		SECTORS: ds ((DISK_SECTORS - 1) * SECTOR_SIZE)

	SECTION RAM
	
		; keep the channels byte aligned
		channels:
			channel_padding: ds 2 * 32
			channel1: ds 2 * 32
			channel2: ds 2 * 32
			channel3: ds 2 * 32
		channels_end:
				
		update_psg_volume_reg: ds 1
		update_psg_pitch_reg: ds 1
		
		last_snsmat: ds 1
	ENDS
	
	SECTION SECTORS
		SECTOR_ALIGN
	ENDS

	SECTION HEADER
	
		; disk header for a 720k dd disk
		; header details from https://www.win.tue.nl/~aeb/linux/fs/fat/fat-1.html
		
		; 8086 jump relative -2 then nop
		; stops PCs from running disk?
		db 0xeb, 0xfe, 0x90
		
		db "JOE SEQ "	; oem name
		dw 512	; sector size
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
		
		bootstrap_start:
		
			di
			
			; at this point the slot config should be like 
			; page 0: bios
			; page 1: disk bios
			; page 2: ram
			; page 3: ram
			
			; set stack pointer as the disk rom sets it to 0xc200 when it's loading the bootstrap
			ld sp, 0xf100
			
			; copy the top 256 of the bytes of boot sector to 0xc100
			; the full 512 byte sector is loaded to a place in memory but only
			; the first 256 bytes are copied to 0xc000 by the bios
			ld hl, (0xf351)
			inc h
			ld bc, 0x0100
			ld de, 0xc100
			ldir
			
			; SCREEN1 - 32x24 text mode
			call INIT32

			; poke in the vdp io port number from bios rather than hardcoding
			ld a, (BIOS_IO_VDP_DATA)
			ld c, a
			ld (poke_io_vdp_data_value + 1), a

			; set palettes
			ld hl, 0x2005
			call SETWRT

			ld a, 0xf2
			out (c), a
			ld a, 0x74
			out (c), a
			ld a, 0xf6
			out (c), a

			; disable noise, enable squares
			ld a, 7
			ld e, 00111000b
			call WRTPSG
			
			; initialise all channels
			ld b, (SIZE_CHANNEL * 4) / 2
			ld hl, channels
			init_channels:
				ld (hl), NOTE_REST
				inc hl
				ld (hl), 4
				inc hl
				djnz init_channels

			ei
			
		main_loop:
		
			halt

			; get snsmat with the keys we're after and copy it to b for later
			ld a, 8
			call SNSMAT
			ld b, a
			
			; get last snsmat value and xor/cpl/or it with the current one
			; which should only leave the bits which are 0 this frame but not last frame as 0
			ld hl, last_snsmat
			ld a, (hl)
			xor b
			cpl
			or b
			
			;update last_snsmat with the current snsmat
			ld (hl), b
		
			; keep this frame's snsmat in c
			ld c, a
		
			; is space being held?
			bit 0, b
			jr z, space_held
			
		space_not_held:
			
			; keys to adjust playback speed
			ld hl, playback_speed
			
			; slow down playback
			check_home:
			bit 1, c
			jr nz, check_insert
				
				inc (hl)
			
			; speed up playback
			check_insert:
			bit 2, c
			jr nz, check_cursors
			
				dec (hl)
			
			; get low byte of cursor pointer
			check_cursors:
			
			ld hl, cursor_ptr
			ld a, (hl)
			ld b, SIZE_CHANNEL

			check_left:
				bit 4, c
				jr nz, check_right
				
					; move cursor left
					dec a
					dec a
				
			check_right:
				bit 7, c
				jr nz, check_up
				
					; move cursor right
					inc a
					inc a		

			check_up:
				bit 5, c
				jr nz, check_down
				
					; move cursor up
					sub b
				
			check_down:
				bit 6, c
				jr nz, update_cursor_ptr
				
					; move cursor down
					add a, b			
						
			update_cursor_ptr:
			
				jr nc, update_cursor_ptr_check_up
					
					; we've gone off the bottom of channel 3
					add a, SIZE_CHANNEL + SIZE_ROW
					jr update_cursor_ptr_done
			
				update_cursor_ptr_check_up:
				cp b
				jr nc, update_cursor_ptr_done 
				
					; we've gone off the top of channel 1
					sub SIZE_CHANNEL + SIZE_ROW
				
				; update low byte of cursor pointer
				update_cursor_ptr_done:
				ld (hl), a
				jr input_done

		space_held:
		
			; keys to adjust loop length
			ld hl, loop_length
			ld a, (hl)
			
			; decrease loop length
			check_space_and_home:
			bit 1, c
			jr nz, check_space_and_insert
				
				srl a
			
			; increase loop length
			check_space_and_insert:
			bit 2, c
			jr nz, check_space_and_cursors
			
				sla a
			
			; get low byte of cursor pointer
			check_space_and_cursors:
			or 1
			and 31
			ld (hl), a
		
			; put address which cursor is pointing at into hl
			ld hl, (cursor_ptr)
		
			; increase note
			check_space_and_up:
				bit 5, c
				jr nz, check_space_and_down
				
					inc (hl)
			
			; decrease note
			check_space_and_down:
				bit 6, c
				jr nz, input_done_note_updated
				
					dec (hl)
						
			input_done_note_updated:
				
				; sanitize note
				; point hl at the note
				ld a, (hl)
				cp NOTE_REST + 1
				jr z, note_upper_bound
				jr c, check_octave_change
				
					; wrap note around to NOTE_REST
					ld (hl), NOTE_REST
					jr check_octave_change
					
				note_upper_bound:
				
					; wrap note around to 0
					ld (hl), 0
			
			; check for changes of octave
			check_octave_change:
			inc hl
			ld a, (hl)
			
			; increase octave
			check_space_and_left:
				bit 4, c
				jr nz, check_space_and_right
					
					dec a
				
			; decrease octave
			check_space_and_right:
				bit 7, c
				jr nz, input_done_octave_updated
				
					inc a
		
			input_done_octave_updated:
				
				; sanitize octave
				and 0x7
				ld (hl), a

		input_done:

			; get playback counter, decrement it and check if it has hit 0
			ld a, (playback_counter)
			dec a
			jr nz, playback_same_step
				
				; get loop length mask
				ld a, (loop_length)
				ld b, a
				
				; if it has, move the playhead on, update the psg registers and reset the counter
				ld hl, playhead
				ld a, (hl)
				inc a
				and b
				ld (hl), a
				
				; updating channel 1 pointer
				ld hl, channel1_ptr
				
				; branch, still using the zero flag from "and b" above
				jr z, channel_pointers_reset

					; move channel1_ptr forward
					inc (hl)
					inc (hl)

					jr channel_pointers_updated
					
				channel_pointers_reset:
				
					ld a, channel1 & 0xff
					ld (hl), a
				
				channel_pointers_updated:
				
				call update_psg
				
				; put playback speed in a for resetting the playback counter
				ld a, (playback_speed)
			
		playback_same_step:
		
			ld (playback_counter), a

		draw_sequences:
			
			ld hl, VRAM_SEQUENCER
			call SETWRT
			
			; start pointer at -16 from the end
			; first thing we'll be doing is adding 16 to it!
			ld hl, channels_end - SIZE_ROW
			ld b, 16
			
			draw_sequences_main_loop:
			
				; 8 beats per line
				ld c, 8
						
				; draw all channels
				draw_sequence_channel:
					
					; we need to keep both b and c as the loop counters
					push bc
					
					; this IO_VDP_DATA value is overwritten by the init code
					; with the port number stored in the bios
					poke_io_vdp_data_value:
					ld c, IO_VDP_DATA
					
					; check if this is the padding row
					ld a, b
					and 0x3
					jr nz, channel_is_not_padding
					
						; move pointer to next 8 beats
						ld a, l
						add a, SIZE_ROW + SIZE_ROW
						ld l, a
						
						; draw empty row of padding
						; using b here as we've preserved the value on the stack
						ld b, 32
						ld a, b
						
						channel_draw_padding:
							out (c), a
							djnz channel_draw_padding
						
						; restore bc before moving to next channel
						pop bc
						jr draw_sequence_channel_done
						
					channel_is_not_padding:

						; check if we're at the cursor
						ld a, (cursor_ptr)
						cp l
						jr nz, indicator_check_playback
						
							ld a, "+"
							jr draw_indicator
						
						; check if this is the note we're playing back
						indicator_check_playback:
						ld a, (channel1_ptr)
						cp l
						jr nz, not_playback_head
						
							ld a, ">"
							jr draw_indicator
						
						not_playback_head:
						
							ld a, " "
						
						; draw blank square/playback head/cursor
						draw_indicator:
						out (c), a
						
						; get address of note name in de
						ld a, (hl)
						add a, a
						add a, note_names & 0xff				
						ld e, a
						ld d, note_names >> 8
						
						; write note name
						ld a, (de)
						out (c), a
						inc de
						ld a, (de)
						out (c), a
						
						; write octave
						inc l
						ld a, (hl)
						add a, "0"
						out (c), a

						; move to next beat
						inc l
						
						; restore bc before moving to next beat
						pop bc
						
						dec c
						jr nz, draw_sequence_channel
						
				draw_sequence_channel_done:

				; move to next row of channel data
				ld a, l
				add a, SIZE_CHANNEL - SIZE_ROW
				ld l, a
				
				djnz draw_sequences_main_loop
			
			jp main_loop
		
		update_psg:
		
			ld a, 8
			ld (update_psg_volume_reg), a
			
			xor a
			ld (update_psg_pitch_reg), a
		
			; point h1 at the current beat of channel 1
			ld hl, (channel1_ptr)
			; looping through all 3 channels
			ld b, 3
			
			update_psg_loop:
				
				push bc
				push hl
				
				; get note number in a
				ld a, (hl)
				cp NOTE_REST
				jr nz, not_mute
				
					; increment pitch registers
					ld hl, update_psg_pitch_reg
					inc (hl)
					inc (hl)
				
					; mute channel
					ld l, update_psg_volume_reg & 0xff
					ld a, (hl)
					ld e, 0x0
					call WRTPSG
					
					jr update_psg_loop_continue
				
				not_mute:
				
					; get octave in b
					inc hl
					ld b, (hl)
					inc b
				
					; multiply note number by 2 and add lower byte of pitch table address
					; to get offset into pitch table to put in hl
					add a, a
					add a, note_pitches & 0xff
					ld l, a
					ld h, note_pitches >> 8
					
					; put pitch data into de
					ld e, (hl)
					inc hl
					ld d, (hl)
					
					; shift it to the correct octave
					update_psg_octave_shift:
						srl d
						rr e
						djnz update_psg_octave_shift
					
					; update pitch
					ld hl, update_psg_pitch_reg
					
					; write to lower pitch register
					ld a, (hl)
					; increment pitch register
					inc (hl)
					call WRTPSG
					
					; write to upper pitch register
					ld a, (hl)
					; increment pitch register
					inc (hl)
					; move high byte into e
					ld e, d
					call WRTPSG
										
					; unmute channel
					; only need to change lower byte of hl 
					ld l, update_psg_volume_reg & 0xff
					ld a, (hl)
					ld e, 0xf
					call WRTPSG
				
				update_psg_loop_continue:
				
				; increment volume register
				inc (hl)
				
				; restore address of channel data and loop counter
				pop hl
				pop bc
				
				; look at next channel
				ld a, l
				add a, SIZE_CHANNEL
				ld l, a
				
				djnz update_psg_loop
				
				ret
			
		; pre-allocate some variables!
		channel1_ptr: dw channel1
		;channel2_ptr: dw channel2
		;channel3_ptr: dw channel3
		
		cursor_ptr: dw channel1
		
		playhead: db 0
		playback_speed: db 16
		playback_counter: db 16
		
		loop_length: db 31
		
		note_names:
			db "C_C#D_D#E_F_F#G_G#A_A#B___"
			
		note_pitches:
			dw 0xd5d << 1, 0xc9c << 1, 0xbe7 << 1, 0xb3c << 1
			dw 0xa9b << 1, 0xa02 << 1, 0x973 << 1, 0x8eb << 1
			dw 0x86b << 1, 0x7f2 << 1, 0x780 << 1, 0x714 << 1
			
	ENDS
