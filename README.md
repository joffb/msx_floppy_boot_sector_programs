# msx_floppy_boot_sector_programs
Boot Sector Programs for MSX Floppy Disks

MSX programs boot off a floppy by copying the first 256 bytes of the boot sector to 0xc000 and then jumping to 0xc01e.
These programs both then copy across the rest of the boot sector to 0xc000 (using the PHYDIO call in the BIOS), so I can use the full 512 bytes of space.
Both programs use the SCREEN1 32x24 text mode to make some drawing easier and to allow for sprites.

The disk header is hardcoded for a 720kb 3.5" floppy and these have been tested on hardware with an MSX2+.

They were written using the Glass assembler (https://www.grauw.nl/projects/glass/).

## Breakout
This is a simple breakout clone.
The ball's angle changes when it hits the paddle and can either bounce off at 45 degrees or 22.5 degrees.
The ball speeds up after 8, 16, 32 and 64 blocks have been broken.
You've got 4 lives and it's game over when you lose them all.
You move the paddle with the left and right cursor keys and after winning or losing, you can press space to restart the game.



## Sequencer
This is a 32 step sequencer with 3 channels of playback, using the AY-3-8910. Each step can either be a note or a rest.
Sequences can be 2, 4, 8, 16 or 32 steps long. The amount of time each step takes can be changed from between 1 frame and 255 frames.
The editing cursor can be moved around with the cursor keys.
Notes are changed by holding Space and pressing Up and Down.
Octaves are changed by holding Space and pressing Left and Right.
The sequencer can be sped up or slowed down with Ins or Home.
The number of steps in the sequence can be changed by holding Space and pressing Ins or Home.

