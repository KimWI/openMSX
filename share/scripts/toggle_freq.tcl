set_help_text toggle_freq "Switch between PAL/NTSC."

proc toggle_freq {} {
	debug write "VDP regs" 9    [expr {[debug read "VDP regs" 9]    ^ 2}]
	debug write "memory" 0xFFE8 [expr {[debug read "memory" 0xFFE8] ^ 2}]

	if {[debug read "VDP regs" 9]==0} {osd::display_message "Frequency set to 60hz" info} else {osd::display_message "Frequency set to 50hz" info}
}
