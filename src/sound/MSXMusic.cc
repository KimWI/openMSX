// $Id$

#include "MSXMusic.hh"

MSXMusic::MSXMusic(Device *config, const EmuTime &time)
	: MSXDevice(config, time), MSXYM2413(config, time)
{
	enable = 1;
}

MSXMusic::~MSXMusic()
{
}

void MSXMusic::reset(const EmuTime &time)
{
	MSXYM2413::reset(time);
}

byte MSXMusic::readMem(word address, const EmuTime &time)
{
	switch (address) {
		//case 0x7ff4:	// TODO are these two defined for MSX-MUSIC
		//case 0x7ff5:
		case 0x7ff6:
			return enable;
		//case 0x7ff7:
		default:
			return rom.read(address & 0x3fff);
	}
}

void MSXMusic::writeMem(word address, byte value, const EmuTime &time)
{
	switch (address) {
		//case 0x7ff4:	// TODO are these two defined for MSX-MUSIC
		//case 0x7ff5;
		case 0x7ff6:
			enable = value & 0x11;
			break;
		//case 0x7ff7:
	}
}
