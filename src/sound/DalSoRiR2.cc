// TODO: for now copied from MSXMoonsound. Must be cleaned up.
// TODO: add flash functionality

// ATM this class does several things:
// - It connects the YMF278b chip to specific I/O ports in the MSX machine
// - It glues the YMF262 (FM-part) and YMF278 (Wave-part) classes together in a
//   full model of a YMF278b chip. IOW part of the logic of the YM278b is
//   modeled here instead of in a chip-specific class.
// TODO it would be nice to move the functionality of the 2nd point to a
//      different class, but until there's a 2nd user of this chip, this is
//      low priority.

#include "DalSoRiR2.hh"
#include "Clock.hh"
#include "serialize.hh"
#include "unreachable.hh"

namespace openmsx {

// The master clock, running at 33.8MHz.
using MasterClock = Clock<33868800>;

// Required delay between register select and register read/write.
static constexpr auto FM_REG_SELECT_DELAY = MasterClock::duration(56);
// Required delay after register write.
static constexpr auto FM_REG_WRITE_DELAY  = MasterClock::duration(56);
// Datasheet doesn't mention any delay for reads from the FM registers. In fact
// it says reads from FM registers are not possible while tests on a real
// YMF278 show they do work (value of the NEW2 bit doesn't matter).

// Required delay between register select and register read/write.
static constexpr auto WAVE_REG_SELECT_DELAY = MasterClock::duration(88);
// Required delay after register write.
static constexpr auto WAVE_REG_WRITE_DELAY  = MasterClock::duration(88);
// Datasheet doesn't mention any delay for register reads (except for reads
// from register 6, see below). I also couldn't measure any delay on a real
// YMF278.

// Required delay after memory read.
static constexpr auto MEM_READ_DELAY  = MasterClock::duration(38);
// Required delay after memory write (instead of register write delay).
static constexpr auto MEM_WRITE_DELAY = MasterClock::duration(28);

// Required delay after instrument load.
// We pick 10000 cycles, this is approximately 300us (the number given in the
// datasheet). The exact number of cycles is unknown. But I did some (very
// rough) tests on real HW, and this number is not too bad (slightly too high
// but within 2%-4% of real value, needs more detailed tests).
static constexpr auto LOAD_DELAY = MasterClock::duration(10000);

// bit(s) of the memory bank selection register REG_BANKn
static constexpr byte REG_BANK_SEGMENT_MASK = (1 << 5) - 1; // 5-bits

// bit(s) of the frame selection register REG_FRAMEn
static constexpr byte DISABLE = 1 << 7;
static constexpr byte REG_FRAME_SEGMENT_MASK = (1 << 3) - 1; // 3-bits

// bits of the configuration register REG_CFG
static constexpr byte ENA_S0 = 1 << 5;
static constexpr byte ENA_FW = 1 << 4;
static constexpr byte ENA_C4 = 1 << 1;
static constexpr byte ENA_C0 = 1 << 0;

DalSoRiR2::DalSoRiR2(const DeviceConfig& config)
	: MSXDevice(config)
	, ymf262(getName() + " FM", config, true)
	, ymf278(getName() + " wave",
	         4096, // 4MB RAM
	         config)
	, ymf278LoadTime(getCurrentTime())
	, ymf278BusyTime(getCurrentTime())
	, sram(config, getName() + " SRAM", "DalSoRi R2 RAM", 0x8000)
{
	powerUp(getCurrentTime());
}

void DalSoRiR2::powerUp(EmuTime::param time)
{
	ymf278.clearRam();
	reset(time);
}

void DalSoRiR2::reset(EmuTime::param time)
{
	ymf262.reset(time);
	ymf278.reset(time);

	opl4latch = 0; // TODO check
	opl3latch = 0; // TODO check

	ymf278BusyTime = time;
	ymf278LoadTime = time;

	for (auto reg : xrange(4)) {
		REG_BANK[reg] = reg;
	}
	for (auto reg : xrange(2)) {
		REG_FRAME[reg] = reg;
	}
	REG_CFG = ENA_C4; // MoonSound compatible by default
}

byte DalSoRiR2::readIO(word port, EmuTime::param time)
{
	if ((port & 0xFF) < 0xC0) {
		// WAVE part  0x7E-0x7F
		switch (port & 0x01) {
		case 0: // read latch, not supported
			return 255;
		case 1: // read wave register
			// Verified on real YMF278:
			// Even if NEW2=0 reads happen normally. Also reads
			// from sample memory (and thus the internal memory
			// pointer gets increased).
			if ((3 <= opl4latch) && (opl4latch <= 6)) {
				// This time is so small that on a MSX you can
				// never see BUSY=1. So I also couldn't test
				// whether this timing applies to registers 3-6
				// (like for write) or only to register 6. I
				// also couldn't test how the other registers
				// behave.
				// TODO Should we comment out this code? It
				// doesn't have any measurable effect on MSX.
				ymf278BusyTime = time + MEM_READ_DELAY;
			}
			return ymf278.readReg(opl4latch);
		default:
			UNREACHABLE;
		}
	} else {
		if ((REG_CFG & ENA_C4) != 0) {
			// FM part  0xC4-0xC7
			switch (port & 0x03) {
			case 0: // read status
			case 2:
				return ymf262.readStatus() | readYMF278Status(time);
			case 1:
			case 3: // read fm register
				return ymf262.readReg(opl3latch);
			default:
				UNREACHABLE;
			}
		} else {
			return 0xFF;
		}
	}
}

byte DalSoRiR2::peekIO(word port, EmuTime::param time) const
{
	if ((port & 0xFF) < 0xC0) {
		// WAVE part  0x7E-0x7F
		switch (port & 0x01) {
		case 0: // read latch, not supported
			return 255;
		case 1: // read wave register
			return ymf278.peekReg(opl4latch);
		default:
			UNREACHABLE;
		}
	} else {
		if ((REG_CFG & ENA_C4) != 0) {
			// FM part  0xC4-0xC7
			switch (port & 0x03) {
			case 0: // read status
			case 2:
				return ymf262.peekStatus() | readYMF278Status(time);
			case 1:
			case 3: // read fm register
				return ymf262.peekReg(opl3latch);
			default:
				UNREACHABLE;
			}
		} else {
			return 0xFF;
		}
	}
}

void DalSoRiR2::writeIO(word port, byte value, EmuTime::param time)
{
	if ((port & 0xFF) < 0xC0) {
		// WAVE part  0x7E-0x7F
		if (getNew2()) {
			switch (port & 0x01) {
			case 0: // select register
				ymf278BusyTime = time + WAVE_REG_SELECT_DELAY;
				opl4latch = value;
				break;
			case 1:
				if ((0x08 <= opl4latch) && (opl4latch <= 0x1F)) {
					ymf278LoadTime = time + LOAD_DELAY;
				}
				if ((3 <= opl4latch) && (opl4latch <= 6)) {
					// Note: this time is so small that on
					// MSX you never see BUSY=1 for these
					// registers. Confirmed on real HW that
					// also registers 3-5 are faster.
					ymf278BusyTime = time + MEM_WRITE_DELAY;
				} else {
					// For the other registers it is
					// possible to see BUSY=1, but only
					// very briefly and only on R800.
					ymf278BusyTime = time + WAVE_REG_WRITE_DELAY;
				}
				if (opl4latch == 0xf8) {
					ymf262.setMixLevel(value, time);
				} else if (opl4latch == 0xf9) {
					ymf278.setMixLevel(value, time);
				}
				ymf278.writeReg(opl4latch, value, time);
				break;
			default:
				UNREACHABLE;
			}
		} else {
			// Verified on real YMF278:
			// Writes are ignored when NEW2=0 (both register select
			// and register write).
		}
	} else {
		if ((REG_CFG & ENA_C4) != 0) {
			// FM part  0xC4-0xC7
			switch (port & 0x03) {
			case 0: // select register bank 0
				opl3latch = value;
				ymf278BusyTime = time + FM_REG_SELECT_DELAY;
				break;
			case 2: // select register bank 1
				opl3latch = value | 0x100;
				ymf278BusyTime = time + FM_REG_SELECT_DELAY;
				break;
			case 1:
			case 3: // write fm register
				ymf278BusyTime = time + FM_REG_WRITE_DELAY;
				ymf262.writeReg(opl3latch, value, time);
				break;
			default:
				UNREACHABLE;
			}
		}
	}
}

byte DalSoRiR2::readMem(word addr, EmuTime::param time)
{
	if ((0x3000 <= addr) && (addr < 0x4000)) {
		// frame 0
		return sram[addr - 0x3000];
	} else if ((0x7000 <= addr) && (addr < 0x8000)) {
		if ((REG_CFG & DISABLE) == 0) {
			// frame 1
			return sram[addr - 0x6000];
		} else {
			// TODO: flash read bank 1
		}
	} else if ((0xB000 <= addr) && (addr < 0xC000)) {
		if ((REG_CFG & DISABLE) == 0) {
			// frame 2
			return sram[addr - 0x9000];
		} else {
			// TODO: flash read bank 2
		}
	}
	
	return MSXDevice::readMem(addr, time);
}

void DalSoRiR2::writeMem(word addr, byte value, EmuTime::param /*time*/)
{
	if ((0x3000 <= addr) && (addr < 0x4000)) {
		// frame 0
		sram[addr - 0x3000] = value;
	} else if ((0x7000 <= addr) && (addr < 0x8000)) {
		if ((REG_CFG & DISABLE) == 0) {
			// frame 1
			sram[addr - 0x6000] = value;
		}
	} else if ((0xB000 <= addr) && (addr < 0xC000)) {
		if ((REG_CFG & DISABLE) == 0) {
			// frame 2
			sram[addr - 0x9000] = value;
		}
	} else if ((0x6000 <= addr) && (addr < 0x6800)) {
		if (addr >= 0x6700) {
			REG_CFG = value;
			ymf278.setRomEnabled((REG_CFG & ENA_S0) == 0);
		} else if (addr >= 0x6500) {
			REG_FRAME[(addr >> 9) & 1] = value;
		} else if (addr < 0x6400) {
			REG_BANK [(addr >> 8) & 3] = value;
		}
	}
}

bool DalSoRiR2::getNew2() const
{
	return (ymf262.peekReg(0x105) & 0x02) != 0;
}

byte DalSoRiR2::readYMF278Status(EmuTime::param time) const
{
	byte result = 0;
	if (time < ymf278BusyTime) result |= 0x01;
	if (time < ymf278LoadTime) result |= 0x02;
	return result;
}

// version 1: initial version
template<typename Archive>
void DalSoRiR2::serialize(Archive& ar, unsigned /*version*/)
{
	ar.template serializeBase<MSXDevice>(*this);
	ar.serialize("ymf262",    ymf262,
	             "ymf278",    ymf278,
	             "opl3latch", opl3latch,
	             "opl4latch", opl4latch,
	             "loadTime",  ymf278LoadTime,
	             "sram",      sram,
	             "REG_BANK",  REG_BANK,
	             "REG_FRAME", REG_FRAME,
	             "REG_CFG",   REG_CFG);
}
INSTANTIATE_SERIALIZE_METHODS(DalSoRiR2);
REGISTER_MSXDEVICE(DalSoRiR2, "DalSoRiR2");

} // namespace openmsx
