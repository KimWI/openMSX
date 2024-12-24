#include "Yamanooto.hh"
<<<<<<< HEAD
#include "narrow.hh"
#include "ranges.hh"
#include "serialize.hh"
#include <array>

/******************************************************************************
Introduction

Yamanooto is a flash cartridge for MSX computers that includes an FPGA implementation of Konami's SCC chip, which provides wavetable sound together with "megarom" mapping for large rom games.
Yamanooto thus uses by default the K5 mapper, expanded to 8 bit to allow direct 2 Mbyte rom size. The register access is done in a compatible way, using the upper bytes of page 1 (4000h-7FFFh):

7FFFh (ENAR): to enable register interface
7FFEh (OFFR): offset register (more on this later)
7FFDh (CFGR); configuration bits
Note that upon reset, no register is readable (you get the rom contents). You need to write the ENAR register [bit 1] to 1 to enable the registers access.

Slots selection

In order to access the Yamanooto (and any cartridge) you need to use the MSX slot mechanism to switch page 1 to the logical slot (a number from 0 to 3). MSX cartridges fit physical slots where you plug them in. Some computers have one physical slot but most have more than one.
Usually first physical slot is logical slot 1 and second (when present) is logical slot 2, so probably a Yamanooto is accessed in logical slots 1 or 2.
Provided you know which logical slot the cartridge is, you use the ppi (Parallel Port Interface) of the MSX to switch page 1 to that slot. The PPI has a register at port A8h to switch pages to logical slots:

bit 0-1: slot for page 0 (0000h-3FFFh)
bit 2-3: slot for page 1 (4000h-7FFFh)
bit 4-5: slot for page 2 (8000h-BFFFh)
bit 6-7: slot for page 3 (C000h-FFFFh)
  (bits in port A8h, AKA port 0 of the PPI)

So, i.e. to switch page 1 to slot 1 you need to keep bits 0-1 and 4-5 unchanged and replace bits 2-3 with value "01" (slot 1).

; switch page 1, desired slot value in B
IN A,(0A8h)
LD (orig_value),A  ; save value to restore it later
AND 0F3h  ; clear bits 2-3
LD C,A
LD A,B  ; get slot value
RLCA
RLCA
OR C
DI  ; disable ints before switch!
OUT (0A8h),A
<now desired slot is selected in page 1>
 
When you're done with the cartridge, you may need to restore the previous usage of page 1 before you reenable the interrupts:

; restore slots
LD A,(orig_value)
OUT (0A8h),A
EI
 
 

Setting the K5 mapper

The K5 mapper uses segments of 8 Kbyte length. Thus the 32 Kbyte cartridge area (pages 1-2, 4000h-BFFFh), are split in 4 banks. Each bank is "half a page"
To switch each bank to a given segment, you write the segment number in a "switch address" for each of the 4 banks:

bank 0: 5000h-57FFh (usually 5000h)
bank 1: 7000h-77FFh (usually 7000h)
bank 2: 9000h-97FFh (usually 9000h)
bank 3: B000h-B7FFh (usually B000h)

Note that the register area of the Yamanooto doesn't conflict with the switch addresses because the registers are in the upper area and the switch address area is in a lower part of the bank.
Once you write in the switch area, the bank is switched immediately, thus the following accesses are mapped to the new segment instead of the previously selected. If the executing code is in the affected bank, you may lose control because you're suddenly executing code in other segment.

The offset register (OFFR)

When K5 was first implemented by Konami in the original SCC chip, only 5 bits were implemented (hence the name) in each of the 4 banks, thus any of the 4 banks could be mapped to any of 32 segments.
As a segment is 8 kbyte long, this allowed for 32 * 8 kbyte = 256 kbyte rom.
Later, more bits were used, up to the full 8 bits possible in a single register. Yamanooto and other modern K5 implementations use all 8 bits which allow for 256 segments thus 256 * 8 kbyte = 2 Mbyte rom size.
In order to both overcome this limitation and provide a way of emulating a different rom for each of several games written in the the same rom space, an additional mechanism was added to allow for larger "flash cartridges".
The offset register changes which area that the "basic mapper" uses of the larger rom space. By adding an offset, we change which real address of the complete flash (which is bigger that 2 Mbyte) is accessed for "mapper segment 0" and all subsequent segments up to the mapper limit (2 Mbyte)

In the Yamanooto, the offset unit is 32kbyte, thus the mapper value is added four times the offset value:

segment address = mapper value + (4 * offset)
 
NOTE: the value is not applied until you actually write any of the corresponding mapper

As the normal mapper functionality is preserved, the offset register may be used to run a game with resides in a part (not only the beginning) of the large flash rom of the Yamanooto. The procedure would be:

Switch page 1 to the correct slot (if needed)
Enable the register access and set the OFFR to the game offset (in 32 kbyte units)
Reset the mapper to initial values (usually 0-1-2-3). This applies the offset to all banks so you'll probably will want to do this with code in ram
Do a fake reboot
A possible code for steps 1-2 would look like that this:

LD B,1  ; for slot 1
CALL set_page1  ; if you're running with page 1 in ram
LD HL,7FFFh
LD (HL),1
DEC HL
LD (HL),<offset value>
DEC HL
LD (HL),0  ; clear ROMDIS (see note)
 
Following you should reset the mapper and perform a fake reset (CALL 0) or simulate the cartridge boot in any manner.
Note that if you're running in some code outside of the Yamanooto flash (i.e. a dos command) and you've skipped the flash boot with the DEL key, you need to reenable the rom boot clearing the bit ROMDIS in CFGR (7FFDh). See the manual
To reset the mapper after changing the offset, you need to switch all four banks:

Ensure pages 1-2 are mapped to correct slot
Write 0 to switch address of bank 0 (5000h <= 0)
Write 1 to switch address of bank 1 (7000h <= 1)
Write 2 to switch address of bank 2 (9000h <= 2)
Write 3 to switch address of bank 3 (B000h <= 3)

Further considerations

The brave would probably have already noticed that the offset register can be used to create large games that use the full 8 Mbyte size of the flash. You can even have each of the banks in a different offset. Any bank can be ANYWHERE because each bank is applied a new offset ONLY when switched!
The occurrence of a hardware reset is safe and will return control to the offset 0 (the normal start of the game or menu) because the mapper reset is done with offset 0 (i.e. actual 0-1-2-3)
In case of a menu selector, code can choose to restore the previously selected game or not by reading the OFFR register (which survives reset)

Have a nice time programming and make great games or compilations with Yamanooto! 
******************************************************************************/

namespace openmsx {

Yamanooto::Yamanooto(
	const DeviceConfig& config, Rom&& rom_)
	: MSXRom(config, std::move(rom_))
	, flash(rom, AmdFlashChip::S29GL064S70TFI040, {}, config)
	, scc("KUC SCC", config, getCurrentTime(), SCC::Mode::Compatible)
{
	powerUp(getCurrentTime());
}

=======

#include "DummyAY8910Periphery.hh"
#include "MSXCPUInterface.hh"

#include "ranges.hh"
#include "unreachable.hh"

#include <cassert>

// TODO:
// * HOME/DEL boot keys.  How are these handled?
// * SCC stereo mode.

namespace openmsx {

static constexpr word ENAR = 0x7FFF;
static constexpr byte REGEN  = 0x01;
static constexpr byte WREN   = 0x10;
static constexpr word OFFR = 0x7FFE;
static constexpr word CFGR = 0x7FFD;
static constexpr byte MDIS   = 0x01;
static constexpr byte ECHO   = 0x02;
static constexpr byte ROMDIS = 0x04;
static constexpr byte K4     = 0x08;
static constexpr byte SUBOFF = 0x30;
// undocumented stuff
static constexpr byte FPGA_EN   = 0x40; // write: 1 -> enable communication with FPGA commands ???
static constexpr byte FPGA_WAIT = 0x80; // read: ready signal ??? (1 = ready)
static constexpr word FPGA_REG  = 0x7FFC; // bi-direction 8-bit communication channel

Yamanooto::Yamanooto(const DeviceConfig& config, Rom&& rom_)
	: MSXRom(config, std::move(rom_))
	, flash(rom, AmdFlashChip::S29GL064N90TFI04, {}, config)
	, scc(getName() + " SCC", config, getCurrentTime(), SCC::Mode::Compatible)
	, psg(getName() + " PSG", DummyAY8910Periphery::instance(), config, getCurrentTime())
{
	// Tests shown that the PSG has higher volume than the scc. See
	//   https://www.msx.org/forum/msx-talk/openmsx/openmsx-cartridge-type?page=2#comment-470014
	//   https://msx.pics/images/2024/12/23/yamanooto_volume_comparison.png
	psg.setSoftwareVolume(3.0f, getCurrentTime());

	getCPUInterface().register_IO_Out_range(0x10, 2, this);
	powerUp(getCurrentTime());
}

Yamanooto::~Yamanooto()
{
	writeConfigReg(0); // unregister A0-A2 ports
	getCPUInterface().unregister_IO_Out_range(0x10, 2, this);
}

>>>>>>> openMSX/master
void Yamanooto::powerUp(EmuTime::param time)
{
	scc.powerUp(time);
	reset(time);
}

void Yamanooto::reset(EmuTime::param time)
{
<<<<<<< HEAD
	mapperReg = 0;
	offsetReg = 0;
	sccMode = 0;
	ranges::iota(bankRegs, byte(0));

	scc.reset(time);

	invalidateDeviceRWCache(); // flush all to be sure
=======
	// TODO is offsetReg changed by reset?
	// TODO are all bits in configReg set to zero?   (also bit 0,1)
	enableReg = 0;
	offsetReg = 0;
	writeConfigReg(0);
	fpgaFsm = 0;

	ranges::iota(bankRegs, uint16_t(0));
	sccMode = 0;
	scc.reset(time);

	psgLatch = 0;
	psg.reset(time);

	flash.reset();

	invalidateDeviceRCache(); // flush all to be sure
}

void Yamanooto::writeConfigReg(byte value)
{
	if ((value ^ configReg) & ECHO) {
		if (value & ECHO) {
			getCPUInterface().register_IO_Out_range(0xA0, 2, this);
		} else {
			getCPUInterface().unregister_IO_Out_range(0xA0, 2, this);
		}
	}
	configReg = value;
}

bool Yamanooto::isSCCAccess(word address) const
{
	if (configReg & K4) return false; // Konami4 doesn't have SCC

	if (sccMode & 0x20) {
		// SCC+   range: 0xB800..0xBFFF,  excluding 0xBFFE-0xBFFF
		return  (bankRegs[3] & 0x80)          && (0xB800 <= address) && (address < 0xBFFE);
	} else {
		// SCC    range: 0x9800..0x9FFF
		return ((bankRegs[2] & 0x3F) == 0x3F) && (0x9800 <= address) && (address < 0xA000);
	}
>>>>>>> openMSX/master
}

unsigned Yamanooto::getFlashAddr(unsigned addr) const
{
	unsigned page8kB = (addr >> 13) - 2;
<<<<<<< HEAD
	if (page8kB >= 4) return unsigned(-1); // outside [0x4000, 0xBFFF]

	byte bank = bankRegs[page8kB] + offsetReg; // wrap at 8 bit
	return ((mapperReg & 0xC0) << (21 - 6)) | (bank << 13) | (addr & 0x1FFF);
}

bool Yamanooto::isSCCAccess(word addr) const
{
	if (sccMode & 0x10) return false;

	if (addr & 0x0100) {
		// Address bit 8 must be zero, this is different from a real
		// SCC/SCC+. According to Manuel Pazos this is a leftover from
		// an earlier version that had 2 SCCs: the SCC on the left or
		// right channel reacts when address bit 8 is respectively 0/1.
		return false;
	}

	if (sccMode & 0x20) {
		// SCC+   range: 0xB800..0xBFFF,  excluding 0xBFFE-0xBFFF
		return  (bankRegs[3] & 0x80)          && (0xB800 <= addr) && (addr < 0xBFFE);
	} else {
		// SCC    range: 0x9800..0x9FFF,  excluding 0x9FFE-0x9FFF
		return ((bankRegs[2] & 0x3F) == 0x3F) && (0x9800 <= addr) && (addr < 0x9FFE);
	}
}

byte Yamanooto::readMem(word addr, EmuTime::param time)
{
	if (isSCCAccess(addr)) {
		return scc.readMem(narrow_cast<uint8_t>(addr & 0xFF), time);
	}

	unsigned flashAddr = getFlashAddr(addr);
	return (flashAddr != unsigned(-1))
		? flash.read(flashAddr)
		: 0xFF; // unmapped read
}

byte Yamanooto::peekMem(word addr, EmuTime::param time) const
{
	if (isSCCAccess(addr)) {
		return scc.peekMem(narrow_cast<uint8_t>(addr & 0xFF), time);
	}

	unsigned flashAddr = getFlashAddr(addr);
	return (flashAddr != unsigned(-1))
		? flash.peek(flashAddr)
		: 0xFF; // unmapped read
}

const byte* Yamanooto::getReadCacheLine(word addr) const
{
	if (isSCCAccess(addr)) return nullptr;

	unsigned flashAddr = getFlashAddr(addr);
	return (flashAddr != unsigned(-1))
		? flash.getReadCacheLine(flashAddr)
		: unmappedRead.data();
}

void Yamanooto::writeMem(word addr, byte value, EmuTime::param time)
{
	unsigned page8kB = (addr >> 13) - 2;
	if (page8kB >= 4) return; // outside [0x4000, 0xBFFF]

	// There are several overlapping functional regions in the address
	// space. A single write can trigger behaviour in multiple regions. In
	// other words there's no priority amongst the regions where a higher
	// priority region blocks the write from the lower priority regions.
	// This only goes for places where the flash is 'seen', so not for the
	// SCC registers

	if (isSCCAccess(addr)) {
		scc.writeMem(narrow_cast<uint8_t>(addr & 0xFF), value, time);
		return; // write to SCC blocks write to other functions
	}

	// address is calculated before writes to other regions take effect
	unsigned flashAddr = getFlashAddr(addr);

	// Mapper and offset registers
	if (isMapperRegisterEnabled()) {
		if (addr == 0x7FFF) {
			mapperReg = value;
		} else if (addr == 0x7FFE) {
			offsetReg = value;
		}
		invalidateDeviceRCache(); // flush all to be sure
	}


	if (areBankRegsEnabled()) {
		// Bank-switching
		if (isKonamiSCCmode()) {
			// Konami-SCC
			if ((addr & 0x1800) == 0x1000) {
				// [0x5000,0x57FF] [0x7000,0x77FF]
				// [0x9000,0x97FF] [0xB000,0xB7FF]
				// Masking of the mapper bits is done on write
				bankRegs[page8kB] = value;
				invalidateDeviceRCache(0x4000 + 0x2000 * page8kB, 0x2000);
			}
		} else {
			// Konami
			if (isBank0Disabled() && (addr < 0x6000)) {
				// Switching 0x4000-0x5FFF disabled (only Konami mode).
			} else {
				// [0x5000,0x57FF] asymmetric!!!
				// [0x6000,0x7FFF] [0x8000,0x9FFF] [0xA000,0xBFFF]
				if (!((addr < 0x5000) || ((0x5800 <= addr) && (addr < 0x6000)))) {
					// Masking of the mapper bits is done on write
					bankRegs[page8kB] = value;
					invalidateDeviceRCache(0x4000 + 0x2000 * page8kB, 0x2000);
				}
			}
		}

		// SCC mode register
		if ((addr & 0xFFFE) == 0xBFFE) {
			sccMode = value;
			scc.setMode((value & 0x20) ? SCC::Mode::Plus
			                           : SCC::Mode::Compatible);
			invalidateDeviceRCache(0x9800, 0x800);
			invalidateDeviceRCache(0xB800, 0x800);
		}
	}

	if ((flashAddr != unsigned(-1)) && isFlashRomWriteEnabled()) {
		flash.write(flashAddr, value);
	}
}

byte* Yamanooto::getWriteCacheLine(word addr)
{
	return ((0x4000 <= addr) && (addr < 0xC000))
	       ? nullptr        // [0x4000,0xBFFF] isn't cacheable
	       : unmappedWrite.data();
=======
	assert(page8kB < 4); // must be inside [0x4000, 0xBFFF]
	auto bank = bankRegs[page8kB] & 0x3ff; // max 8MB
	return (bank << 13) | (addr & 0x1FFF);
}

[[nodiscard]] static word mirror(word address)
{
	if (address < 0x4000 || 0xC000 <= address) {
		// mirror 0x4000 <-> 0xc000   /   0x8000 <-> 0x0000
		address ^= 0x8000; // real hw probably just ignores upper address bit
	}
	assert((0x4000 <= address) && (address < 0xC000));
	return address;
}

static constexpr std::array<byte, 4 + 1> FPGA_ID = {
	0xFF, // idle
	0x1F, 0x23, 0x00, 0x00, // TODO check last 2
};
byte Yamanooto::peekMem(word address, EmuTime::param time) const
{
	address = mirror(address);

	// 0x7ffc-0x7fff
	if (FPGA_REG <= address && address <= ENAR && (enableReg & REGEN)) {
		switch (address) {
		case FPGA_REG:
			if (!(configReg & FPGA_EN)) return 0xFF; // TODO check this
			assert(fpgaFsm < 5);
			return FPGA_ID[fpgaFsm];
		case CFGR: return configReg | FPGA_WAIT; // not-busy
		case OFFR: return offsetReg;
		case ENAR: return enableReg;
		default: UNREACHABLE;
		}
	}

	// 0x9800-0x9FFF or 0xB800-0xBFFD
	if (isSCCAccess(address)) {
		return scc.peekMem(narrow_cast<uint8_t>(address & 0xFF), time);
	}
	return ((configReg & ROMDIS) == 0) ? flash.peek(getFlashAddr(address))
	                                   : 0xFF; // access to flash ROM disabled
}

byte Yamanooto::readMem(word address, EmuTime::param time)
{
	// 0x7ffc-0x7fff  (NOT mirrored)
	if (FPGA_REG <= address && address <= ENAR && (enableReg & REGEN)) {
		return peekMem(address, time);
	}
	address = mirror(address);

	// 0x9800-0x9FFF or 0xB800-0xBFFD
	if (isSCCAccess(address)) {
		return scc.readMem(narrow_cast<uint8_t>(address & 0xFF), time);
	}
	return ((configReg & ROMDIS) == 0) ? flash.read(getFlashAddr(address))
	                                   : 0xFF; // access to flash ROM disabled
}

const byte* Yamanooto::getReadCacheLine(word address) const
{
	address = mirror(address);
	if (((address & CacheLine::HIGH) == (ENAR & CacheLine::HIGH)) && (enableReg & REGEN)) {
		return nullptr; // Yamanooto registers, non-cacheable
	}
	if (isSCCAccess(address)) return nullptr;
	return ((configReg & ROMDIS) == 0) ? flash.getReadCacheLine(getFlashAddr(address))
	                                   : unmappedRead.data(); // access to flash ROM disabled
}

void Yamanooto::writeMem(word address, byte value, EmuTime::param time)
{
	// 0x7ffc-0x7fff  (NOT mirrored)
	if (FPGA_REG <= address && address <= ENAR) {
		assert(false);
		if (address == ENAR) {
			enableReg = value;
		} else if (enableReg & REGEN) {
			switch (address) {
			case FPGA_REG:
				if (!(configReg & FPGA_EN)) break;
				switch (fpgaFsm) {
				case 0:
					if (value == 0x9f) { // read ID command ??
						++fpgaFsm;
					}
					break;
				case 1: case 2: case 3:
					++fpgaFsm;
					break;
				case 4:
					fpgaFsm = 0;
					break;
				default:
					UNREACHABLE;
				}
				break;
			case CFGR:
				writeConfigReg(value);
				break;
			case OFFR:
				offsetReg = value; // does NOT immediately switch bankRegs
				break;
			default:
				UNREACHABLE;
			}
		}
		invalidateDeviceRCache();
	}
	address = mirror(address);
	unsigned page8kB = (address >> 13) - 2;

	if (enableReg & WREN) {
		// write to flash rom
		if (!(configReg & ROMDIS)) { // disabled?
			flash.write(getFlashAddr(address), value);
			invalidateDeviceRCache(); // needed ?
		}
	} else {
		auto invalidateCache = [&](unsigned start, unsigned size) {
			invalidateDeviceRCache(start, size);
			invalidateDeviceRCache(start ^ 0x8000, size); // mirror
		};
		auto offset = (offsetReg << 2) | ((configReg & SUBOFF) >> 4);
		if (configReg & K4) {
			// Konami-4
			if (((configReg & MDIS) == 0) && (0x6000 <= address)) {
				// bank 0 is NOT switchable, but it keeps the
				// value it had before activating K4 mode
				bankRegs[page8kB] = (value + offset) & 0x3FF;
				invalidateCache(0x4000 + 0x2000 * page8kB, 0x2000);
			}
		} else {
			// 0x9800-0x9FFF or 0xB800-0xBFFD
			if (isSCCAccess(address)) {
				scc.writeMem(narrow_cast<uint8_t>(address & 0xFF), value, time);
			}

			// Konami-SCC
			if (((address & 0x1800) == 0x1000) && ((configReg & MDIS) == 0)) {
				// [0x5000,0x57FF] [0x7000,0x77FF]
				// [0x9000,0x97FF] [0xB000,0xB7FF]
				bankRegs[page8kB] = (value + offset) & 0x3FF;
				invalidateCache(0x4000 + 0x2000 * page8kB, 0x2000);
			}

			// SCC mode register
			if ((address & 0xFFFE) == 0xBFFE) {
				sccMode = value;
				scc.setMode((value & 0x20) ? SCC::Mode::Plus
				                           : SCC::Mode::Compatible);
				invalidateCache(0x9800, 0x800);
				invalidateCache(0xB800, 0x800);
			}
		}
	}
}

byte* Yamanooto::getWriteCacheLine(word /*address*/)
{
	return nullptr;
}

byte Yamanooto::peekIO(word /*port*/, EmuTime::param /*time*/) const
{
	return 0xff;
}

byte Yamanooto::readIO(word /*port*/, EmuTime::param /*time*/)
{
	// PSG is not readable
	return 0xff; // should never be called
}

void Yamanooto::writeIO(word port, byte value, EmuTime::param time)
{
	if (port & 1) { // 0x11 or 0xA1
		psg.writeRegister(psgLatch, value, time);
	} else { // 0x10 or 0xA0
		psgLatch = value & 0x0F;
	}
>>>>>>> openMSX/master
}

template<typename Archive>
void Yamanooto::serialize(Archive& ar, unsigned /*version*/)
{
<<<<<<< HEAD
	// skip MSXRom base class
	ar.template serializeBase<MSXDevice>(*this);

	ar.serialize("flash",     flash,
	             "scc",       scc,
	             "mapperReg", mapperReg,
	             "offsetReg", offsetReg,
	             "sccMode",   sccMode,
	             "bankRegs",  bankRegs);
=======
	ar.serialize(
		"flash", flash,
		"scc", scc,
		"psg", psg,
		"bankRegs", bankRegs,
		"enableReg", enableReg,
		"offsetReg", offsetReg,
		"configReg", configReg,
		"sccMode", sccMode,
		"sccMode", sccMode,
		"psgLatch", psgLatch,
		"fpgaFsm", fpgaFsm
	);
>>>>>>> openMSX/master
}
INSTANTIATE_SERIALIZE_METHODS(Yamanooto);
REGISTER_MSXDEVICE(Yamanooto, "Yamanooto");

} // namespace openmsx
