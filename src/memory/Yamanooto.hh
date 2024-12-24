#ifndef YAMANOOTO_HH
#define YAMANOOTO_HH

<<<<<<< HEAD
#include "MSXRom.hh"
#include "AmdFlash.hh"
#include "SCC.hh"
=======
#include "AY8910.hh"
#include "AmdFlash.hh"
#include "MSXRom.hh"
#include "SCC.hh"

>>>>>>> openMSX/master
#include <array>

namespace openmsx {

class Yamanooto final : public MSXRom
{
public:
	Yamanooto(const DeviceConfig& config, Rom&& rom);
<<<<<<< HEAD
=======
	~Yamanooto() override;
>>>>>>> openMSX/master

	void powerUp(EmuTime::param time) override;
	void reset(EmuTime::param time) override;
	[[nodiscard]] byte peekMem(word address, EmuTime::param time) const override;
	[[nodiscard]] byte readMem(word address, EmuTime::param time) override;
	[[nodiscard]] const byte* getReadCacheLine(word address) const override;
	void writeMem(word address, byte value, EmuTime::param time) override;
	[[nodiscard]] byte* getWriteCacheLine(word address) override;

<<<<<<< HEAD
=======
	byte peekIO(word port, EmuTime::param time) const override;
	byte readIO(word port, EmuTime::param time) override;
	void writeIO(word port, byte value, EmuTime::param time) override;

>>>>>>> openMSX/master
	template<typename Archive>
	void serialize(Archive& ar, unsigned version);

private:
<<<<<<< HEAD
	[[nodiscard]] bool isSCCAccess(word addr) const;
	[[nodiscard]] unsigned getFlashAddr(unsigned addr) const;

	[[nodiscard]] bool isKonamiSCCmode()         const { return (mapperReg & 0x20) == 0; }
	[[nodiscard]] bool isFlashRomWriteEnabled()  const { return (mapperReg & 0x10) != 0; }
	[[nodiscard]] bool isBank0Disabled()         const { return (mapperReg & 0x08) != 0; }
	[[nodiscard]] bool isMapperRegisterEnabled() const { return (mapperReg & 0x04) == 0; }
	[[nodiscard]] bool areBankRegsEnabled()      const { return (mapperReg & 0x02) == 0; }

private:
	AmdFlash flash;
	SCC scc;

	byte mapperReg;
	byte offsetReg;
	byte sccMode;
	std::array<byte, 4> bankRegs;
=======
	void writeConfigReg(byte value);
	[[nodiscard]] bool isSCCAccess(word addr) const;
	[[nodiscard]] unsigned getFlashAddr(unsigned addr) const;

private:
	AmdFlash flash;
	SCC scc;
	AY8910 psg;
	std::array<uint16_t, 4> bankRegs = {}; // need 10 bits per entry (not 8)
	byte enableReg = 0;
	byte offsetReg = 0;
	byte configReg = 0;
	byte sccMode = 0;
	byte psgLatch = 0;
	byte fpgaFsm = 0; // hack, just enough to read ID
>>>>>>> openMSX/master
};

} // namespace openmsx

#endif
