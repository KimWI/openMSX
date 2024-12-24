#ifndef DALSORIR2_HH
#define DALSORIR2_HH

#include "MSXDevice.hh"
#include "YMF262.hh"
#include "YMF278.hh"
#include "serialize_meta.hh"

// TODO: for now copied from MSXMoonsound. Must be cleaned up.

namespace openmsx {

class DalSoRiR2 final : public MSXDevice
{
public:
	explicit DalSoRiR2(const DeviceConfig& config);

	void powerUp(EmuTime::param time) override;
	void reset(EmuTime::param time) override;
	[[nodiscard]] byte readIO(word port, EmuTime::param time) override;
	[[nodiscard]] byte peekIO(word port, EmuTime::param time) const override;
	void writeIO(word port, byte value, EmuTime::param time) override;
	byte readMem(word addr, EmuTime::param time) override;
	void writeMem(word addr, byte value, EmuTime::param time) override;

	template<typename Archive>
	void serialize(Archive& ar, unsigned version);

private:
	[[nodiscard]] bool getNew2() const;
	[[nodiscard]] byte readYMF278Status(EmuTime::param time) const;

private:
	YMF262 ymf262;
	YMF278 ymf278;

	/** Time at which instrument loading is finished. */
	EmuTime ymf278LoadTime;
	/** Time until which the YMF278 is busy. */
	EmuTime ymf278BusyTime;

	int opl3latch;
	byte opl4latch;

	Ram sram;
	
	std::array<byte, 4> REG_BANK;
	std::array<byte, 2> REG_FRAME;
	byte REG_CFG;
};
SERIALIZE_CLASS_VERSION(DalSoRiR2, 1);

} // namespace openmsx

#endif
