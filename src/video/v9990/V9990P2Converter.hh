#ifndef V9990P2CONVERTER_HH
#define V9990P2CONVERTER_HH

namespace openmsx {

class V9990;
class V9990VRAM;

template<typename Pixel>
class V9990P2Converter
{
public:
	V9990P2Converter(V9990& vdp, const Pixel* palette64);

	void convertLine(
		Pixel* linePtr, unsigned displayX, unsigned displayWidth,
		unsigned displayY, unsigned displayYA, bool drawSprites);

private:
	V9990& vdp;
	V9990VRAM& vram;
	const Pixel* const palette64;
};

} // namespace openmsx

#endif
