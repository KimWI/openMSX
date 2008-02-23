// $Id$

#ifndef DUMMYVIDEOSYSTEM_HH
#define DUMMYVIDEOSYSTEM_HH

#include "VideoSystem.hh"

namespace openmsx {

class DummyVideoSystem: public VideoSystem
{
public:
	// VideoSystem interface:
	virtual V9990Rasterizer* createV9990Rasterizer(V9990& vdp);
	virtual void flush();
	virtual OutputSurface* getOutputSurface();
};

} // namespace openmsx

#endif
