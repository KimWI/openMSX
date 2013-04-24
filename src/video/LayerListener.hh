#ifndef LAYERLISTENER_HH
#define LAYERLISTENER_HH

namespace openmsx {

class Layer;

class LayerListener
{
public:
	virtual void updateZ(Layer& layer) = 0;
protected:
	virtual ~LayerListener() {}
};

} // namespace openmsx

#endif
