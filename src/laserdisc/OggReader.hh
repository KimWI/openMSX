#ifndef OGGREADER_HH
#define OGGREADER_HH

#include "openmsx.hh"
#include "noncopyable.hh"
#include <ogg/ogg.h>
#include <vorbis/codec.h>
#include <theora/theoradec.h>
#include <memory>
#include <list>
#include <deque>
#include <map>
#include <set>

namespace openmsx {

class CliComm;
class RawFrame;
class File;
class Filename;

struct AudioFragment
{
	static const size_t UNKNOWN_POS = size_t(-1);
	static const unsigned MAX_SAMPLES = 2048;
	size_t position;
	unsigned length;
	float pcm[2][MAX_SAMPLES];
};

struct Frame
{
	Frame(const th_ycbcr_buffer& yuv);
	~Frame();

	th_ycbcr_buffer buffer;
	size_t no;
	int length;
};

class OggReader : private noncopyable
{
public:
	OggReader(const Filename& filename, CliComm& cli);
	~OggReader();

	bool seek(size_t frame, size_t sample);
	unsigned getSampleRate() const { return vi.rate; }
	void getFrameNo(RawFrame& frame, size_t frameno);
	const AudioFragment* getAudio(size_t sample);
	size_t getFrames() const;
	int getFrameRate() const { return frameRate; }

	// metadata
	bool stopFrame(size_t frame) const;
	size_t chapter(int chapterNo) const;

private:
	void cleanup();
	void readTheora(ogg_packet* packet);
	void theoraHeaderPage(ogg_page* page, th_info& ti, th_comment& tc,
	                      th_setup_info*& tsi);
	void readMetadata(th_comment& tc);
	void readVorbis(ogg_packet* packet);
	void vorbisHeaderPage(ogg_page* page);
	bool nextPage(ogg_page* page);
	bool nextPacket();
	void recycleAudio(std::unique_ptr<AudioFragment> audio);
	void vorbisFoundPosition();
	size_t frameNo(ogg_packet* packet);

	size_t findOffset(size_t frame, size_t sample);
	size_t bisection(size_t frame, size_t sample,
	                 size_t maxOffset, size_t maxSamples, size_t maxFrames);

	CliComm& cli;
	const std::unique_ptr<File> file;

	enum State {
		PLAYING,
		FIND_LAST,
		FIND_FIRST,
		FIND_KEYFRAME
	} state;

	// ogg state
	ogg_sync_state sync;
	ogg_stream_state vorbisStream, theoraStream;
	int audioSerial;
	int videoSerial;
	int skeletonSerial;
	size_t fileOffset;
	size_t fileSize;

	// video
	th_dec_ctx* theora;
	int frameRate;
	size_t keyFrame;
	size_t currentFrame;
	int granuleShift;
	size_t totalFrames;

	typedef std::deque<std::unique_ptr<Frame>> Frames;
	Frames frameList;
	Frames recycleFrameList;

	// audio
	int audioHeaders;
	vorbis_info vi;
	vorbis_comment vc;
	vorbis_dsp_state vd;
	vorbis_block vb;
	size_t currentSample;
	size_t vorbisPos;

	typedef std::list<std::unique_ptr<AudioFragment>> AudioFragments;
	AudioFragments audioList;
	AudioFragments recycleAudioList;

	// Metadata
	std::set<size_t> stopFrames;
	std::map<int, size_t> chapters;
};

} // namespace openmsx

#endif
