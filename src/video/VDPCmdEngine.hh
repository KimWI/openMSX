// $Id$

#ifndef __VDPCMDENGINE_HH__
#define __VDPCMDENGINE_HH__

#include "openmsx.hh"
#include "VDP.hh"
#include "DisplayMode.hh"
#include "BooleanSetting.hh"
#include "SettingListener.hh"


namespace openmsx {

class VDPVRAM;


/** VDP command engine by Alex Wulms.
  * Implements command execution unit of V9938/58.
  */
class VDPCmdEngine : private SettingListener
{
public:
	/** Constructor.
	  */
	VDPCmdEngine(VDP* vdp);

	/** Destructor
	  */
	virtual ~VDPCmdEngine();

	/** Reinitialise Renderer state.
	  * @param time The moment in time the reset occurs.
	  */
	void reset(const EmuTime& time);

	/** Synchronises the command engine with the VDP.
	  * Ideally this would be a private method, but the current
	  * design doesn't allow that.
	  * @param time The moment in emulated time to sync to.
	  */
	inline void sync(const EmuTime& time) {
		if (currentCommand) currentCommand->execute(time);
	}

	/** Gets the command engine status (part of S#2).
	  * Bit 7 (TR) is set when the command engine is ready for
	  * a pixel transfer.
	  * Bit 4 (BD) is set when the boundary colour is detected.
	  * Bit 0 (CE) is set when a command is in progress.
	  */
	inline byte getStatus(const EmuTime& time) {
		if (time >= statusChangeTime) {
			sync(time);
		}
		return status;
	}

	/** Use this method to transfer pixel(s) from VDP to CPU.
	  * This method implements V9938 S#7.
	  * @param time The moment in emulated time this read occurs.
	  * @return Colour value of the pixel.
	  */
	inline byte readColour(const EmuTime& time) {
		sync(time);
		return COL;
	}
	inline void resetColour() {
		//status &= 0x7F; // dont reset TR
		// Note: Real VDP does reset TR, but for such a short time
		//       that the MSX won't notice it.
		transfer = true;
	}

	/** Gets the X coordinate of a border detected by SRCH.
	  * @param time The moment in emulated time this get occurs.
	  */
	inline int getBorderX(const EmuTime& time) {
		sync(time);
		return borderX;
	}

	/** Writes to a command register.
	  * @param index The register [0..14] to write to.
	  * @param value The new value for the specified register.
	  * @param time The moment in emulated time this write occurs.
	  */
	void setCmdReg(byte index, byte value, const EmuTime& time);

	/** Read the content of a command register. This method is ment to
	  * be used by the debugger, there is no strict guarantee that the
	  * returned value is the correct value at _exactly_ this moment in
	  * time (IOW this method does not sync the complete CmdEngine)
	  * @param index The register [0..14] to read from.
	  */
	byte peekCmdReg(byte index);

	/** Informs the command engine of a VDP display mode change.
	  * @param mode The new display mode.
	  * @param time The moment in emulated time this change occurs.
	  */
	void updateDisplayMode(DisplayMode mode, const EmuTime& time);

	/** Interface for logical operations.
	  */
	class LogOp {
	public:
		/** Write a pixel using a logical operation.
		  * @param time Time at which the write occurs.
		  * @param vram Pointer to VRAM class.
		  * @param addr Address of pixel in VRAM.
		  * @param colour Colour of pixel to be written.
		  */
		virtual void pset(
			EmuTime& time, VDPVRAM* vram, int addr, byte colour, byte mask
			) = 0;
	};

private:

	/** Represents V9938 Graphic 4 mode (SCREEN5).
	  */
	class Graphic4Mode {
		friend class VDPCmd;
	public:
		static const byte COLOUR_MASK = 0x0F;
		static const byte PIXELS_PER_BYTE = 2;
		static const byte PIXELS_PER_BYTE_SHIFT = 1;
		static const word PIXELS_PER_LINE = 256;
		static inline int addressOf(int x, int y);
		static inline byte point(VDPVRAM* vram, int x, int y);
		static inline void pset(EmuTime& time, VDPVRAM* vram,
			int x, int y, byte colour, LogOp* op);
	};

	/** Represents V9938 Graphic 5 mode (SCREEN6).
	  */
	class Graphic5Mode {
		friend class VDPCmd;
	public:
		static const byte COLOUR_MASK = 0x03;
		static const byte PIXELS_PER_BYTE = 4;
		static const byte PIXELS_PER_BYTE_SHIFT = 2;
		static const word PIXELS_PER_LINE = 512;
		static inline int addressOf(int x, int y);
		static inline byte point(VDPVRAM* vram, int x, int y);
		static inline void pset(EmuTime& time, VDPVRAM* vram,
			int x, int y, byte colour, LogOp* op);
	};

	/** Represents V9938 Graphic 6 mode (SCREEN7).
	  */
	class Graphic6Mode {
		friend class VDPCmd;
	public:
		static const byte COLOUR_MASK = 0x0F;
		static const byte PIXELS_PER_BYTE = 2;
		static const byte PIXELS_PER_BYTE_SHIFT = 1;
		static const word PIXELS_PER_LINE = 512;
		static inline int addressOf(int x, int y);
		static inline byte point(VDPVRAM* vram, int x, int y);
		static inline void pset(EmuTime& time, VDPVRAM* vram,
			int x, int y, byte colour, LogOp* op);
	};

	/** Represents V9938 Graphic 7 mode (SCREEN8).
	  */
	class Graphic7Mode {
		friend class VDPCmd;
	public:
		static const byte COLOUR_MASK = 0xFF;
		static const byte PIXELS_PER_BYTE = 1;
		static const byte PIXELS_PER_BYTE_SHIFT = 0;
		static const word PIXELS_PER_LINE = 256;
		static inline int addressOf(int x, int y);
		static inline byte point(VDPVRAM* vram, int x, int y);
		static inline void pset(EmuTime& time, VDPVRAM* vram,
			int x, int y, byte colour, LogOp* op);
	};

	/** This is an abstract base class the VDP commands
	  */
	class VDPCmd {
	public:
		VDPCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual ~VDPCmd();

		/** Prepare execution of cmd
		  */
		virtual void start(const EmuTime& time) = 0;

		/** Perform a given V9938 graphical operation.
		  */
		virtual void execute(const EmuTime& time) = 0;

	protected:
		VDPCmdEngine* engine;
		VDPVRAM* vram;

		/** Time at which the next operation cycle starts.
		  * A cycle consists of reading source VRAM (if applicable),
		  * reading destination VRAM (if applicable),
		  * writing destination VRAM and updating coordinates.
		  * For perfect timing each phase within a cycle should be timed
		  * explicitly, but for now we use an average execution time per
		  * cycle.
		  */
		EmuTimeFreq<VDP::TICKS_PER_SECOND> currentTime;

		word ASX, ADX, ANX;
	};
	friend class VDPCmd;

	template <template <class Mode> class Command>
	void createEngines(int cmd);

	/** Abort
	  */
	class AbortCmd : public VDPCmd {
	public:
		AbortCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class AbortCmd;

	/** Point
	  */
	template <class Mode>
	class PointCmd : public VDPCmd {
	public:
		PointCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class PointCmd<Graphic4Mode>;
	friend class PointCmd<Graphic5Mode>;
	friend class PointCmd<Graphic6Mode>;
	friend class PointCmd<Graphic7Mode>;

	/** Pset
	  */
	template <class Mode>
	class PsetCmd : public VDPCmd {
	public:
		PsetCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class PsetCmd<Graphic4Mode>;
	friend class PsetCmd<Graphic5Mode>;
	friend class PsetCmd<Graphic6Mode>;
	friend class PsetCmd<Graphic7Mode>;

	/** Search a dot.
	  */
	template <class Mode>
	class SrchCmd : public VDPCmd {
	public:
		SrchCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class SrchCmd<Graphic4Mode>;
	friend class SrchCmd<Graphic5Mode>;
	friend class SrchCmd<Graphic6Mode>;
	friend class SrchCmd<Graphic7Mode>;

	/** Draw a line.
	  */
	template <class Mode>
	class LineCmd : public VDPCmd {
	public:
		LineCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class LineCmd<Graphic4Mode>;
	friend class LineCmd<Graphic5Mode>;
	friend class LineCmd<Graphic6Mode>;
	friend class LineCmd<Graphic7Mode>;
	
	/** Abstract base class for block commands.
	  */
	class BlockCmd : public VDPCmd {
	public:
		BlockCmd(VDPCmdEngine* engine, VDPVRAM* vram, const int* timing);
	protected:
		void calcFinishTime(word NX, word NY);

		const int* timing;
	};
	friend class BlockCmd;

	/** Logical move VDP -> VRAM.
	  */
	template <class Mode>
	class LmmvCmd : public BlockCmd {
	public:
		LmmvCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class LmmvCmd<Graphic4Mode>;
	friend class LmmvCmd<Graphic5Mode>;
	friend class LmmvCmd<Graphic6Mode>;
	friend class LmmvCmd<Graphic7Mode>;

	/** Logical move VRAM -> VRAM.
	  */
	template <class Mode>
	class LmmmCmd : public BlockCmd {
	public:
		LmmmCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class LmmmCmd<Graphic4Mode>;
	friend class LmmmCmd<Graphic5Mode>;
	friend class LmmmCmd<Graphic6Mode>;
	friend class LmmmCmd<Graphic7Mode>;

	/** Logical move VRAM -> CPU.
	  */
	template <class Mode>
	class LmcmCmd : public BlockCmd {
	public:
		LmcmCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class LmcmCmd<Graphic4Mode>;
	friend class LmcmCmd<Graphic5Mode>;
	friend class LmcmCmd<Graphic6Mode>;
	friend class LmcmCmd<Graphic7Mode>;

	/** Logical move CPU -> VRAM.
	  */
	template <class Mode>
	class LmmcCmd : public BlockCmd {
	public:
		LmmcCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class LmmcCmd<Graphic4Mode>;
	friend class LmmcCmd<Graphic5Mode>;
	friend class LmmcCmd<Graphic6Mode>;
	friend class LmmcCmd<Graphic7Mode>;

	/** High-speed move VDP -> VRAM.
	  */
	template <class Mode>
	class HmmvCmd : public BlockCmd {
	public:
		HmmvCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class HmmvCmd<Graphic4Mode>;
	friend class HmmvCmd<Graphic5Mode>;
	friend class HmmvCmd<Graphic6Mode>;
	friend class HmmvCmd<Graphic7Mode>;

	/** High-speed move VRAM -> VRAM.
	  */
	template <class Mode>
	class HmmmCmd : public BlockCmd {
	public:
		HmmmCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class HmmmCmd<Graphic4Mode>;
	friend class HmmmCmd<Graphic5Mode>;
	friend class HmmmCmd<Graphic6Mode>;
	friend class HmmmCmd<Graphic7Mode>;

	/** High-speed move VRAM -> VRAM (Y direction only).
	  */
	template <class Mode>
	class YmmmCmd : public BlockCmd {
	public:
		YmmmCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class YmmmCmd<Graphic4Mode>;
	friend class YmmmCmd<Graphic5Mode>;
	friend class YmmmCmd<Graphic6Mode>;
	friend class YmmmCmd<Graphic7Mode>;

	/** High-speed move CPU -> VRAM.
	  */
	template <class Mode>
	class HmmcCmd : public BlockCmd {
	public:
		HmmcCmd(VDPCmdEngine* engine, VDPVRAM* vram);
		virtual void start(const EmuTime& time);
		virtual void execute(const EmuTime& time);
	};
	friend class HmmcCmd<Graphic4Mode>;
	friend class HmmcCmd<Graphic5Mode>;
	friend class HmmcCmd<Graphic6Mode>;
	friend class HmmcCmd<Graphic7Mode>;


	virtual void update(const SettingLeafNode* setting) throw();
	
	void executeCommand(const EmuTime& time);
	
	/** Finshed executing graphical operation.
	  */
	void commandDone(const EmuTime& time);

	/** Get the current command timing, depends on vdp settings (sprites, display).
	  */
	inline int getTiming() {
		return brokenTiming ? 0 : vdp->getAccessTiming(); 
	}

	/** Report the VDP command specified in the registers.
	  */
	void reportVdpCommand();


	/** VDP command registers.
	  */
	word SX, SY, DX, DY, NX, NY;
	byte COL, ARG, CMD, LOG;

	/** The command engine status (part of S#2).
	  * Bit 7 (TR) is set when the command engine is ready for
	  * a pixel transfer.
	  * Bit 4 (BD) is set when the boundary colour is detected.
	  * Bit 0 (CE) is set when a command is in progress.
	  */
	byte status;

	/** Used in LMCM LMMC HMMC cmds, true when CPU has read or written
	  * next byte.
	  */
	bool transfer;

	/** The X coordinate of a border detected by SRCH.
	  */
	int borderX;

	/** The VDP this command engine is part of.
	  */
	VDP* vdp;
	VDPVRAM* vram;

	/** Current screen mode.
	  * 0 -> SCREEN5, 1 -> SCREEN6, 2 -> SCREEN7, 3 -> SCREEN8,
	  * -1 -> other.
	  */
	int scrMode;

	/** Real command timing or instantaneous (broken) timing
	  */
	bool brokenTiming;

	/** Lower bound for the time when the status register will change, IOW
	  * the status register will not change before this time.
	  * Can also be EmuTime::zero -> status can change any moment
	  * or EmuTime::infinity -> this command doesn't change the status
	  */
	EmuTime statusChangeTime;

	VDPCmd* commands[16][4];
	VDPCmd* currentCommand;
	LogOp* currentOperation;

	/** Only call reportVdpCommand() when this setting is turned on
	  */
	BooleanSetting cmdTraceSetting;
};

} // namespace openmsx

#endif // __VDPCMDENGINE_HH__
