// $Id$

#ifndef __MSXMAPPERIO_HH__
#define __MSXMAPPERIO_HH__

#include <iostream>
#include <fstream>
#include <string>
#include "EmuTime.hh"
#include "MSXIODevice.hh"

class MSXMapperIODevice
{
	public:
		/**
		 * Convert a previously written byte (out &hff,1) to a byte 
		 * returned from an read operation (inp(&hff)).
		 */
		virtual byte convert(byte value) = 0;

		/**
		 * Every mapper registers itself with MSXMapperIO (not this class)
		 * MSXMapperIO than calls this method. This can be used to influence
		 * the result returned in convert().
		 */
		virtual void registerMapper(int blocks) = 0;
};

class MSXMapperIO : public MSXIODevice
{
	public:
		/**
		 * Destructor
		 */
		~MSXMapperIO();

		/**
		 * This is a singleton class. This method returns a reference
		 * to the single instance of this class.
		 */
		static MSXMapperIO *instance();
		
		byte readIO(byte port, const EmuTime &time);
		void writeIO(byte port, byte value, const EmuTime &time);
		
		void reset(const EmuTime &time);
		
		void registerMapper(int blocks);
		byte getPageNum(int page);
	
	private:
		/**
		 * Constructor.
		 */
		MSXMapperIO(MSXConfig::Device *config, const EmuTime &time);

		static MSXMapperIO *oneInstance;

		MSXMapperIODevice *device;
		byte pageNum[4];
};

#endif //__MSXMAPPERIO_HH__

