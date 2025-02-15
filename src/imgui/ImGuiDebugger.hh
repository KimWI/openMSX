#ifndef IMGUI_DEBUGGER_HH
#define IMGUI_DEBUGGER_HH

#include "ImGuiPart.hh"

#include "EmuTime.hh"

#include <cstdint>
#include <memory>
#include <vector>

namespace openmsx {

class CPURegs;
class DebuggableEditor;
class Debugger;
class ImGuiBitmapViewer;
class ImGuiCharacter;
class ImGuiDisassembly;
class ImGuiSpriteViewer;
class MSXCPUInterface;

class ImGuiDebugger final : public ImGuiPart
{
public:
	explicit ImGuiDebugger(ImGuiManager& manager);
	~ImGuiDebugger();

	static void loadIcons();

	[[nodiscard]] zstring_view iniName() const override { return "debugger"; }
	void save(ImGuiTextBuffer& buf) override;
	void loadStart() override;
	void loadLine(std::string_view name, zstring_view value) override;
	void showMenu(MSXMotherBoard* motherBoard) override;
	void paint(MSXMotherBoard* motherBoard) override;

	void signalBreak();
	void setGotoTarget(uint16_t target);
	void checkShortcuts(MSXCPUInterface& cpuInterface, MSXMotherBoard& motherBoard);

private:
	void drawControl(MSXCPUInterface& cpuInterface, MSXMotherBoard& motherBoard);
	void drawSlots(MSXCPUInterface& cpuInterface, Debugger& debugger);
	void drawStack(const CPURegs& regs, const MSXCPUInterface& cpuInterface, EmuTime::param time);
	void drawRegisters(CPURegs& regs);
	void drawFlags(CPURegs& regs);

	void actionBreakContinue(MSXCPUInterface& cpuInterface);
	void actionStepIn(MSXCPUInterface& cpuInterface);
	void actionStepOver();
	void actionStepOut();
	void actionStepBack();

private:
	std::vector<std::unique_ptr<ImGuiDisassembly>> disassemblyViewers;
	std::vector<std::unique_ptr<ImGuiBitmapViewer>> bitmapViewers;
	std::vector<std::unique_ptr<ImGuiCharacter>> tileViewers;
	std::vector<std::unique_ptr<ImGuiSpriteViewer>> spriteViewers;
	std::vector<std::unique_ptr<DebuggableEditor>> hexEditors; // sorted on 'getDebuggableName()'

	bool showControl = false;
	bool showSlots = false;
	bool showStack = false;
	bool showRegisters = false;
	bool showFlags = false;
	bool showXYFlags = false;
	int flagsLayout = 1;

	static constexpr auto persistentElements = std::tuple{
		PersistentElement{"showControl",     &ImGuiDebugger::showControl},
		PersistentElement{"showRegisters",   &ImGuiDebugger::showRegisters},
		PersistentElement{"showSlots",       &ImGuiDebugger::showSlots},
		PersistentElement{"showStack",       &ImGuiDebugger::showStack},
		PersistentElement{"showFlags",       &ImGuiDebugger::showFlags},
		PersistentElement{"showXYFlags",     &ImGuiDebugger::showXYFlags},
		PersistentElementMax{"flagsLayout",  &ImGuiDebugger::flagsLayout, 2}
		// manually handle "showDebuggable.xxx"
	};
};

} // namespace openmsx

#endif
