#include "ImGuiConnector.hh"

#include "ImGuiCpp.hh"
#include "ImGuiManager.hh"
#include "ImGuiUtils.hh"

#include "Connector.hh"
#include "MSXMotherBoard.hh"
#include "Pluggable.hh"
#include "PluggingController.hh"

#include <imgui.h>

namespace openmsx {

void ImGuiConnector::showMenu(MSXMotherBoard* motherBoard)
{
	im::Menu("Connectors", motherBoard != nullptr, [&]{
		im::Table("table", 2, [&]{
			const auto& pluggingController = motherBoard->getPluggingController();
			const auto& pluggables = pluggingController.getPluggables();
			for (auto* connector : pluggingController.getConnectors()) {
				if (ImGui::TableNextColumn()) { // connector
					ImGui::TextUnformatted(connector->getDescription());
				}
				if (ImGui::TableNextColumn()) { // pluggable
					const auto& connectorName = connector->getName();
					auto connectorClass = connector->getClass();
					const auto& currentPluggable = connector->getPlugged();
					ImGui::SetNextItemWidth(150.0f);
					im::Combo(tmpStrCat("##", connectorName).c_str(), std::string(currentPluggable.getName()).c_str(), [&]{
						if (!currentPluggable.getName().empty()) {
							if (ImGui::Selectable("[unplug]")) {
								manager.executeDelayed(makeTclList("unplug", connectorName));
							}
						}
						for (auto& plug : pluggables) {
							if (plug->getClass() != connectorClass) continue;
							auto plugName = std::string(plug->getName());
							bool selected = plug.get() == &currentPluggable;
							int flags = !selected && plug->getConnector() ? ImGuiSelectableFlags_Disabled : 0; // plugged in another connector
							if (ImGui::Selectable(plugName.c_str(), selected, flags)) {
								manager.executeDelayed(makeTclList("plug", connectorName, plugName));
							}
							simpleToolTip(plug->getDescription());
						}
					});
					if (const auto& desc = currentPluggable.getDescription(); !desc.empty()) {
						simpleToolTip(desc);
					}
				}
			}
		});
	});
}

} // namespace openmsx