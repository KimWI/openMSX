include build/node-start.mk

SRC_HDR:= \
	EventDistributor \
	HotKey \
	AfterCommand \
	Keys \
	CliComm GlobalCliComm MSXCliComm \
	StdioMessages TclCallbackMessages \
	CliServer CliConnection Socket \
	Event InputEvents \
	InputEventGenerator InputEventFactory \
	MessageCommand \

HDR_ONLY:= \
	CliListener \
	EventListener \
	FinishFrameEvent

include build/node-end.mk
