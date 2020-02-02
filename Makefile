all: debug

debug:
	swift build -Xswiftc "-D" -Xswiftc "SPM"

release:
	swift build -c release -Xswiftc "-D" -Xswiftc "SPM"
