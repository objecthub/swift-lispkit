.PHONY: clean run debug release clean

all: debug

run:
	.build/debug/LispKitRepl -d LispKit

debug:
	swift build -Xswiftc "-D" -Xswiftc "SPM"

release:
	swift build -c release -Xswiftc "-D" -Xswiftc "SPM"

clean:
	rm -rf .build
