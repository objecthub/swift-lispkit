.PHONY: clean run debug release clean

all: run

run: debug
	.build/debug/LispKitRepl -r Sources/LispKit/Resources -d LispKit

debug:
	swift build -Xswiftc "-D" -Xswiftc "SPM"

release:
	swift build -c release -Xswiftc "-D" -Xswiftc "SPM"

clean:
	rm -rf .build
