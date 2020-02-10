.PHONY: clean run debug release clean

all: run

run: debug
	.build/debug/LispKitRepl -r Sources/LispKit/Resources -d LispKit

debug:
	swift build -Xswiftc "-D" -Xswiftc "SPM"

repl: release
	.build/release/LispKitRepl -r Sources/LispKit/Resources -d LispKit

release:
	swift build -c release -Xswiftc "-D" -Xswiftc "SPM"

test:
	swift test -Xswiftc "-D" -Xswiftc "SPM"

clean:
	rm -rf .build
