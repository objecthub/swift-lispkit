.PHONY: clean run debug release clean

all: run

run: debug
ifeq ($(program),)
	.build/debug/LispKitRepl -r Sources/LispKit/Resources -d LispKit
else
	.build/debug/LispKitRepl -r Sources/LispKit/Resources -d LispKit $(program)
endif

debug: replinfo
	swift build -Xswiftc "-D" -Xswiftc "SPM"

repl: release
ifeq ($(program),)
	.build/release/LispKitRepl -r Sources/LispKit/Resources -d LispKit
else
	.build/release/LispKitRepl -r Sources/LispKit/Resources -d LispKit $(program)
endif

release: replinfo
	swift build -c release -Xswiftc "-D" -Xswiftc "SPM"

replinfo:
	sed "s/CURRENT-DATE/$(shell date +%s)/" Sources/LispKitRepl/AppInfo.tmpl > Sources/LispKitRepl/AppInfo.swift

test:
	swift test -Xswiftc "-D" -Xswiftc "SPM"

update:
	carthage update --platform macOS

clean:
	rm -rf .build
