.PHONY: clean run debug release clean

all: run

run: debug
ifeq ($(program),)
	.build/debug/LispKitRepl -x -r Sources/LispKit/Resources -d LispKit
else
	.build/debug/LispKitRepl -x -r Sources/LispKit/Resources -d LispKit $(program)
endif

debug: replinfo
	swift build

repl: release
ifeq ($(program),)
	.build/release/LispKitRepl -x -r Sources/LispKit/Resources -d LispKit
else
	.build/release/LispKitRepl -x -r Sources/LispKit/Resources -d LispKit $(program)
endif

release: replinfo
	swift build -c release

replinfo:
	sed "s/CURRENT-DATE/$(shell date +%s)/" Sources/LispKitRepl/AppInfo.tmpl > Sources/LispKitRepl/AppInfo.swift

test:
	swift test

clean:
	rm -rf .build
