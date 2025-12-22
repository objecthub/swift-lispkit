.PHONY: all clean run debug repl replinfo release test install uninstall dist

# Installation paths
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin
LIBEXECDIR = $(PREFIX)/libexec/lispkit
SHAREDIR = $(PREFIX)/share/lispkit
VERSION = 2.6.0

# Test install to a temporary location
#   make install PREFIX=/tmp/lispkit-test
#
# Run the installed version
#   /tmp/lispkit-test/bin/lispkit
#
# Clean up
#   make uninstall PREFIX=/tmp/lispkit-test

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
	sed "s/CURRENT-DATE/$(shell date +%s)/g;s/CURRENT-VERSION/$(VERSION)/g" Sources/LispKitRepl/AppInfo.tmpl > Sources/LispKitRepl/AppInfo.swift

test:
	swift test

clean:
	rm -rf .build

# Install target for Homebrew and manual installation
install: release
	@echo "Installing LispKit to $(PREFIX)..."
	install -d $(LIBEXECDIR)
	install -d $(BINDIR)
	install -d $(SHAREDIR)
	install -m 755 .build/release/LispKitRepl $(LIBEXECDIR)/LispKitRepl
	install -m 755 Sources/LispKitRepl/lispkit-wrapper.sh $(BINDIR)/lispkit
	cp -R Sources/LispKit/Resources $(SHAREDIR)/
	@echo ""
	@echo "Installation complete!"
	@echo "Resources installed to: $(SHAREDIR)/Resources"
	@echo "Binary installed to: $(BINDIR)/lispkit"
	@echo "Run with: lispkit"

# Uninstall target
uninstall:
	rm -f $(BINDIR)/lispkit
	rm -rf $(LIBEXECDIR)
	rm -rf $(SHAREDIR)
	@echo "LispKit uninstalled from $(PREFIX)"

# Create distributable tarball
dist: release
	@echo "Creating distribution tarball for version $(VERSION)..."
	mkdir -p Distributions/lispkit-$(VERSION)/bin
	cp .build/release/LispKitRepl Distributions/lispkit-$(VERSION)/bin
	sed "s/share\/lispkit\/Resources/Resources/g;s/libexec\/lispkit/bin/g" Sources/LispKitRepl/lispkit-wrapper.sh > Distributions/lispkit-$(VERSION)/bin/lispkit
	chmod 755 Distributions/lispkit-$(VERSION)/bin/lispkit
	cp -R Sources/LispKit/Resources Distributions/lispkit-$(VERSION)/
	sed "s/CURRENT-VERSION/$(VERSION)/" Sources/LispKitRepl/README > Distributions/lispkit-$(VERSION)/README
	cp LICENSE Distributions/lispkit-$(VERSION)/ 2>/dev/null || true
	cd Distributions && tar czf lispkit-$(VERSION).tar.gz lispkit-$(VERSION)
	rm -rf Distributions/lispkit-$(VERSION)
	shasum -a 256 Distributions/lispkit-$(VERSION).tar.gz
	@echo "Tarball created: Distributions/lispkit-$(VERSION).tar.gz"
