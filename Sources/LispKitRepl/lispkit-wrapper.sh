#!/bin/bash
#
# LispKit Scheme Wrapper Script
# 
# Automatically sets the resource path for the installed LispKit
# distribution. This wrapper allows users to run 'lispkit' without
# manually specifying the resource directory path via the -r flag.

# Determine the installation prefix by resolving the script location
SCRIPT_PATH="$(readlink "$0" || echo "$0")"
SCRIPT_DIR="$(cd "$(dirname "$SCRIPT_PATH")" && pwd)"
PREFIX="$(cd "$SCRIPT_DIR/.." && pwd)"

# Set default resource path
RESOURCE_PATH="$PREFIX/share/lispkit/Resources"

# Check if -r or --root is already in arguments
HAS_ROOT=0
for arg in "$@"; do
    if [[ "$arg" == "-r" ]] || [[ "$arg" == "--root" ]]; then
        HAS_ROOT=1
        break
    fi
done

# Determine path to actual LispKitRepl binary
LISPKIT_BIN="$PREFIX/libexec/lispkit/LispKitRepl"

# Execute LispKitRepl with resource path if not already specified
if [ $HAS_ROOT -eq 0 ]; then
    exec "$LISPKIT_BIN" -r "$RESOURCE_PATH" -d LispKit "$@"
else
    # User provided their own -r flag, don't override
    exec "$LISPKIT_BIN" "$@"
fi
