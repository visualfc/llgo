#!/bin/bash

# llgo_cache_dir prints the platform cache root used by os.UserCacheDir plus
# the llgo suffix. Keep CI cache restore paths and local build helpers on the
# same path by sourcing this file instead of duplicating the platform mapping.
llgo_cache_dir() {
    case "$(uname -s)" in
        Darwin)
            printf '%s\n' "${HOME}/Library/Caches/llgo"
            ;;
        MINGW*|MSYS*|CYGWIN*)
            printf '%s\n' "${LOCALAPPDATA}/llgo"
            ;;
        *)
            printf '%s\n' "${XDG_CACHE_HOME:-$HOME/.cache}/llgo"
            ;;
    esac
}
