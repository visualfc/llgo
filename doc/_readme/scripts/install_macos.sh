# shellcheck disable=all
brew update
brew install llvm@19 lld@19 bdw-gc openssl cjson libffi libuv pkg-config
brew install python@3.12 # optional
export PKG_CONFIG_PATH="$(brew --prefix libffi)/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
brew link --overwrite llvm@19 lld@19
# curl https://raw.githubusercontent.com/xgo-dev/llgo/refs/heads/main/install.sh | bash
./install.sh
