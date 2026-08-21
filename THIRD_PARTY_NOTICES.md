# Third-party notices

## TinyGo

LLGo contains source derived from the [TinyGo project](https://github.com/tinygo-org/tinygo):

- the conservative bare-metal collector in `runtime/internal/runtime/tinygogc/gc_tinygo.go`;
- firmware image support in `internal/firmware/esp.go`, `nrfutil.go`, `objcopy.go`, and `uf2.go`;
- the marked flashing-support portions of `internal/flash/flash.go`;
- target configurations and support files under `targets`.

Reference snapshots contemporaneous with the initial LLGo imports are:

- [runtime GC](https://github.com/tinygo-org/tinygo/tree/79ab77facd8b4d7ea39257f85d37f094f52770d2/src/runtime);
- [firmware builders](https://github.com/tinygo-org/tinygo/tree/3869f76887feef6c444308e7e1531b7cac1bbd10/builder);
- [initial target configuration import](https://github.com/tinygo-org/tinygo/tree/8c5886060f022a36768b5c29327759846021a868/targets).

The TinyGo-derived portions remain subject to the BSD 3-Clause License in
[`LICENSES/TinyGo-BSD-3-Clause.txt`](LICENSES/TinyGo-BSD-3-Clause.txt).
Independently written LLGo code remains subject to the repository's Apache
License 2.0; applicable file and directory notices identify the license for
modifications to derived files.

Distributions containing the TinyGo-derived source or compiled code, including
bare-metal firmware that links the collector, must retain or reproduce the
TinyGo copyright notice, license conditions, and disclaimer as required by the
BSD 3-Clause License.
