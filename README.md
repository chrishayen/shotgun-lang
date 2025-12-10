# Shotgun

A systems language for app developers that combines Odin's simplicity, Zig's comptime, Python's clean aesthetics, and Rust's safety - without the complexity.

> **Note:** Shotgun is in early development. Expect breaking changes.

**Documentation:** https://shotgun.dev

## Installation

Download the latest binary from [GitHub Releases](https://github.com/chrishayen/shotgun-lang/releases):

```bash
curl -LO https://github.com/chrishayen/shotgun-lang/releases/latest/download/shotgun-linux-x86_64
chmod +x shotgun-linux-x86_64
mv shotgun-linux-x86_64 ~/.local/bin/shotgun
```

> **Note:** Only Linux x86_64 binaries are available. macOS/Windows not supported.

## Building from Source

### Prerequisites

- CMake 3.16+
- C++17 compiler (GCC or Clang)
- LLVM 17+ (development libraries)
- tomlplusplus (header-only TOML library)

On Arch Linux:
```bash
pacman -S cmake llvm tomlplusplus
```

On Ubuntu/Debian:
```bash
apt install cmake llvm-dev libtomlplusplus-dev
```

### Build

```bash
make build
```

### Test

```bash
make test
```

### Install

```bash
make install  # installs to ~/.local/bin/shotgun
```

## Usage

```bash
shotgun build file.bs           # compile to executable
shotgun build file.bs -o out    # compile with custom output name
shotgun check file.bs           # type check only
shotgun emit-ir file.bs         # output LLVM IR
shotgun test                    # run test suite
```

## Contact

- Email: chris@shotgun.dev

## License

MIT
