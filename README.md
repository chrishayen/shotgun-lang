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

- OCaml 5.4.0
- opam
- GCC

### Install OCaml (if needed)

```bash
opam switch create 5.4.0
eval $(opam env --switch=5.4.0)
```

### Build

```bash
cd compiler
opam install . --deps-only -y
cd ..
make build
```

### Test

```bash
make test
./tests/run_tests.sh
```

### Install

```bash
make install  # installs to ~/.local/bin/shotgun
```

## Usage

```bash
shotgun build file.bs           # compile to binary
shotgun build file.bs -o out    # compile with custom output name
shotgun check file.bs           # type check only
shotgun emit-c file.bs          # output generated C code
```

## Contact

- X: [@shotgundotdev](https://x.com/shotgundotdev)
- Email: chris@shotgun.dev

## License

MIT
