macos:
  cargo zigbuild --release --target universal2-apple-darwin
  cp target/universal2-apple-darwin/release/libemacs_nucleo.dylib nucleo-module.dylib

# zigbuild is borked: https://github.com/rust-cross/cargo-zigbuild/discussions/508
native:
  cargo build --release --target aarch64-apple-darwin
  cp target/aarch64-apple-darwin/release/libemacs_nucleo.dylib nucleo-module.dylib
