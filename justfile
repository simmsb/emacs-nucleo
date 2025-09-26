macos:
  cargo zigbuild --release --target universal2-apple-darwin
  cp target/universal2-apple-darwin/release/libemacs_nucleo.dylib nucleo-module.dylib
