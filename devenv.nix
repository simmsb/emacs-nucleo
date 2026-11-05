{ pkgs, lib, config, inputs, ... }:

{
  packages = with pkgs; [
    just

    zig
    cargo-zigbuild
  ];

  languages.rust = {
    enable = true;
    channel = "stable";
    targets = [ "x86_64-apple-darwin" ];
  };
}
