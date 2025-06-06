{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  env.LD_LIBRARY_PATH = with pkgs;
    lib.makeLibraryPath [
      llvmPackages_18.llvm
      libxml2
      libffi
    ];

  env.LLVM_SYS_181_PREFIX = "${pkgs.llvmPackages_18.llvm.dev}";

  languages.rust.enable = true;
}
