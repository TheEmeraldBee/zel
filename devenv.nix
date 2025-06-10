{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  packages = with pkgs; [
    llvmPackages_18.llvm.dev
    libxml2.dev
    libffi.dev
  ];
  env.LD_LIBRARY_PATH = with pkgs;
    lib.makeLibraryPath [
      llvmPackages_18.llvm.dev
      libxml2.dev
      libffi.dev
    ];

  env.LLVM_SYS_181_PREFIX = "${pkgs.llvmPackages_18.llvm.dev}";

  languages.rust.enable = true;
}
