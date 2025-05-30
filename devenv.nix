{
  pkgs,
  lib,
  config,
  inputs,
  ...
}: {
  env.GREET = "devenv";

  languages.rust.enable = true;
}
