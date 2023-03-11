{ pkgs ? import <nixpkgs> {} }:
  pkgs.buildIdris
  pkgs.mkShell {
    name="dev-environment";    # that requires a name
    nativeBuildInputs = [ pkgs.idris2 ];
    shellHook = ''
      echo "Start developing..."
    '';   
}