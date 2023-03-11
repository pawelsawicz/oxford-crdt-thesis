{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    name="dev-environment";    # that requires a name
    nativeBuildInputs = [ pkgs.idris2 ];
    shellHook = ''
      echo "Start developing..."
    '';   
}