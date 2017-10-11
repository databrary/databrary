let
#bring into scope output variables from build.nix
inherit (import ./build.nix {}) pkgs nixpkgs nodePackages;
in
# override buildInput attribute during mkDerivation process
pkgs.databrary-dev.env.overrideAttrs (attrs: {
  buildInputs = attrs.buildInputs or [] ++
    [nodePackages.shell.nodeDependencies
     nixpkgs.nodejs-8_x
     nixpkgs.jdk #required by Solr
     nixpkgs.haskellPackages.cabal-install
    ];
})
