{
  description = "Crisp, a Curried Lisp";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    hix = {
      url = "https://flakehub.com/f/tek/hix/~0.7.tar.gz";
    };
  };

  outputs = inputs: inputs.hix ({ config, ... }: let
    pkgs = inputs.nixpkgs.legacyPackages.${config.system};
  in {
    envs.dev = {
      ghc.compiler = "ghc946";
      hls.enable = true;
      buildInputs = with pkgs; [
        nixVersions.latest
        mktemp
      ];
    };

    cabal = {
      license = "MIT";
      license-file = "LICENSE";
      author = "reo101";
      component.language = "GHC2021";
      default-extensions = [
        "DerivingStrategies"
        "DeriveAnyClass"
        "DataKinds"
        "BlockArguments"
        "LambdaCase"
        "ExplicitNamespaces"
        "RecordWildCards"
        "OverloadedRecordDot"
        "OverloadedStrings"
        "UndecidableInstances"
        "TypeFamilies"
      ];
      ghc-options = [
        "-Wall"
        "-Wunused-type-patterns"
        "-Wunused-packages"
        "-Wmissing-deriving-strategies"
        "-Wredundant-constraints"
        "-Widentities"
        "-Wmissing-export-lists"
        "-Wno-name-shadowing"
      ];
    };

    packages.crisp = {
      src = ./.;
      cabal.meta.synopsis = "Crisp, A Curried Lisp";
      override = { nodoc, ... }: nodoc;

      library = {
        enable = true;
        dependencies = [
          "containers"
          "haskeline"
          "mtl"
        ];
      };

      executable = {
        enable = true;
        dependencies = [
          "haskeline"
        ];
      };

      test = {
        enable = true;
        main = "Spec.hs";
        dependencies = [
          "base >=4.7 && <5"
          "containers"
          "generic-random"
          "hspec"
          "mtl"
        ];
      };
    };
  });
}
