{
  description = "Crisp, a Curried Lisp";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    hix = {
      url = "https://flakehub.com/f/tek/hix/~0.7.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs: inputs.hix ({ config, lib, ... }: let
    pkgs = inputs.nixpkgs.legacyPackages.${config.system};
  in {
    envs.dev = {
      ghc.compiler = "ghc946";
      hls.enable = true;
      buildInputs = with pkgs; [
        nixVersions.latest
        mktemp
        haskellPackages.hoogle
      ];
    };

    overrides = { hackage, unbreak, ... }: {
      # "comonad-extras" = unbreak;
    };

    cabal = {
      license = "MIT";
      license-file = "LICENSE";
      author = "reo101";
      component.language = "GHC2021";

      default-extensions = [
        "AllowAmbiguousTypes"
        "BlockArguments"
        "DataKinds"
        "DeriveAnyClass"
        "DeriveGeneric"
        "DerivingStrategies"
        "DuplicateRecordFields"
        "ExplicitForAll"
        "ExplicitNamespaces"
        "FlexibleContexts"
        "GADTSyntax"
        "GADTs"
        "LambdaCase"
        "MultiWayIf"
        "NoFieldSelectors"
        "NoMonomorphismRestriction"
        "OverloadedLabels"
        "OverloadedRecordDot"
        "OverloadedStrings"
        "PolyKinds"
        "RankNTypes"
        "RecordWildCards"
        "ScopedTypeVariables"
        "TemplateHaskell"
        "TypeApplications"
        "TypeFamilies"
        "TypeOperators"
        "UndecidableInstances"
        "UnicodeSyntax"
        "ViewPatterns"
      ];

      # prelude = {
      #   enable = true;
      #   package = "base";
      #   # module = "CustomPrelude";
      #   # "base hiding (Prelude)"
      #   # "crisp (CustomPrelude as Prelude)"
      # };
      # baseHide = {
      #   mixin = [
      #     "hiding (Prelude)"
      #     "(Prelude as BasePrelude)"
      #   ];
      # };

      ghc-options = [
        "-Wall"
        "-Wunused-type-patterns"
        "-Wunused-packages"
        "-Wmissing-deriving-strategies"
        "-Wredundant-constraints"
        "-Widentities"
        "-Wmissing-export-lists"
        "-Wno-name-shadowing"
        "-O2"
        "-flate-specialise"
        "-fspecialise-aggressively"
        "-fplugin=Polysemy.Plugin"
      ];
    };

    packages.crisp = {
      src = lib.fileset.toSource rec {
        root = ./.;
        fileset = lib.fileset.fileFilter (file:
          !(lib.any lib.id [
            (lib.hasSuffix ".nix" file.name)
            (file.name == ".jj" && file.type == "directory")
            (file.name == "flake.lock")
            (file.name == "dist-newstyle" && file.type == "directory")
            (file.name == "notes" && file.type == "directory")
          ])
        ) root;
      };
      cabal.meta.synopsis = "Crisp, A Curried Lisp";
      override = { nodoc, ... }: nodoc;

      library = {
        enable = true;
        dependencies = [
          "bifunctors"
          "comonad"
          # NOTE: broken in `nixpkgs`
          # "data-aviary"
          "composition"
          "deriving-compat"
          "extra"
          "free"
          "generic-lens"
          "haskeline"
          "lens"
          "megaparsec"
          "polysemy"
          "polysemy-plugin"
          "pretty-simple"
          "some"
          "text"
          # NOTE: for `Polysemy.Megaparsec`
          # "transformers"
          "utility-ht"
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
          # Base
          "base >=4.7 && <5"

          # QuickCheck
          "QuickCheck"
          "quickcheck-instances"

          # HUnit && hspec
          "HUnit"
          "hspec"
          "hspec-discover"

          # Other
          "megaparsec"
          "hspec-megaparsec"
          "lens-properties"
          "text"
        ];
      };
    };
  });
}
