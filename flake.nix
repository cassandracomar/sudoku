{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs: let
  in
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = {
        self,
        system,
        config,
        pkgs,
        ...
      }: {
        haskellProjects.ghc910 = {
          defaults.packages = {}; # Disable scanning for local package
          devShell.enable = false; # Disable devShells
          autoWire = []; # Don't wire any flake outputs

          basePackages =
            pkgs
            .haskell
            .packages
            .ghc910
            .extend (final: prev: {
              # Diff = pkgs.haskell.packages.ghc910.Diff_1_0_2;
              # Diff_0_5_0 = prev.Diff;
              # aeson_2_2_3_0 = pkgs.haskell.packages.ghc910.aeson;
              microstache = prev.microstache_1_0_3;
            });

          packages = let
            hls-src = pkgs.fetchFromGitHub {
              owner = "haskell";
              repo = "haskell-language-server";
              rev = "3ab746dce03f618a6b584c97fbc2a7a9dc03af72";
              sha256 = "sha256-6WXA2e/K4cFqIc71kz4Gij6BqkaNCR9qWV2JTexvXWk=";
            };
          in {
            Diff.source = "1.0.2";
            fourmolu.source = pkgs.fetchFromGitHub {
              owner = "fourmolu";
              repo = "fourmolu";
              rev = "v0.18.0.0";
              sha256 = "sha256-VygaYu/sK61TFaKXnsfC+GaXqRccb1Ue/4Ut5vbdpvA=";
            };
            ormolu.source = pkgs.fetchFromGitHub {
              owner = "tweag";
              repo = "ormolu";
              rev = "0.8.0.0";
              sha256 = "sha256-JpyNlb7GzauygKeGiNmGfaVDT7o/C9rtGpMoW4mfPVI=";
            };
            singletons.source = "3.0.3";
            singletons-th.source = "3.4";
            singletons-base.source = "3.4";
            th-desugar.source = "1.17";
            ghc-exactprint.source = "1.9.0.0";
            apply-refact.source = pkgs.fetchFromGitHub {
              owner = "mpickering";
              repo = "apply-refact";
              rev = "623dc593f1f2091a63e04dd4f3ff89d0db223d77";
              sha256 = "sha256-M/0rqgnV4PjqiM50K7vTg2CqYcB6yf66k8H+Qzn/UDE=";
            };
            hlint.source = pkgs.fetchFromGitHub {
              owner = "ndmitchell";
              repo = "hlint";
              rev = "v3.10";
              sha256 = "sha256-oe8TC0amRb4l3SHdx3pL7qYpgduJ+KUu/uG1hT9gjQ4=";
            };
            ghc-lib-parser.source = "9.12.1.20241218";
            ghc-lib-parser-ex.source = "9.12.0.0";
            haskell-language-server.source = hls-src;
            ghcide.source = "${hls-src}/ghcide";
            hls-test-utils.source = "${hls-src}/hls-test-utils";
            hls-graph.source = "${hls-src}/hls-graph";
            hls-plugin-api.source = "${hls-src}/hls-plugin-api";
            hie-compat.source = "${hls-src}/hie-compat";
            cabal-add.source = pkgs.fetchFromGitHub {
              owner = "Bodigrim";
              repo = "cabal-add";
              rev = "870357e77d3ae3f65f8054860cd8e6ccbfc5f04e";
              sha256 = "sha256-RyjEdOJeFtmaOao5Kqa+G2laNCo55CongJqEUDXcvQM=";
            };
          };

          settings = {
            binary-instances.jailbreak = true;
            cabal-install-parsers.jailbreak = true;
            lsp-types.jailbreak = true;
            lsp-test.jailbreak = true;
            apply-refact.jailbreak = true;

            aeson.check = false;
            apply-refact.check = false;
            cabal-add.check = false;
            singletons-base.check = false;
            fourmolu.check = false;

            fourmolu.patches = [
              ./fourmolu-revert-Cabal-syntax-3.14.patch
            ];
            ormolu.patches = [
              ./ormolu-revert-Cabal-syntax-3.14.patch
            ];

            hls-plugin-api = {
              extraBuildTools = [pkgs.git];
              jailbreak = true;
            };
            ghcide.jailbreak = true;

            haskell-language-server = {self, ...}: {
              cabalFlags = {
                retrie = true;
                floskell = false;
                ormolu = true;
                stylishhaskell = false;
                hlint = true;
                semanticTokens = true;
                stan = false;
              };
              extraTestToolDepends = with pkgs; [git self.cabal-fmt];
              extraBuildDepends = with self; [hlint apply-refact ghc-lib-parser-ex refact cabal-add];
              extraSetupDepends = [pkgs.pkg-config];
              sharedExecutables = false;
              custom = drv:
                drv.overrideAttrs (old: {
                  HOME = "."; # fix tests trying to create paths under $HOME

                  postInstall =
                    old.postInstall
                    or ""
                    + ''
                      set -x
                      find $out/lib/ghc-9.10.1/lib/aarch64-osx-ghc-9.10.1-inplace -name "*.dylib" -exec ln -sf {} $out/lib/links/ \;
                      set +x
                    '';
                });
              check = false;
              patches = [
                ./hls-enable-hlint.patch
              ];
              jailbreak = true;
            };
          };
        };
        haskellProjects.default = let
        in {
          basePackages = config.haskellProjects.ghc910.outputs.finalPackages;
          projectRoot = ./.;

          settings = {
            text-show.extraConfigureFlags = ["--ghc-options=-fexpose-all-unfoldings"];
          };

          devShell = {
            hlsCheck.enable = pkgs.stdenv.isDarwin;
            hoogle = true;
            tools = hs: {
              inherit (hs) cabal-install fourmolu hlint stan;
            };
          };
        };

        packages.default = self.packages.smonad;
        formatter = inputs.nixpkgs.legacyPackages.${system}.alejandra;
      };
    };
}
