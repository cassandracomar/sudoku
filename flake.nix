{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    llvm-hs.url = "github:cassandracomar/llvm-hs/llvm-16";
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
      }: let
        haskellLibUncomposable = import "${inputs.nixpkgs}/pkgs/development/haskell-modules/lib" {
          inherit (pkgs) lib;
          inherit pkgs;
        };
        callPackage = pkgs.newScope {
          haskellLib = haskellLibUncomposable.compose;
          overrides = pkgs.haskell.packageOverrides;
          stdenv = pkgs.llvmPackages_16.libcxxStdenv;
        };
        ghc =
          (callPackage "${inputs.nixpkgs}/pkgs/development/compilers/ghc/9.10.1.nix" {
            bootPkgs = pkgs.pkgsBuildBuild.haskell.packages.ghc963Binary;
            buildTargetLlvmPackages = pkgs.llvmPackages_16;
            llvmPackages = pkgs.llvmPackages_16;
            useLLVM = false;
            inherit (pkgs.pkgsBuildBuild.darwin) autoSignDarwinBinariesHook xattr;
          })
          .overrideAttrs (old: let
            targetPlatform = pkgs.llvmPackages_16.libcxxStdenv.targetPlatform.config;
            hadrianFlags = ["*.*.ghc.*.opts += -optc--target=${targetPlatform} -optcxx--target=${targetPlatform}" "*.*.cc.c.opts += --target=${targetPlatform}"];
          in {
            configurePlatforms = ["build" "host" "target"];
            configureFlags = (old.configureFlags or []) ++ ["--target=${targetPlatform}" "LlvmTarget=${targetPlatform}" "bootstrap_llvm_target=${targetPlatform}"];
            preConfigure =
              (old.preConfigure or "")
              + ''
                hadrianFlagsArray+=(${pkgs.lib.escapeShellArgs hadrianFlags})
                sed -i 's/arm64-apple-darwin/aarch64-apple-darwin/' llvm-targets
                sed -i 's/LlvmMaxVersion=16/LlvmMaxVersion=17/' configure
              '';
          });
        haskellPackages = callPackage "${inputs.nixpkgs}/pkgs/development/haskell-modules" {
          inherit ghc;
          buildHaskellPackages = haskellPackages;
          compilerConfig = callPackage "${inputs.nixpkgs}/pkgs/development/haskell-modules/configuration-ghc-9.10.x.nix" {};
        };
      in {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          config.replaceStdenv = {pkgs, ...}: pkgs.llvmPackages_16.libcxxStdenv;
          overlays = [
            (final: prev: {
              inherit haskellPackages;
              pythonPackages = prev.pythonPackages.overrides (final': prev': {
                sphinx = prev.sphinx.override {
                  doCheck = false;
                };
              });
            })
          ];
        };
        haskellProjects.ghc910 = {
          defaults.packages = {}; # Disable scanning for local package
          devShell.enable = false; # Disable devShells
          autoWire = []; # Don't wire any flake outputs

          basePackages =
            haskellPackages
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
            accelerate-llvm = pkgs.applyPatches {
              name = "accelerate-llvm-src";
              src = pkgs.fetchFromGitHub {
                owner = "acceleratehs";
                repo = "accelerate-llvm";
                rev = "eb544e52e66509314c5efe9f9765c5e42d00c5a4";
                sha256 = "sha256-EXMfVFFUGAlwo3+E5krJIid8lMb5B2bGD0zRi+Jq9u4=";
              };
              patches = [./accelerate-llvm-fix.patch];
            };
            accelerate = pkgs.applyPatches {
              name = "accelerate-src";
              src = pkgs.fetchFromGitHub {
                owner = "acceleratehs";
                repo = "accelerate";
                rev = "237303a660a41f04e43b1661c3fa31528be7927b";
                sha256 = "sha256-B91EucR+RujhJFEjM0794/0ecPJFDPiUbE1DsXCaHxE=";
              };
              patches = [./accelerate-fix.patch];
            };
            accelerate-llvm-native = pkgs.runCommand "prep-accelerate-llvm-source" {} ''
              mkdir -p $out
              cp -rL ${accelerate-llvm}/accelerate-llvm-native/ $out
            '';
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
            accelerate.source = accelerate;
            accelerate-llvm.source = "${accelerate-llvm}/accelerate-llvm";
            accelerate-llvm-native.source = "${accelerate-llvm-native}/accelerate-llvm-native";
            llvm-hs.source = "${inputs.llvm-hs}/llvm-hs";
            llvm-hs-pure.source = "${inputs.llvm-hs}/llvm-hs-pure";
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
              extraTestToolDepends = with pkgs; [git self.cabal-fmt self.cabal-install];
              extraBuildDepends = with self; [hlint apply-refact ghc-lib-parser-ex refact cabal-add];
              extraSetupDepends = [pkgs.pkg-config];
              sharedExecutables = false;
              custom = drv:
                pkgs.haskell.lib.compose.overrideCabal (old: {
                  postInstall =
                    (old.postInstall or "")
                    + ''
                      find $out/lib/ghc-9.10.1/lib/aarch64-osx-ghc-9.10.1-inplace -name "*.dylib" -exec ln -sf {} $out/lib/links/ \;
                    '';
                })
                drv;
              patches = [
                ./hls-enable-hlint.patch
              ];
              jailbreak = true;
              check = false;
            };
            accelerate = {self, ...}: {
              cabalFlags = {
                nofib = true;
              };
              extraBuildDepends = with self; [tasty-expected-failure tasty-hedgehog tasty-hunit tasty-rerun];
            };
            accelerate-llvm = {
              extraPkgconfigDepends = [pkgs.zlib];
              extraBuildTools = with pkgs.llvmPackages_16; [libllvm];
            };
            accelerate-llvm-native = {
              extraPkgconfigDepends = with pkgs.llvmPackages_16; [pkgs.zlib libcxx];
              extraBuildTools = with pkgs.llvmPackages_16; [libllvm bintools pkgs.which];
              custom = drv:
                drv.overrideAttrs (old: {
                  preCheck = ''
                    export HOME=$(pwd)
                    export LD=clang
                  '';
                });
            };
            llvm-hs = {self, ...}: {
              extraBuildTools = with pkgs.llvmPackages_16; [self.hsc2hs libllvm.dev];
              extraPkgconfigDepends = with pkgs; [zlib xml2];
              extraLibraries = with pkgs.llvmPackages_16; [libllvm.lib];
              extraConfigureFlags = with pkgs.llvmPackages_16; [
                "--ghc-option=-pgmotool=${libllvm}/bin/llvm-otool"
                "--ghc-option=-pgminstall_name_tool=${libllvm}/bin/llvm-install-name-tool"
              ];
            };
          };
        };
        haskellProjects.default = {
          basePackages = config.haskellProjects.ghc910.outputs.finalPackages;
          projectRoot = ./.;

          settings = {
            text-show.extraConfigureFlags = ["--ghc-options=-fexpose-all-unfoldings"];
            containers-accelerate = {
              custom = drv:
                drv.overrideAttrs (old: {
                  preCheck = ''
                    export HOME=.
                    export LD=clang
                  '';
                });
            };
            lens-accelerate.jailbreak = true;
            sudoku = {
              extraBuildTools = with pkgs.llvmPackages_16; [libllvm.dev];
              extraLibraries = with pkgs.llvmPackages_16; [libllvm.lib];
            };
          };

          devShell = {
            hlsCheck.enable = pkgs.stdenv.isDarwin;
            hoogle = true;
            tools = hs: {
              inherit (hs) cabal-install fourmolu hlint;
            };
            mkShellArgs = {
              packages = with pkgs.llvmPackages_16; [libllvm libllvm.dev libllvm.lib];
            };
          };
        };

        packages.ghc910 = ghc;
        packages.llvm-hs = config.haskellProjects.ghc910.outputs.finalPackages.llvm-hs;
        packages.stdenv = pkgs.stdenv;
        packages.libllvm = pkgs.llvmPackages_16.libllvm;

        packages.default = self.packages.sudoku;
        formatter = inputs.nixpkgs.legacyPackages.${system}.alejandra;
      };
    };
}
