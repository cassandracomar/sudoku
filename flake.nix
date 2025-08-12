{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    # llvm-hs.url = "github:cassandracomar/llvm-hs/llvm-16";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = {
        self',
        system,
        config,
        pkgs,
        ...
      }: let
        # nixpkgs' = inputs.nixpkgs.legacyPackages.${system}.applyPatches {
        #   name = "nixpkgs-patched";
        #   src = inputs.nixpkgs;
        #   patches = [
        #     ./containers-0-8.patch
        #   ];
        # };
        # accelerate-io = pkgs.fetchFromGitHub {
        #   owner = "acceleratehs";
        #   repo = "accelerate-io";
        #   rev = "972aa4bca622252b23cac45f8f7a6e28f9a644dc";
        #   sha256 = "sha256-h9fvrYgEVJlQkvMy+AYWehNnlYeqL5Nfrvfy8nH428w=";
        # };
        # capstone = pkgs.capstone.overrideAttrs (old: rec {
        #   version = "5.0.3";
        #   src = pkgs.fetchFromGitHub {
        #     owner = "capstone-engine";
        #     repo = "capstone";
        #     rev = version;
        #     hash = "sha256-LZ10czBn5oaKMHQ8xguC6VZa7wvEgPRu6oWt/22QaDs=";
        #   };
        # });
        libcxxStdenv =
          pkgs.overrideCC (
            pkgs.llvmPackages.libcxxStdenv.override (old: {
              hostPlatform =
                (old.hostPlatform or {})
                // {
                  useLLVM = true;
                  linker = "lld";
                };
              buildPlatform =
                (old.buildPlatform or {})
                // {
                  useLLVM = true;
                  linker = "lld";
                };
              targetPlatform =
                (old.targetPlatform or {})
                // {
                  useLLVM = true;
                  linker = "lld";
                };
            })
          )
          pkgs.llvmPackages.clangUseLLVM;
        targetPackages = {
          stdenv = libcxxStdenv;
        };
        ghc = (pkgs.haskell.compiler.ghc9122.override ({
            useLLVM = true;
          }
          // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
            inherit targetPackages;
            stdenv = libcxxStdenv;
            buildPackages = targetPackages;
            pkgsHostTarget = {
              inherit targetPackages;
              inherit (pkgs.pkgsLLVM) elfutils gmp libffi ncurses numactl;
            };
            pkgsBuildTarget = {
              inherit targetPackages;
            };
          })).overrideAttrs (old:
          pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
            hardeningDisable = (old.hardeningDisable or []) ++ ["fortify"];
            hadrianFlags = (old.hadrianFlags or []) ++ ["-j"];
          });
        haskellPackages = pkgs.callPackage "${inputs.nixpkgs}/pkgs/development/haskell-modules" {
          inherit ghc;
          haskellLib = pkgs.haskell.lib.compose;
          buildHaskellPackages = haskellPackages;
          stdenv =
            if pkgs.stdenv.isLinux
            then libcxxStdenv
            else pkgs.stdenv;
          compilerConfig = pkgs.callPackage "${inputs.nixpkgs}/pkgs/development/haskell-modules/configuration-ghc-9.12.x.nix" {
            haskellLib = pkgs.haskell.lib.compose;
          };
        };
      in {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          # config.replaceStdenv = {pkgs}:
          #   if pkgs.stdenv.hostPlatform.isLinux
          #   then pkgs.llvmPackages.libcxxStdenv
          #   else pkgs.stdenv;
          # overlays = [
          #   (final: prev:
          #     prev.lib.optionalAttrs prev.stdenv.isLinux rec {
          #       python3 = prev.python3.override {
          #         packageOverrides = pyfinal: pyprev: {
          #           pycparser = pyprev.pycparser.overrideAttrs (old: {
          #             unittestCheckPhase = "true";
          #           });
          #           sphinx = pyprev.sphinx.overrideAttrs (old: {
          #             pytestCheckPhase = "true";
          #             unittestCheckPhase = "true";
          #             pythonImportsCheckPhase = "true";
          #           });
          #         };
          #       };
          #       python3Packages = python3.pkgs;
          #       sphinx = python3Packages.sphinx;
          #     })
          # ];
        };
        haskellProjects.ghc912 = {
          defaults.packages = {}; # Disable scanning for local package
          devShell.enable = false; # Disable devShells
          autoWire = []; # Don't wire any flake outputs

          basePackages = haskellPackages.extend (final: prev: {
            Cabal = prev.Cabal_3_14_2_0;
            Cabal-syntax = prev.Cabal-syntax_3_14_2_0;
            stylish-haskell = prev.stylish-haskell_0_15_0_1;
          });

          packages = let
            hls-src = pkgs.applyPatches {
              src = pkgs.fetchFromGitHub {
                owner = "haskell";
                repo = "haskell-language-server";
                rev = "e3d38b0c4666681dca39901aa9d47b04422c82ff";
                sha256 = "sha256-FMCBKSS0pqKtPXqPrM95Gn9Mj7F9uPq4hLMMIXSlqaw=";
              };
              patches = [
                ./inlay-hints-local-binding.patch
                ./package-import-inlay-hints.patch
              ];
            };
            hie-bios = pkgs.fetchFromGitHub {
              owner = "haskell";
              repo = "hie-bios";
              rev = "1383519eabf349c300ca53b9efde76eaa36aa481";
              sha256 = "sha256-G6H0VFjpqm3xwOeRT78+FBfMbRjfgA97u1WYBTk/bvU=";
            };
            cabal-install-parsers = pkgs.fetchFromGitHub {
              owner = "haskell-ci";
              repo = "haskell-ci";
              rev = "5bb403c3325ebae8f4d02eaa3d93a6f91f1c217d";
              sha256 = "sha256-XJ7/aEM+3uFV7MYEXJxVuWSsSzV8XkqyB0EtxJ/JdMc=";
            };
            # accelerate-llvm = pkgs.applyPatches {
            #   name = "accelerate-llvm-src";
            #   src = pkgs.fetchFromGitHub {
            #     owner = "acceleratehs";
            #     repo = "accelerate-llvm";
            #     rev = "eb544e52e66509314c5efe9f9765c5e42d00c5a4";
            #     sha256 = "sha256-EXMfVFFUGAlwo3+E5krJIid8lMb5B2bGD0zRi+Jq9u4=";
            #   };
            #   patches = [./accelerate-llvm-fix.patch];
            # };
            # accelerate = pkgs.fetchFromGitHub {
            #   owner = "acceleratehs";
            #   repo = "accelerate";
            #   rev = "bca75e0aac05630009cb315796ee1a28b4af15d4";
            #   sha256 = "sha256-yelMOEUpz4W1FQ/sV8pP85sjicMQoS7P2I9rvAZO+wc=";
            # };
            # accelerate-llvm-native = pkgs.runCommand "prep-accelerate-llvm-source" {} ''
            #   mkdir -p $out
            #   cp -rL ${accelerate-llvm}/accelerate-llvm-native/ $out
            # '';
            # accelerate-io-vector = pkgs.runCommand "prep-accelerate-io-vector-source" {} ''
            #   mkdir -p $out
            #   cp -rL ${accelerate-io}/accelerate-io-vector/ $out
            # '';
          in {
            singletons.source = "3.0.4";
            singletons-th.source = "3.5";
            singletons-base.source = "3.5";
            th-desugar.source = "1.18";
            cabal-fmt.source = pkgs.fetchFromGitHub {
              owner = "phadej";
              repo = "cabal-fmt";
              rev = "ecdbe8925ceeaf3a4c1c123f20d198e3376e3b6a";
              sha256 = "sha256-0S777PiZabqmXmYkTbDXZJ+Z1/SLV2fRHyvviwmqxbk=";
            };
            cabal-install-parsers.source = "${cabal-install-parsers}/cabal-install-parsers";
            tree-diff.source = pkgs.fetchFromGitHub {
              owner = "haskellari";
              repo = "tree-diff";
              rev = "83974f62eb1e9ac64b4eaca76a3e0c9c02066160";
              sha256 = "sha256-b2q5nsBZvujDnMK8fglW7ijbo5tbJud91OHDDxjqaAQ=";
            };
            haskell-language-server.source = hls-src;
            ghcide.source = "${hls-src}/ghcide";
            hls-test-utils.source = "${hls-src}/hls-test-utils";
            hls-graph.source = "${hls-src}/hls-graph";
            hls-plugin-api.source = "${hls-src}/hls-plugin-api";
            hie-bios.source = hie-bios;
            hiedb.source = pkgs.fetchFromGitHub {
              owner = "wz1000";
              repo = "HieDb";
              rev = "524bb3c2e4f00269591760a83f90405a8f7e9fc9";
              sha256 = "sha256-IOWPipoIKGOitV02FuyjSQxE5BwUmSrd8ImsL/djlVY=";
            };
            # accelerate.source = accelerate;
            # accelerate-llvm.source = "${accelerate-llvm}/accelerate-llvm";
            # accelerate-llvm-native.source = "${accelerate-llvm-native}/accelerate-llvm-native";
            # llvm-hs.source = pkgs.applyPatches {
            #   name = "llvm-hs";
            #   src = "${inputs.llvm-hs}/llvm-hs";
            #   patches = [./llvm-hs-fix.patch];
            # };
            # llvm-hs-pure.source = "${inputs.llvm-hs}/llvm-hs-pure";
            # accelerate-io-vector.source = "${accelerate-io-vector}/accelerate-io-vector";
            retrie.source = pkgs.applyPatches {
              name = "retrie";
              src = pkgs.fetchFromGitHub {
                owner = "wz1000";
                repo = "retrie";
                rev = "wip/ghc-9.10";
                sha256 = "sha256-xSH+B47ralaR4WvaoOpvXwlKdjXcneT5AMxaIjewl88=";
              };
              patches = [./retrie.patch];
            };
            # cabal-hoogle.source = pkgs.fetchFromGitHub {
            #   owner = "cassandracomar";
            #   repo = "cabal-hoogle";
            #   rev = "c5c12325f2713aa3856cf7b86bc05064484d3c26";
            #   sha256 = "sha256-grGuPu1F+Xl2OiYQOfZp7BQR9Kv/vvh+xpnfZdFkhpE=";
            # };
            stylish-haskell.source = "0.15.1.0";
          };

          settings = {
            singletons-base-code-generator.broken = false;
            cabal-fmt.jailbreak = true;
            cabal-install-parsers.jailbreak = true;
            statistics.jailbreak = true;
            lsp-types.jailbreak = true;
            lsp-test.jailbreak = true;
            ormolu.jailbreak = true;
            retrie.jailbreak = true;
            data-clist.jailbreak = true;
            hw-prim.jailbreak = true;

            cabal-install-parsers.check = false;
            singletons-base.check = false;
            hie-bios.check = false;
            hiedb.check = false;
            retrie.check = false;
            cryptonite.check = false;
            test-ini-compat.check = false;
            config-ini.check = false;
            hedgehog.check = false;
            fast-logger.check = false;
            tls.check = false;

            # cabal-hoogle = {self, ...}: {
            #   broken = false;
            #   check = false;
            #   jailbreak = true;
            #   extraBuildDepends = [self.cabal-install];
            #   custom = drv:
            #     drv.override {
            #       inherit (self) cabal-install;
            #     };
            # };

            doctest-parallel.custom = drv:
              drv.overrideAttrs (old: {
                patches = null;
              });

            hls-plugin-api = {
              extraBuildTools = [pkgs.git];
              jailbreak = true;
            };
            ghcide.jailbreak = true;

            haskell-language-server = {self, ...}: {
              cabalFlags = {
                retrie = false;
                floskell = false;
                ormolu = true;
                stylishhaskell = false;
                hlint = true;
                semanticTokens = true;
                stan = false;
                ignore-plugins-ghc-bounds = true;
                splice = true;
              };
              extraTestToolDepends = with pkgs; [git self.cabal-fmt self.cabal-install];
              extraBuildDepends = with self; [hlint apply-refact ghc-lib-parser-ex refact cabal-add];
              extraSetupDepends = [pkgs.pkg-config];
              staticLibraries = false;
              custom = drv:
                pkgs.haskell.lib.compose.overrideCabal (old: {
                  postInstall =
                    (old.postInstall or "")
                    + (pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isDarwin ''
                      find $out/lib/ghc-${ghc.version}/lib/aarch64-osx-ghc-${ghc.version}* -name "*.dylib" -exec ln -sf {} $out/lib/links/ \;
                      find ${ghc}/lib/ghc-${ghc.version}/lib/aarch64-osx-ghc-${ghc.version}* -name "*.dylib" -exec ln -sf {} $out/lib/links/ \;
                    '')
                    + (pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
                      find dist/build -name "*.so" -exec mv {} $out/lib/ \;
                    '');
                })
                ((drv.override {stylish-haskell = pkgs.hello;}).overrideAttrs (old: {
                  nativeBuildInputs =
                    (old.nativeBuildInputs or [])
                    ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [pkgs.autoPatchelfHook];
                }));
              jailbreak = true;
              check = false;
            };
            # accelerate = {self, ...}: {
            #   cabalFlags = {
            #     # nofib = true;
            #     # debug = true;
            #     bounds-checks = false;
            #   };
            #   extraBuildTools = with self; with pkgs; [cmake];
            #   extraPkgconfigDepends = with pkgs; [freetype glfw3 capstone zstd tbb pkgs.llvmPackages_16.libllvm hello];
            #   extraBuildDepends = with self; [tasty-expected-failure tasty-hedgehog tasty-hunit tasty-rerun];
            #   patches = [./accelerate-fix.patch];
            #   check = false;
            #   custom = drv:
            #     drv.overrideAttrs (old: {
            #       NIX_ENFORCE_NO_NATIVE = false;
            #       NIX_CFLAGS_COMPILE =
            #         (old.NIX_CFLAGS_COMPILE or "")
            #         + toString (
            #           []
            #           ++ pkgs.lib.optional (pkgs.llvmPackages_16.libcxxStdenv.cc.isClang
            #             && pkgs.llvmPackages_16.libcxxStdenv.hostPlatform.isDarwin) " -fno-lto"
            #         );
            #     });
            # };
            # accelerate-llvm = {
            #   extraPkgconfigDepends = [pkgs.zlib];
            #   extraBuildTools = with pkgs.llvmPackages_16; [libllvm];
            #   check = false;
            # };
            # accelerate-llvm-native = {
            #   extraPkgconfigDepends = with pkgs.llvmPackages_16; [pkgs.zlib libcxx];
            #   extraBuildTools = with pkgs.llvmPackages_16; [libllvm bintools pkgs.which];
            #   custom = drv:
            #     drv.overrideAttrs (old: {
            #       preCheck = ''
            #         export HOME=$(pwd)
            #         export LD=clang
            #       '';
            #     });
            #   check = false;
            # };
            # llvm-hs = {self, ...}: {
            #   extraBuildTools = with pkgs.llvmPackages_16; [self.hsc2hs libllvm.dev];
            #   extraPkgconfigDepends = with pkgs; [zlib xml2];
            #   extraLibraries = with pkgs.llvmPackages_16; [libllvm.lib];
            #   extraConfigureFlags = with pkgs.llvmPackages_16; [
            #     "--ghc-option=-pgmotool=${libllvm}/bin/llvm-otool"
            #     "--ghc-option=-pgminstall_name_tool=${libllvm}/bin/llvm-install-name-tool"
            #   ];
            # };
          };
        };
        haskellProjects.default = {
          basePackages =
            config.haskellProjects.ghc912.outputs.finalPackages.extend (final: prev: {
            });
          projectRoot = ./.;

          settings = {
            text-show.extraConfigureFlags = ["--ghc-options=-fexpose-all-unfoldings"];
            # lens-accelerate = {
            #   broken = false;
            #   jailbreak = true;
            # };
            sudoku = {self, ...}: {
              extraLibraries = [self.containers_0_8];
              custom = drv:
                pkgs.haskell.lib.compose.allowInconsistentDependencies (drv.override {
                  containers = self.containers_0_8;
                });
            };
          };

          devShell = {
            hlsCheck.enable = pkgs.stdenv.isDarwin;
            hoogle = true;
            tools = hs: {
              inherit
                (config.haskellProjects.ghc912.outputs.finalPackages)
                cabal-install
                fourmolu
                hlint
                # cabal-hoogle
                stylish-haskell
                cabal-gild
                cabal-fmt
                haskell-language-server
                ;
              graphviz = inputs.nixpkgs.legacyPackages.${system}.graphviz;
            };
            mkShellArgs = {
              packages = with pkgs.llvmPackages; [libllvm libllvm.dev libllvm.lib llvm];
              # shellHook = ''
              #   export LD="${pkgs.llvmPackages_16.libcxxStdenv.cc}/bin/clang"
              # '';
            };
          };
        };

        packages.ghc912 = ghc;
        # packages.llvm-hs = config.haskellProjects.ghc912.outputs.finalPackages.llvm-hs;
        packages.stdenv = pkgs.stdenv;
        packages.llvm = pkgs.llvmPackages.llvm;
        packages.libcxxStdenv = libcxxStdenv;

        packages.default = self'.packages.sudoku;
        formatter = inputs.nixpkgs.legacyPackages.${system}.alejandra;
      };
    };
}
