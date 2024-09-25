{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
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
        self',
        system,
        lib,
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
              retrie = pkgs.haskell.packages.ghc910.retrie;
              Diff = pkgs.haskell.packages.ghc910.Diff_0_5;
              microstache = pkgs.haskell.packages.ghc910.microstache_1_0_3;
              aeson_2_2_3_0 = pkgs.haskell.packages.ghc910.aeson;
              th-desguar = pkgs.haskell.packages.ghc910.th-desugar_1_17;
              th-desugar_1_15 = pkgs.haskell.packages.ghc910.th-desguar_1_17;
            });

          packages = let
            hls-src = pkgs.fetchFromGitHub {
              owner = "haskell";
              repo = "haskell-language-server";
              rev = "f628754f20b63745d2312c392358dfdd0700837e";
              sha256 = "sha256-90WrnmTumnVXzMxK47CkXkcpu/qHj026TXKq7r3SBZs=";
            };
            haskell-MIP = pkgs.fetchFromGitHub {
              owner = "msakai";
              repo = "haskell-MIP";
              rev = "714c079b217e20442a57c95dedf8eaee08a8ce35";
              sha256 = "sha256-9sp1ytkcN0Vgk8fAa9MmbNS25fzcREpKYbneQ3VmHHc=";
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
            llvm-hs = pkgs.applyPatches {
              name = "llvm-hs-src";
              src = pkgs.fetchFromGitHub {
                owner = "noahmartinwilliams";
                repo = "llvm-hs";
                rev = "6f70e9846e425275252aaa1e10b31c1d33831e83";
                sha256 = "sha256-dIjO+pKdkM2EAI4vWoby8jREo9SgRn53gGNqCXYMtPY=";
              };
              patches = [./llvm-hs-fix.patch];
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
            singletons = pkgs.fetchFromGitHub {
              owner = "goldfirere";
              repo = "singletons";
              rev = "da13db1698eeb888eb866bab684e2ac759bd4134";
              sha256 = "sha256-ZaAeUaJes4U45Ql8L0ZLUCRLixZ3VKTGQsjz4Vi4GCk=";
            };
          in {
            apply-refact.source = pkgs.fetchFromGitHub {
              owner = "mpickering";
              repo = "apply-refact";
              rev = "623dc593f1f2091a63e04dd4f3ff89d0db223d77";
              sha256 = "sha256-M/0rqgnV4PjqiM50K7vTg2CqYcB6yf66k8H+Qzn/UDE=";
            };
            hlint.source = pkgs.fetchFromGitHub {
              owner = "ndmitchell";
              repo = "hlint";
              rev = "49f47288040400f1a4c4af71646d2037fdbbd72b";
              sha256 = "sha256-gHVNiyYkCX8DKzOEt2HeV+2v/AyafpjsgfZbZjHo0lM=";
            };
            fourmolu.source = pkgs.fetchFromGitHub {
              owner = "fourmolu";
              repo = "fourmolu";
              rev = "7380fb62a78a80deaa696ec0d29495556262827b";
              sha256 = "sha256-k3dg/+JHqe1kyGXlFAeeDMLc39Qx3ABFVA4PQ+l/z6Q=";
            };
            haskell-language-server.source = hls-src;
            ghcide.source = "${hls-src}/ghcide";
            hls-test-utils.source = "${hls-src}/hls-test-utils";
            hls-graph.source = "${hls-src}/hls-graph";
            hls-plugin-api.source = "${hls-src}/hls-plugin-api";
            hie-compat.source = "${hls-src}/hie-compat";
            cabal-add.source = pkgs.fetchFromGitHub {
              owner = "Bodigrim";
              repo = "cabal-add";
              rev = "c47b1097c2912bec7d9f3d7202c51483f5c89f5f";
              sha256 = "sha256-Fmber0Cpu7OsHjj4LO9UbhR7GreGxSR1hEjPsHnnPmw=";
            };
            statistics.source = "0.16.2.1";
            MIP.source = "${haskell-MIP}/MIP";
            accelerate.source = accelerate;
            accelerate-llvm.source = "${accelerate-llvm}/accelerate-llvm";
            accelerate-llvm-native.source = "${accelerate-llvm-native}/accelerate-llvm-native";
            llvm-hs.source = "${llvm-hs}/llvm-hs";
            llvm-hs-pure.source = "${llvm-hs}/llvm-hs-pure";
            th-desugar.source = pkgs.fetchFromGitHub {
              owner = "goldfirere";
              repo = "th-desugar";
              rev = "df957ef8c5586ab2653c325549b2c805bae8bd6f";
              sha256 = "sha256-xRHlGzc3s2+0sJA/FuCf9tyMEhmRIANBpA1NyXpFMbI=";
            };
            singletons.source = "${singletons}/singletons";
            singletons-th.source = "${singletons}/singletons-th";
            singletons-base.source = "${singletons}/singletons-base";
          };

          settings = {
            unification-fd.broken = false;

            http2.jailbreak = true;
            fourmolu.jailbreak = true;
            apply-refact.jailbreak = true;
            extensions.jailbreak = true;
            binary-instances.jailbreak = true;
            unification-fd.jailbreak = true;
            universe-base.jailbreak = true;
            universe-reverse-instances.jailbreak = true;
            autoapply.jailbreak = true;

            lifted-base.check = false;
            call-stack.check = false;
            bsb-http-chunked.check = false;
            http2.check = false;
            retrie.check = false;
            apply-refact.check = false;
            fourmolu.check = false;
            cabal-add.check = false;
            bytestring-encoding.check = false;

            hls-plugin-api = {
              self,
              super,
              ...
            }: {
              extraBuildDepends = with self; [Diff];
              check = false;
            };

            haskell-language-server = {
              self,
              super,
              ...
            }: {
              cabalFlags = {
                with-retrie = true;
                with-floskell = false;
                with-ormolu = false;
                with-stylish-haskell = false;
                with-hlint = true;
              };
              extraTestToolDepends = with pkgs; [git self.cabal-fmt];
              extraBuildDepends = with self; [hlint apply-refact ghc-lib-parser-ex refact cabal-add];
              extraSetupDepends = with self; [pkgs.pkg-config];
              sharedExecutables = false;
              custom = drv:
                (with pkgs.haskell.lib; enableCabalFlag (enableCabalFlag (enableCabalFlag drv "semanticTokens") "retrie") "hlint").overrideAttrs (old: {
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

            MIP = {
              self,
              super,
              ...
            }: {
              broken = false;
            };
            vulkan-utils = {
              custom = drv:
                drv.overrideAttrs (old: {
                  meta.badPlatforms = [];
                });
            };
            VulkanMemoryAllocator = {
              custom = drv:
                drv.overrideAttrs (old: {
                  meta.badPlatforms = [];
                });
            };
            accelerate = {
              self,
              super,
              ...
            }: {
              cabalFlags = {
                nofib = true;
              };
              extraBuildDepends = with self; [tasty-expected-failure tasty-hedgehog tasty-hunit tasty-rerun];
            };
            accelerate-llvm = {
              extraPkgconfigDepends = [pkgs.zlib pkgs.llvmPackages.libcxx];
              extraBuildTools = [pkgs.libllvm];
            };
            accelerate-llvm-native = {
              extraPkgconfigDepends = [pkgs.zlib pkgs.llvmPackages.libcxx];
              extraBuildTools = [pkgs.libllvm pkgs.llvmPackages.bintools-unwrapped pkgs.which];
              custom = drv:
                drv.overrideAttrs (old: {
                  preCheck = ''
                    export HOME=$(pwd)
                    export LD=clang
                  '';
                });
            };
            llvm-hs = {
              self,
              super,
              ...
            }: {
              cabalFlags = {
                with-llvm-with-rtti = false;
              };
              extraBuildTools = [pkgs.libllvm self.hsc2hs];
              extraPkgconfigDepends = [pkgs.xml2 pkgs.llvmPackages.libcxx];
              extraBuildDepends = [pkgs.llvmPackages.libcxx self.hsc2hs];
              # extraBuildFlags = ["--ghc-options='-optcxx=-std=c++14 -optcxx=-lstdc++ -optcxx=-fno-rtti'"];
            };
          };
        };
        haskellProjects.default = let
        in {
          basePackages = config.haskellProjects.ghc910.outputs.finalPackages;
          projectRoot = ./.;

          # packages = {
          #   accelerate-vulkan.source = pkgs.fetchFromGitHub {
          #     owner = "largeword";
          #     repo = "accelerate-vulkan";
          #     rev = "777ed17b0dcba997f5515c6910385672a3d12a7a";
          #     sha256 = "sha256-c2AD+CFRtueT1Ts51iap1dIDO+w8fxWbn7kPM3A30IM=";
          #   };
          # };
          packages = {
            bitfield.source = "0.0.0.1";
          };
          settings = {
            singletons-th.jailbreak = true;
            sudoku = {
              extraBuildDepends = with pkgs; [
              ];
              extraBuildTools = with pkgs; [
                # vulkan-loader
              ];
            };
            bitfield = {
              broken = false;
            };
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
            # vulkan = {
            #   custom = drv:
            #     drv.overrideAttrs (old: {
            #       meta.badPlatforms = [];
            #     });
            # };
          };

          devShell = {
            hlsCheck.enable = pkgs.stdenv.isDarwin;
            hoogle = true;
            tools = hs: {
              inherit (hs) cabal-install fourmolu_0_16_2_0 hs-speedscope;
            };
          };
        };

        packages.default = self'.packages.smonad;
        formatter = inputs.nixpkgs.legacyPackages.${system}.alejandra;
      };
    };
}
