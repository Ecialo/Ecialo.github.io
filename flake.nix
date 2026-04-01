{
  inputs = {
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };
  outputs = inputs: inputs.ghc-wasm-meta.inputs.flake-utils.lib.eachDefaultSystem (system:
    let pkgs = inputs.ghc-wasm-meta.inputs.nixpkgs.legacyPackages.${system};
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
          inputs.ghc-wasm-meta.packages.${system}.all_9_12
          pkgs.dart-sass
        ];
        buildInputs = with pkgs; [
          # Try to include system libraries that might help
          pkg-config
        ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
          # macOS specific - provide system headers
          darwin.apple_sdk.frameworks.CoreFoundation
          darwin.apple_sdk.frameworks.SystemConfiguration
        ];
        shellHook = ''
          # Try to make system headers available to WASM compiler
          if [ -d "/usr/include" ]; then
            export C_INCLUDE_PATH="/usr/include:$C_INCLUDE_PATH"
            export CPATH="/usr/include:$CPATH"
          fi
          
          # macOS specific paths
          if [ -d "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include" ]; then
            export C_INCLUDE_PATH="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include:$C_INCLUDE_PATH"
            export CPATH="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include:$CPATH"
          fi
          
          # Configure for wasm32-wasi-clang with system headers
          export CFLAGS="-I/usr/include $CFLAGS"
          export CXXFLAGS="-I/usr/include $CXXFLAGS"
          
          echo "WASM environment with system headers configured"
        '';
      };
    });
}