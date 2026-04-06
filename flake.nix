{

  inputs = {
    miso.url = "github:dmjio/miso/a854d79d22582a4ab27048474f6ceb2b9b7ddcdb";
  };

  outputs = inputs:
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system: {
      devShell = inputs.miso.outputs.devShells.${system}.default;
      devShells.hls = inputs.miso.outputs.devShells.${system}.hls;
      devShells.wasm = inputs.miso.outputs.devShells.${system}.wasm;
      devShells.ghcjs = inputs.miso.outputs.devShells.${system}.ghcjs;
    });

}
