
.PHONY= update build optim

all: update build optim

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build homepage-exe
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin homepage-exe | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output static/ghc_wasm_jsffi.js
	cp -v $(my_wasm) static/

optim:
	wasm-opt -all -O2 static/homepage-exe.wasm -o static/homepage-exe.wasm
	wasm-tools strip -o static/homepage-exe.wasm static/homepage-exe.wasm

serve:
	http-server public

clean:
	rm -rf dist-newstyle public