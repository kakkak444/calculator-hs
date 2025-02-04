{
	description = "Environment of calculator-hs";

	inputs = {
		nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
		flake-util.url = "github:numtide/flake-utils";
	};

	outputs = { nixpkgs, flake-utils, ... }:
		flake-utils.lib.eachDefaultSystem (
			system:
				let
					overlays = [];
					pkgs = import nixpkgs {
						inherit system overlays;
					};
				in
				{
					devShells.default = pkgs.mkShell {
						buildInputs = [
							pkgs.cabal-install
							pkgs.haskell-language-server
							pkgs.haskell.compiler.ghc910
						];
					};
				}
		);
}
