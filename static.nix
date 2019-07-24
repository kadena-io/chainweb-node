{ system ? builtins.currentSystem
}:

let

rpSrc = builtins.fetchTarball {
  url = "https://github.com/vaibhavsagar/reflex-platform/archive/ae542c3e7ed4fb1b4552f447b1205982e261cd68.tar.gz";
  sha256 = "0p14b4kdjkykkcql8xdp2x8qvw7cla8imikl940a8qcsc49vkwpf";
};

overlay = self: super: {
  z3 = super.z3.overrideAttrs (drv: {
    src = self.fetchFromGitHub {
      owner = "Z3Prover";
      repo = "z3";
      rev = "727929c9af003d71eab1f0d90cc8e01761943491";
      sha256 = "02p8rhflimc852ysgf7nmaypz6ga3w4iss3z8d3qrby5a2d464p9";
    };
  });
};

rp = import rpSrc { inherit system; nixpkgsOverlays = [ overlay ]; config.allowBroken = true; };

in
  rp.ghcMusl64.override
  {
    overrides = self: super: (import ./overrides.nix rp.nixpkgs false false self super) // {
    };
  }
