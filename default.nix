{ reflex-platform ? ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "efc6d923c633207d18bd4d8cae3e20110a377864";
    sha256 = "121rmnkx8nwiy96ipfyyv6vrgysv0zpr2br46y70zf4d0y1h1lz5";
    })
}:
(import reflex-platform {}).project ({ pkgs, ... }:{
  useWarp = true;

  packages = {
    reflex-dom-extra = ../reflex-dom-extra;
  };

  shellToolOverrides = ghc: super: {
    closure-compiler = null;
    haskell-ide-engine = null;
    hdevtools = null;
    hlint = null;
    stylish-haskell = null;
    hoogle = null;
  };

  shells = {
    ghc = ["reflex-dom-extra"];
    ghcjs = ["reflex-dom-extra"];
  };
})
