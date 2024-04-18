{ reflex-platform ? ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "a4664e3b99f1b89a77ac759501b6bd85e18eac95";
    sha256 = "1pjzjhj966rs16f4b80ir9v8d7g7q2aa0i0zs30fny3x0xk1b4ah";
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
