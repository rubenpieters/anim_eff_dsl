# nix-shell --pure --command 'make'

{ pkgs ? import <nixpkgs>{} } :

with pkgs;

let
  ghc = haskellPackages.ghcWithPackages (p: [p.microlens]);
in
stdenv.mkDerivation {
  name = "paper";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # Add other LaTeX libraries (packages) here as needed, e.g:
                      minted
                      fvextra
                      ifplatform
                      xstring
                      framed
                      ifsym

                      # build tools
                      latexmk
                      ;
                  })
                  gnumake
                  ghc
                  haskellPackages.lhs2tex
                  haskellPackages.flat-tex
                  which
                  git
                  pythonPackages.virtualenv
                  pythonPackages.pip
                ];
  buildPhase = "make";
  shellHook = ''
    virtualenv --no-wheel --no-setuptools venv
    venv/bin/pip install -v pygments
    source venv/bin/activate
  '';
}
