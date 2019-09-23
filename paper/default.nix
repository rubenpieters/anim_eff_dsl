# nix-shell --pure --command 'make'

{ pkgs ? import <nixpkgs>{} } :

with pkgs;

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
                  haskellPackages.lhs2tex
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
