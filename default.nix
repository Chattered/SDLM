{ pkgs ? (import <nixpkgs> {}).pkgs
}:
let
  myemacs =
    let emacsWithImageMagick = with pkgs; recurseIntoAttrs (emacsPackagesNgGen (pkgs.emacs.override { imagemagick = pkgs.imagemagick; })); in
    with pkgs.emacsPackages; emacsWithImageMagick.emacsWithPackages
      [ pkgs.haskellPackages.ghc-mod haskellMode magit emacsw3m ];
   myhaskell =
     pkgs.haskellPackages.ghcWithPackages (p: with p; [
       base binary byteorder mtl PhiledCommon sdl2 sdl2-ttf

       cabal-install hlint
    ]);
in with pkgs; stdenv.mkDerivation {
  name = "SDLM";
  buildInputs = [ myemacs myhaskell pkgs.dejavu_fonts ];
  fonts = pkgs.dejavu_fonts;
  shellHook = ''
    export shellHook=
    emacs-tcp sdlm .emacs
  '';
}
#TODO: Dependency on which should be made runtime.
