{ pkgs }: {
  allowUnfree = true;
  packageOverrides = super: let self = super.pkgs; in {
    # Builds vim from Github master based on the current Nixpkg derivation
    # vim8 = pkgs.vim.overrideDerivation (oldAttrs : {
    #  name = "vim-8";
    #  version = "324a78f3b649e7b14741519ecf19c4aba178772d";
    #
    #  src = pkgs.fetchFromGitHub {
    #    owner = "vim";
    #    repo = "vim";
    #    rev = "324a78f3b649e7b14741519ecf19c4aba178772d";
    #    sha256 = "0gmpdsg6h3fhm2kdcgnw1il0av4lxnsnd7gb7vcndi09yq96hcn0";
    #  };
    # });
  };
}
