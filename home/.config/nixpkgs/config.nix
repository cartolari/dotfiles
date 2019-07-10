{
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        ag
        cdrkit
        curl
        file
        fzf
        git
        gptfdisk
        grml-zsh-config
        homesick
        htop
        inetutils
        less
        man
        man-pages
        nodejs-11_x
        openconnect
        openssh
        python
        python3
        python3Packages.pip
        rmlint
        tig
        tmux
        tree
        unzip
        zsh
      ];
    };
  };
}
