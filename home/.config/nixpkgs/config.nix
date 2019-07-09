{
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        adapta-gtk-theme
        ag
        augeas
        (autojump.overrideAttrs (attrs: {
          meta.priority = 3;
        }))
        awscli
        bridge-utils
        cdrkit
        cloc
        compton
        cryptsetup
        curl
        dnsmasq
        docker
        docker-compose
        file
        fontconfig
        fuse
        fzf
        git
        git-lfs
        gnused
        go
        gptfdisk
        gnugrep
        grml-zsh-config
        homesick
        htop
        inetutils
        less
        man
        man-pages
        ncurses5
        nodejs-11_x
        open-sans
        openbox
        openconnect
        openssh
        papirus-icon-theme
        python
        python2Packages.supervisor
        python3
        python3Packages.pip
        python3Packages.setuptools
        qemu
        rclone
        rclone-browser
        readline70
        ripgrep
        rmlint
        (ruby_2_6.overrideAttrs (attrs: {
          meta.priority = 3;
        }))
        s3cmd
        shellcheck
        socat
        sqlite
        terraform
        tig
        tigervnc
        tint2
        tmux
        tree
        unzip
        vagrant
        vimPlugins.youcompleteme
        (vim_configurable.override (attrs: {
          meta.priority = 3;
          python = python3;
        }))
        wget
        zip
        zsh
        zsh-autosuggestions
        zsh-syntax-highlighting
      ];
    };
  };
}
