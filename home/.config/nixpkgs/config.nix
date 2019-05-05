{
  packageOverrides = pkgs: with pkgs; {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        ag
        appimage-run
        augeas
        (autojump.overrideAttrs (attrs: {
          meta.priority = 3;
        }))
        aws
        bridge-utils
        cloc
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
        grml-zsh-config
        homesick
        htop
        inetutils
        less
        lxc
        lxd
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
        python3
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
        tmux
        tree
        unzip
        vagrant
        (vimHugeX.overrideAttrs (attrs: {
          meta.priority = 3;
        }))
        wget
        xorg.xset
        zip
        zsh
        zsh-autosuggestions
        zsh-syntax-highlighting
      ];
    };
  };
}
