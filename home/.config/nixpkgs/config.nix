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
        bridge-utils
        curl
        dnsmasq
        docker
        docker-compose
        file
        fuse
        fzf
        git
        gnused
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
        nodejs-11_x
        openssh
        qemu
        rclone
        readline70
        (ruby_2_6.overrideAttrs (attrs: {
          meta.priority = 3;
        }))
        socat
        tig
        tmux
        tree
        (vimHugeX.overrideAttrs (attrs: {
          meta.priority = 3;
        }))
        wget
        xorg.xset
        zsh
        zsh-autosuggestions
        zsh-syntax-highlighting
      ];
    };
  };
}
