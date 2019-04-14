{
  packageOverrides = pkgs: with pkgs; {
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        ag
        augeas
        bridge-utils
        curl
        dnsmasq
        file
        git
        gnused
        gptfdisk
        homesick
        htop
        inetutils
        less
        lxc
        lxd
        man-pages
        man
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
        vim
        wget
        xorg.xset
        zsh
      ];
    };
  };
}
