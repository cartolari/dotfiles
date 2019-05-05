#!/bin/sh

USAGE_zsh=''
HELP_zsh='
  Open a ZSH shell inside the Arch chroot
'
cmd_zsh() {
  SHELL=/bin/bash /home/chronos/user/.nix-profile/bin/zsh -l
}
