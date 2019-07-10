#!/bin/sh

USAGE_zsh=''
HELP_zsh='
  Open a ZSH shell
'
cmd_zsh() {
  SHELL=/bin/bash /home/chronos/user/.nix-profile/bin/zsh -l
}

USAGE_crouton=''
HELP_crouton='
  Open a ZSH shell inside Crouton
'
cmd_crouton() {
  SHELL=/bin/bash sudo /usr/local/bin/enter-chroot /bin/zsh -l
}

USAGE_crouton_bash=''
HELP_crouton_bash='
  Open a Bash shell inside Crouton
'
cmd_crouton_bash() {
  SHELL=/bin/bash sudo /usr/local/bin/enter-chroot
}
