# /etc/skel/.bashrc
if [[ $- != *i* ]] ; then
  # Shell is non-interactive.  Be done now!
  return
fi

export EDITOR=vim
export PAGER=less
export PATH=$PATH:/sbin
export PATH=$PATH:/opt/VirtualBox

if [ -f ~/.nix-profile/etc/bash_completion.d/git-completion.bash ]; then
  source ~/.nix-profile/etc/bash_completion.d/git-completion.bash
fi
