# /etc/skel/.bashrc
if [[ $- != *i* ]] ; then
  # Shell is non-interactive.  Be done now!
  return
fi

shopt -s histappend

export EDITOR=vim
export PATH=$PATH:/sbin
export PATH=$PATH:/opt/VirtualBox
export LD_LIBRARY_PATH=

if [ -f ~/.nix-profile/etc/bash_completion.d/git-completion.bash ]; then
  source ~/.nix-profile/etc/bash_completion.d/git-completion.bash
fi

export PAGER=more
if hash less 2> /dev/null; then
  export PAGER=less
fi
