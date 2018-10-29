# /etc/skel/.bashrc
if [[ $- != *i* ]] ; then
  # Shell is non-interactive.  Be done now!
  return
fi

export DISPLAY=:0
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib
export PAGER=/usr/local/bin/less
export PATH=/usr/local/sbin:$PATH
export PATH=$PATH:/sbin
export PATH=$HOME/.nodebrew/current/bin:$PATH
# git completion
if [ -f /usr/local/share/git-completion/git-completion.bash ]; then
  source /usr/local/share/git-completion/git-completion.bash
fi
