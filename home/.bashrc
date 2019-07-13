# /etc/skel/.bashrc
if [[ $- != *i* ]] ; then
  # Shell is non-interactive.  Be done now!
  return
fi

shopt -s histappend

export EDITOR=vim
export PATH=$PATH:/sbin
export PATH=$PATH:/opt/VirtualBox
export PATH=$PATH:/usr/local/sbin
export PATH=$PATH:/usr/local/bin
export LD_LIBRARY_PATH=/usr/local/lib64:/usr/local/lib:/lib64:/lib
export LOCALE_ARCHIVE=/usr/lib64/locale/locale-archive

export PAGER=more
if hash less 2> /dev/null; then
  export PAGER=less
fi
