alias ack='ack-grep'
alias cls='printf "\033c"'
alias ccat='pygmentize -g -O style=monokai -f console256 -g'
alias tmux='TERM=screen-256color-bce tmux'
alias git='gh'
alias vim='nvim'

# Apt
alias apt-install="sudo apt-get install"
alias apt-update="sudo apt-get update"
alias apt-ppa="sudo add-apt-repository"
alias apt-search="apt-cache search"
alias apt-show="apt-cache show"
alias apt-remove="sudo apt-get remove"
alias apt-purge="sudo apt-get purge"
alias apt-upgrade="sudo apt-get upgrade"
alias apt-dist-upgrade="sudo apt-get dist-upgrade"

# Docker
alias fig="echo 'Use docker-compose'"
alias dup="docker-compose up -d"
alias drun="docker-compose run"
alias druns="docker-compose run --service-ports"
alias dremove='docker rm -f $(docker ps -aq)'
alias dps="docker-compose ps"
alias dlogs="docker-compose logs"

#ssh
alias s="ssh"
