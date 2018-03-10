if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

export PATH="$(ruby -e 'print Gem.user_dir')/bin:$HOME/.cargo/bin:$PATH"
