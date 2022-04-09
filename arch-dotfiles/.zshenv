#export PATH=$PATH:$(ruby -e 'print Gem.user_dir')/bin:$HOME/bin:/Library/TeX/texbin/:/opt/clojurescript/bin:$HOME/.cargo/bin:/usr/lib/emscripten
export PATH=$PATH:$HOME/bin:/Library/TeX/texbin/:/opt/clojurescript/bin:$HOME/.cargo/bin:/usr/lib/emscripten
export EDITOR=emacsclient
export CEF_DIR=/opt/cef-minimal/
PS1='%n%m %~ %(?..[%?] )%# '
. "$HOME/.cargo/env"
