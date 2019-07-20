HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.history

autoload edit-command-line; zle -N edit-command-line
bindkey -M vicmd v edit-command-line

setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS

setopt NO_BEEP
setopt EXTENDED_GLOB
setopt CORRECT

[ -x $(command -v "git") ] && alias g="git"
[ -x $(command -v "nvim") ] && alias vim="nvim"
[ -x $(command -v "youtube-dl") ] && alias yt-dl-mp3='youtube-dl -x --audio-format mp3'
[ -x $(command -v "pyenv") ] && eval "$(pyenv init -)"

[ -r ~/.zshrc-os ] && source ~/.zshrc-os
[ -r ~/.zshrc-machine ] && source ~/.zshrc-machine

# run fzf's install to generate .fzf.zsh. You can skip updating this file.
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# TODO: add optional STW_CFG_DIR
[ -f ~/stow_config/submodules/z/z.sh ] && source ~/stow_config/submodules/z/z.sh

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# prompt ops
# TODO: assuming this file always exists... ok?
source ~/stow_config/submodules/zsh-git-prompt/zshrc.sh
PS1='%F{red}%(?..%? )%F{green}%n@%m %# %f'
RPS1='%F{cyan}%(5~|%-1~/../%3~|%~)%f$(git_super_status)'

# # Do menu-driven completion.
# zstyle ':completion:*' menu select
#
# # Color completion for some things.
# # http://linuxshellaccount.blogspot.com/2008/12/color-completion-using-zsh-modules-on.html
# zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
#
# # formatting and messages
# # http://www.masterzen.fr/2009/04/19/in-love-with-zsh-part-one/
# zstyle ':completion:*' verbose yes
# zstyle ':completion:*:descriptions' format "$fg[yellow]%B--- %d%b"
# zstyle ':completion:*:messages' format '%d'
# zstyle ':completion:*:warnings' format "$fg[red]No matches for:$reset_color %d"
# zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
# zstyle ':completion:*:*:git:*' verbose yes
# zstyle ':completion:*' group-name ''

# The following lines were added by compinstall

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _complete _ignored
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'r:|[._- ]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=0
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/Users/mds/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

[ -f ~/stow_config/submodules/zsh-npm/zsh-better-npm-completion.plugin.zsh ] && \
    source ~/stow_config/submodules/zsh-npm/zsh-better-npm-completion.plugin.zsh


# Sigma stuff
[ -r ~/.sigma/zsh] && source ~/.sigma/zsh
