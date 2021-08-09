# You may need to manually set your language environment
export LANG=en_US.UTF-8

if [ -z $TMUX ]; then;
   tmux
fi

export GPG_TTY=$(tty)

export ZSH_DISABLE_COMPFIX=true
export ZPLUG_HOME=/usr/local/opt/zplug
[[ -s "$ZPLUG_HOME/init.zsh" ]] && source $ZPLUG_HOME/init.zsh

# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="dvbeato"

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

#TERM=xterm
# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
#COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  asdf
  aws
  docker
  docker-compose
  git
  git-extras
  github
  gitignore
  kubectl
  lein
  mvn
  tmux
  z
#  zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh
[[ -s "$HOME/.z.sh" ]] && source $HOME/.z.sh
[[ -s "$HOME/.env" ]] && source "$HOME/.env"
# User configuration


# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
#export ARCHFLAGS="-arch x86_64"
export EDITOR='vim'
export VISUAL=vim

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Mac only
if [[ "$OSTYPE" == "darwin"* ]]; then
  eval $(/opt/homebrew/bin/brew shellenv)

  alias emacs='open -a Emacs'
  alias idea='open -a "IntelliJ IDEA CE"'

  # Android Setup
  export ANDROID_HOME="$HOME/Library/Android/sdk"
  export PATH=$ANDROID_HOME/platform-tools:$ANDROID_HOME/emulator:$PATH

  export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk-16.0.1.jdk/Contents/Home
  export PATH="$JAVA_HOME/bin:$PATH"
fi

# LINUX only
if [[ "$OSTYPE" == "linux-gnu" ]]; then
  alias ls='ls --color=auto'
  alias pbcopy='xclip -selection clipboard'
  alias pbpaste='xclip -selection clipboard -o'
  alias open='xdg-open'

 # export JAVA_HOME="/home/linuxbrew/.linuxbrew/bin/java"
  export JAVA_HOME="/usr/lib/jvm/default/"

  export APPS_HOME=$HOME/Applications
  export BREW="/home/linuxbrew/.linuxbrew/bin"
  export PATH="$JAVA_HOME/bin:$BREW:$PATH"
fi

# aliases
# git
alias g='git'
alias ga='g add'
alias gl='g log'
alias gs='g status'
alias gpl='g pull'
alias gps='g push'
alias gc='g commit -am'
alias gck='g checkout'
alias gdf='g diff'
alias gdfn='df --name-only'
alias gunstage='g reset HEAD -- '

#rails
alias rr='rake routes'
alias rdbm='rake db:migrate'
alias rgc='rails generate controller'
alias rgm='rails generate model'
alias rs='rails server'

#lein
alias lr='lein repl'
alias ldp='lein deps'
alias lma='lein midje :autotest'

# utils
alias pong='ping www.google.com'
alias myip="ifconfig | grep 'inet ' | cut -d ' ' -f2 | grep -v '127.0.0.1'"

alias ll='ls -lhap'

if [ -x "$(command -v nvim)" ]; then
  alias vim='nvim'
fi

# functions
function update-dotfiles { git -C $HOME/.dotfiles pull }
function update-vimfiles { git -C $HOME/.vimfiles pull }
function = { echo "$1" | bc }

# NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# zsh-syntax-highlighting
[[ -s "$HOME/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]] && source $HOME/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[[ -s "/usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]] && source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[[ -s "$HOME/.asdf/asdf.sh" ]] && . $HOME/.asdf/asdf.sh
[[ -s "$HOME/.asdf/completations/asdf.bash" ]] && . $HOME/.asdf/completations/asdf.bash

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
