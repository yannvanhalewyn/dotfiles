# Setting up a new Mac

## Clone this repo

``` sh
git clone https://github.com/yannvanhalewyn/dotfiles.git ~/dotfiles
```

## Install apps and packages

Visit https://brew.sh for installing homebrew. Then run:

``` sh
brew bundle --file=~/dotfiles/Brewfile
```

## Configure Terminal

With either iTerm or terminal, upload the colorscheme profiles found in the
iTerm directory. Then change the font to Liberation Mono For Powerline
(installed via Brew Cask in the previous step).

Run the FZF install script to get fuzzy finding in the terminal and keybindings.
It should be:

``` sh
# $(brew --prefix)/opt/fzf/install
```

But check https://github.com/junegunn/fzf for any changes.

## Run MacOS provisioning

This will setup

``` sh
cd ~/dotfiles/ansible && ./runner.sh
```

## Troubleshooting

***Hammerspoon won't work properly**

Verify that hammerspoon has been given accessibility rights somewhere in System Preferences > Security & Privacy > Accessibility

**bin/doom or emacs binary not working*

> dyld: Library not loaded: /usr/local/opt/libffi/lib/libffi.7.dylib

Was fixed by reinstalling emacs using `brew reinstall emacs-plus`. Could also be
fixed by installing libffi explicitely using `brew install libffi`
