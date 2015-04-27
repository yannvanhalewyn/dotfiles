# Install command-line tools using Homebrew

# Make sure we’re using the latest Homebrew
brew update

# Upgrade any already-installed formulae
brew upgrade

# Install GNU core utilities (those that come with OS X are outdated)
# Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.
#brew install coreutils
# Install some other useful utilities like `sponge`
#brew install moreutils
# Install GNU `find`, `locate`, `updatedb`, and `xargs`, `g`-prefixed
#brew install findutils
# Install GNU `sed`, overwriting the built-in `sed`
#brew install gnu-sed --default-names


# Install more recent versions of some OS X tools
brew install vim --override-system-vi
brew install tmux
brew install mysql
brew install git
brew install boost
brew install glm
brew install sfml
brew install sdl2
brew install sdl2_mixer
brew install sdl3_ttf
brew install mysql
brew tap homebrew/versions
brew install homebrew/versions/bash-completion2

# setup for tmux connecting to OSX pastebin
brew install reattach-to-user-namespace

# Remove outdated versions from the cellar
brew cleanup
