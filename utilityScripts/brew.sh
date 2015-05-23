# Install command-line tools using Homebrew

# Make sure we’re using the latest Homebrew
brew update

# Upgrade any already-installed formulae
brew upgrade --all

# Install GNU core utilities (those that come with OS X are outdated)
brew install coreutils
# Install some other useful utilities like `sponge`
#brew install moreutils
# Install GNU `find`, `locate`, `updatedb`, and `xargs`, `g`-prefixed
#brew install findutils
# Install GNU `sed`, overwriting the built-in `sed`
brew install gnu-sed --default-names


# Install more recent versions of some OS X tools
brew tap neovim/neovim
brew install --HEAD neovim
brew tap homebrew/dupes
brew install grep --with-default-names
# brew install vim --override-system-vi --with-lua
brew install tmux --with-nvim-tui-color
brew install mysql
brew install git
brew install boost
brew install glm
brew install sfml
brew install sdl2
brew install sdl2_mixer
brew install sdl2_ttf
brew install cheat
brew install cowsay
brew install curlpp
brew install figlet
brew install imagemagick
brew install lua
brew install mongodb
brew install mysql-connector-c++
brew install node
brew install python
brew install qt
brew install ag
brew install tree
brew install youtube-dl
brew install zlib
brew install webkit2png

# setup for tmux connecting to OSX pastebin
brew install reattach-to-user-namespace

# Remove outdated versions from the cellar
brew cleanup
