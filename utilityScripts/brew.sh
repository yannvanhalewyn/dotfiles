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


# Install Bash 4
# Note: don’t forget to add `/usr/local/bin/bash` to `/etc/shells` before running `chsh`.
#brew install bash
# regular bash-completion package is held back to an older release, so we get latest from versions.
#   github.com/Homebrew/homebrew/blob/master/Library/Formula/bash-completion.rb#L3-L4
#brew tap homebrew/versions
#brew install homebrew/versions/bash-completion2

# Install more recent versions of some OS X tools
brew install vim --override-system-vi
brew install tmux
brew install mysql

# setup for tmux connecting to OSX pastebin
brew install reattach-to-user-namespace

# Install other useful binaries
brew install git

# Remove outdated versions from the cellar
brew cleanup
