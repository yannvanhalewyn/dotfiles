#!/bin/bash

# Download and install Command Line Tools
if [[ ! -x /usr/bin/gcc ]]; then
  echo "Install xcode"
  xcode-select --install
fi

# Download and install Homebrew
if [[ ! -x /usr/local/bin/brew ]]; then
  echo "Install homebrew"
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

export PATH=/usr/local/bin:$PATH

if ! command -v python &> /dev/null; then
  echo "Install python"
  brew install python
fi

if [[ ! -x /usr/local/bin/ansible ]]; then
  echo "Install ansible"
  brew install ansible
fi

ansible-galaxy collection install community.general
ansible-playbook -v -i hosts --ask-become-pass main.yml 
