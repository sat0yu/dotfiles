#!/bin/bash
# install pyenv
git clone git://github.com/yyuu/pyenv.git ~/.pyenv
# install 2 and 3
pyenv install 3.8.2
pyenv install 2.7.17
# install virtualenv and crearte virtual environments
pyenv global 3.8.2
virtualenv -p python3 ~/nvim-python3
pyenv global 2.7.17
virtualenv -p python ~/nvim-python2
#install requirement
pyenv global system
source ~/nvim-python3/bin/activate
pip install -r ~/dotfiles/config/nvim/nvim-python3-requirements.txt
deactivate
source ~/nvim-python2/bin/activate
pip install -r ~/dotfiles/config/nvim/nvim-python2-requirements.txt
deactivate
