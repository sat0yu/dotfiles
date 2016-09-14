#!/bin/zsh

# ------------------------------
# initialize
# ------------------------------
current_dir=`pwd`

# ------------------------------
# common tools
# ------------------------------
#brew update
#brew upgrade
common=( \
    'zsh' 'wget' 'ctags' 'peco' 'lua' 'lv' 'tmux' 'tig' 'gnu-sed' \
    'ag' 'ack' 'colordiff' 'nkf' 'valgrind' 'z' 'mosh' 'mp3gain' 'jq' \
    'pyenv' 'pyenv-virtualenv' \
    'vim --devel --with-lua' \
    'subversion --unicode-path' \
    'weechat --with-ruby --with-python --with-perl' \
)
#brew install ${formulas[@]}

# ------------------------------
# Unix best practices
# ------------------------------
# avoid misoperations such as 'rm *' at own home directory
touch i
chmod 0 i
mv i ~/-i

# ------------------------------
# tex
# ------------------------------
tex=('ghostscript' 'imagemagick' 'latex-mk')
#brew install ${tex[@]}


# ------------------------------
# pandoc
# ------------------------------
pandoc=('ghc' 'cabal-install')
#brew install ${pandoc[@]}
#cabal update
#cabal install pandoc


# ------------------------------
# topcoder
# ------------------------------
tc_host='http://community.topcoder.com'
jars=(\
    '/contest/classes/TZTester/TZTester.jar' \
    '/contest/classes/CodeProcessor/CodeProcessor.jar' \
    '/contest/classes/FileEdit/FileEdit.jar' \
)

target_dir="${HOME}/topcoder/plugins"
echo ${target_dir}
mkdir -p ${target_dir}
cd ${target_dir}
for i in ${jars}; do
    if [ -e ${i##*/} ]; then
        echo "already exist: ${i##*/}"
    else
        echo "download: ${i##*/} from ${tc_host}"
        curl -O ${tc_host}${i}
    fi
done
cd ${current_dir}
