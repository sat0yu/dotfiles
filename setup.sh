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
    'zsh' 'wget' 'ctags' 'vim' 'lv' 'tmux' 'tig' 'gnu-sed' \
    'ack' 'colordiff' 'nkf' 'valgrind' 'z' 'mosh' 'mp3gain' \
    'subversion --unicode-path' \
    'weechat --with-ruby --with-python --with-perl' \
)
#brew install ${formulas[@]}


# ------------------------------
# tex
# ------------------------------
tex=('ghostscript' 'imagemagick')
#brew install ${tex[@]}


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
