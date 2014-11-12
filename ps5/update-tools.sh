TOOLS="$HOME/3110-tools/cs3110-cli"
RED='\e[0;31m'
GREEN='\e[0;32m'
NOCOLOR='\e[0m'

green() {
    echo -e "$GREEN$1$NOCOLOR"
}

red() {
    echo -e "$RED$1$NOCOLOR"
}

error() {
    red "ERROR: $1"
    exit 1
}

main() {
    if test ! -d "$TOOLS"; then
        error "The tools directory $TOOLS does not exist."
    fi

    if cd "$TOOLS" && git pull && make; then
        green "Your tools have been successfully updated!"
    else
        error "Something went wrong. You're tools may not have updated!"
    fi
}

main
