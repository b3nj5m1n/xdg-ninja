#!/bin/bash

USE_GLOW=true
if ! command -v glow &> /dev/null
then
    echo "Glow not found, markdown rendering not available."
    USE_GLOW=false
fi

unalias -a

HELPSTRING="""\


    \e[37;45;1mxdg-ninja\e[0m

    \e[1;3mCheck your \$HOME for unwanted files.\e[1;0m

    ────────────────────────────────────

    \e[3m--help\e[0m              \e[1mThis help menu\e[0m
    \e[3m-h\e[0m

    \e[3m--no-skip-ok\e[0m        \e[1mDisplay messages for all files checked (verbose)\e[0m
    \e[3m-v\e[0m

    \e[3m--skip-ok\e[0m           \e[1mDon't display anything for files that do not exist (default)\e[0m

"""

SKIP_OK=true
for i in "$@" ; do
    if [[ $i == "--help" ]] || [[ $i == "-h" ]] ; then
        echo -e "$HELPSTRING"
        exit
    elif [[ $i == "--skip-ok" ]] ; then
        SKIP_OK=true
    elif [[ $i == "--no-skip-ok" ]] ; then
        SKIP_OK=false
    elif [[ $i == "-v" ]] ; then
        SKIP_OK=false
    fi
done

ERR=0
WARN=1
INFO=2
SUCS=3
HELP=4

# Function to expand enviornment variables in string
# https://stackoverflow.com/a/20316582/11110290
apply_shell_expansion() {
    declare data="$1"
    declare delimiter="__apply_shell_expansion_delimiter__"
    declare command="cat <<$delimiter"$'\n'"$data"$'\n'"$delimiter"
    eval "$command"
}

# Returns 0 if the path doesn't lead anywhere
# Return 1 if the path points to a file, 2 if it points to a directory
check_not_exists_file() {
    FILE_PATH=$(apply_shell_expansion "$1")
    if [ -f "$FILE_PATH" ]; then
        return 1
    elif [ -d "$FILE_PATH" ]; then
        return 2
    else
        return 0
    fi
}


# Function to handle the formatting of output
log() {
    MODE="$1"
    NAME="$2"
    FILENAME="$3"
    HELP="$4"

    case "$MODE" in

        ERR)
            printf "[\e[1;31m$NAME\e[1;0m]: \e[1;3m$FILENAME\e[1;0m\n"
            ;;

        WARN)
            printf "[\e[1;33m$NAME\e[1;0m]: \e[1;3m$FILENAME\e[1;0m\n"
            ;;

        INFO)
            printf "[\e[1;36m$NAME\e[1;0m]: \e[1;3m$FILENAME\e[1;0m\n"
            ;;

        SUCS)
            if [ "$SKIP_OK" = false ]; then
                printf "[\e[1;32m$NAME\e[1;0m]: \e[1;3m$FILENAME\e[1;0m\n"
            fi
            ;;

        HELP)
            if $USE_GLOW; then
                echo "$HELP" | glow -
            else
                echo "$HELP"
            fi
            ;;

    esac
}

# Checks that the given file does not exist, otherwise outputs help
check_file() {
    INPUT="$1"
    NAME="$2"

    FILENAME=$(echo -E "$INPUT" | jq -r .path)
    MOVABLE=$(echo -E "$INPUT" | jq -r .movable)
    HELP=$(echo -E "$INPUT" | jq -r .help)

    check_not_exists_file "$FILENAME"

    case $? in

        0)
            log SUCS "$NAME" "$FILENAME" "$HELP"
            ;;

        1 | 2)
            if "$MOVABLE"; then
                log ERR "$NAME" "$FILENAME" "$HELP"
            else
                log WARN "$NAME" "$FILENAME" "$HELP"
            fi
            if ! [ -z "$HELP" ]; then
                log HELP "$NAME" "$FILENAME" "$HELP"
            else
                log HELP "$NAME" "$FILENAME" "_No help available._"
            fi
            ;;

    esac
}

# Reads a file from programs/, calls check_file on each file specified for the program
check_program() {
    INPUT=$1

    NAME=$(echo "$INPUT" | jq -r .name)
    

    while IFS= read -r file; do
        check_file "$file" "$NAME"
    done <<< "$(echo "$INPUT" | jq -rc '.files[]')"
}

# Loops over all files in the programs/ directory and calls check_program
enumerate_programs() {
    echo -e "\e[1;3mStarting to check your \e[1;36m\$HOME.\e[1;0m"
    echo -e ""
    for prog_filename in ./programs/*; do
        check_program "$(cat $prog_filename)"
    done
    echo -e "\e[1;3mDone checking your \e[1;36m\$HOME.\e[1;0m"
    echo -e ""
    echo -e "\e[3mIf you have files in your \e[1;36m\$HOME\e[1;0m that shouldn't be there, but weren't recognised by xdg-ninja, please consider creating a configuration file for it and opening a pull request on github.\e[1;0m"
    echo -e ""
}

enumerate_programs
