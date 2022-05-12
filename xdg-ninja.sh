#!/bin/bash

USE_GLOW=true
if ! command -v glow &> /dev/null
then
    echo "Glow not found, markdown rendering not available."
    USE_GLOW=false
fi

unalias -a

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
            printf "[\e[1;32m$NAME\e[1;0m]: \e[1;3m$FILENAME\e[1;0m\n"
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
    for prog_filename in ./programs/*; do
        check_program "$(cat $prog_filename)"
    done
}

enumerate_programs
