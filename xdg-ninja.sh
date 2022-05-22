#!/usr/bin/env sh

USE_GLOW=false
USE_BAT=false
if command -v glow >/dev/null 2>/dev/null; then
    USE_GLOW=true
elif command -v bat >/dev/null 2>/dev/null; then
    USE_BAT=true
    printf "Glow not found, markdown rendering will be done by bat.\n"
    printf "Install glow for easier reading & copy-paste.\n"
else
    printf "Glow or bat not found, markdown rendering not available.\n"
    printf "Output will be raw markdown and might look weird.\n"
    printf "Install glow for easier reading & copy-paste.\n"
fi

unalias -a

HELPSTRING="""\


    \033[37;45;1mxdg-ninja\033[0m

    \033[1;3mCheck your \$HOME for unwanted files.\033[1;0m

    ────────────────────────────────────

    \033[3m--help\033[0m              \033[1mThis help menu\033[0m
    \033[3m-h\033[0m

    \033[3m--no-skip-ok\033[0m        \033[1mDisplay messages for all files checked (verbose)\033[0m
    \033[3m-v\033[0m

    \033[3m--skip-ok\033[0m           \033[1mDon't display anything for files that do not exist (default)\033[0m

"""

SKIP_OK=true
for i in "$@"; do
    if [ "$i" = "--help" ] || [ "$i" = "-h" ]; then
        printf "%b" "$HELPSTRING"
        exit
    elif [ "$i" = "--skip-ok" ]; then
        SKIP_OK=true
    elif [ "$i" = "--no-skip-ok" ]; then
        SKIP_OK=false
    elif [ "$i" = "-v" ]; then
        SKIP_OK=false
    fi
done

if [ -z "${XDG_DATA_HOME}" ]; then
    printf '\033[1;36m%s\033[1;0m\n' "The \$XDG_DATA_HOME environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!"
    printf "\033[1;36m    ⤷ \033[1mThe recommended value is: \033[1;3m\$HOME/.local/share\033[1;0m\n"
fi
if [ -z "${XDG_CONFIG_HOME}" ]; then
    printf '\033[1;36m%s\033[1;0m\n' "The \$XDG_CONFIG_HOME environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!"
    printf "\033[1;36m    ⤷ \033[1mThe recommended value is: \033[1;3m\$HOME/.config\033[1;0m\n"
fi
if [ -z "${XDG_STATE_HOME}" ]; then
    printf '\033[1;36m%s\033[1;0m\n' "The \$XDG_STATE_HOME environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!"
    printf "\033[1;36m    ⤷ \033[1mThe recommended value is: \033[1;3m\$HOME/.local/state\033[1;0m\n"
fi
if [ -z "${XDG_CACHE_HOME}" ]; then
    printf '\033[1;36m%s\033[1;0m\n' "The \$XDG_CACHE_HOME environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!"
    printf "\033[1;36m    ⤷ \033[1mThe recommended value is: \033[1;3m\$HOME/.cache\033[1;0m\n"
fi
if [ -z "${XDG_RUNTIME_DIR}" ]; then
    printf '\033[1;36m%s\033[1;0m\n' "The \$XDG_RUNTIME_DIR environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!"
    printf "\033[1;36m    ⤷ \033[1mThe recommended value is: \033[1;3m/run/user/\$UID\033[1;0m\n"
fi

if ! command -v jq >/dev/null 2>/dev/null; then
    printf "jq is needed to run this script, but it wasn't found. Please install it to be able to use this script.\n"
    exit
fi

printf "\n"

# Function to expand environment variables in string
# https://stackoverflow.com/a/20316582/11110290
apply_shell_expansion() {
    data="$1"
    delimiter="__apply_shell_expansion_delimiter__"
    command=$(printf "cat <<%s\n%s\n%s" "$delimiter" "$data" "$delimiter")
    eval "$command"
}

# Returns 0 if the path doesn't lead anywhere
# Return 1 if the path points to a file, 2 if it points to a directory
check_if_file_exists() {
    FILE_PATH=$(apply_shell_expansion "$1")
    if [ -e "$FILE_PATH" ]; then
        return 1
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
        printf '[\033[1;31m%s\033[1;0m]: \033[1;3m%s\033[1;0m\n' "$NAME" "$FILENAME"
        ;;

    WARN)
        printf '[\033[1;33m%s\033[1;0m]: \033[1;3m%s\033[1;0m\n' "$NAME" "$FILENAME"
        ;;

    INFO)
        printf '[\033[1;36m%s\033[1;0m]: \033[1;3m%s\033[1;0m\n' "$NAME" "$FILENAME"
        ;;

    SUCS)
        [ "$SKIP_OK" = false ] &&
            printf '[\033[1;32m%s\033[1;0m]: \033[1;3m%s\033[1;0m\n' "$NAME" "$FILENAME"
        ;;

    HELP)
        if $USE_GLOW; then
            printf "%s\n" "$HELP" | glow -
        elif $USE_BAT; then
            printf "%s\n" "$HELP" | bat -pp --decorations=always --color=always --language markdown
        else
            printf "%s\n" "$HELP"
        fi
        ;;

    esac
}

# Checks that the given file does not exist, otherwise outputs help
check_file() {
    INPUT="$1"
    NAME="$2"

    FILENAME=$(printf "%s" "$INPUT" | jq -r .path)
    MOVABLE=$(printf "%s" "$INPUT" | jq -r .movable)
    HELP=$(printf "%s" "$INPUT" | jq -r .help)

    check_if_file_exists "$FILENAME"

    case $? in

    0)
        log SUCS "$NAME" "$FILENAME" "$HELP"
        ;;

    1)
        if "$MOVABLE"; then
            log ERR "$NAME" "$FILENAME" "$HELP"
        else
            log WARN "$NAME" "$FILENAME" "$HELP"
        fi
        if [ "$HELP" ]; then
            log HELP "$NAME" "$FILENAME" "$HELP"
        else
            log HELP "$NAME" "$FILENAME" "_No help available._"
        fi
        ;;

    esac
}

# Reads a file from programs/, calls check_file on each file specified for the program
check_program() {
    PROGRAM=$1

    NAME=$(jq -r .name "$PROGRAM")

    while IFS= read -r file; do
        check_file "$file" "$NAME"
    done <<EOF
$(jq -rc '.files[]' "$PROGRAM")
EOF
}

# Loops over all files in the programs/ directory and calls check_program
enumerate_programs() {
    printf "\033[1;3mStarting to check your \033[1;36m\$HOME.\033[1;0m\n"
    printf "\n"
    for prog_filename in "$(dirname "$0")"/programs/*; do
        check_program "$prog_filename"
    done
    printf "\033[1;3mDone checking your \033[1;36m\$HOME.\033[1;0m\n"
    printf "\n"
    printf "\033[3mIf you have files in your \033[1;36m\$HOME\033[1;0m that shouldn't be there, but weren't recognised by xdg-ninja, please consider creating a configuration file for it and opening a pull request on github.\033[1;0m\n"
    printf "\n"
}

enumerate_programs
