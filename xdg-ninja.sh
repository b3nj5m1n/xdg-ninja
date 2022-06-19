#!/usr/bin/env sh

has_command() {
    command -v "$1" >/dev/null 2>/dev/null
    return $?
}

unalias -a

init_constants() {
    FX_RESET="\033[0m"
    FX_BOLD="\033[1m"
    FX_ITALIC="\033[3m"

    FG_RED="\033[31m"
    FG_GREEN="\033[32m"
    FG_YELLOW="\033[33m"
    FG_CYAN="\033[36m"
    FG_WHITE="\033[37m"

    BG_MAGENTA="\033[45m"
}
init_constants

help() {
    init_constants
    HELPSTRING="""\


    ${FG_WHITE}${BG_MAGENTA}${FX_BOLD}xdg-ninja${FX_RESET}

    ${FX_BOLD}${FX_ITALIC}Check your \$HOME for unwanted files.${FX_RESET}


    ${FX_ITALIC}--help${FX_RESET}              ${FX_BOLD}This help menu${FX_RESET}
    ${FX_ITALIC}-h\033${FX_RESET}

    ${FX_ITALIC}--skip-ok${FX_RESET}           ${FX_BOLD}Don't display anything for files that do not exist (default)${FX_RESET}
    ${FX_ITALIC}--no-skip-ok${FX_RESET}        ${FX_BOLD}Display messages for all files checked${FX_RESET}

    ${FX_ITALIC}--skip-warn${FX_RESET}         ${FX_BOLD}Don't display anything for files that cannot be fixed${FX_RESET}
    ${FX_ITALIC}--no-skip-warn${FX_RESET}      ${FX_BOLD}Display help for files that cannot be fixed (default)${FX_RESET}

    ${FX_ITALIC}--color=WHEN${FX_RESET}        ${FX_BOLD}Color the output always, never, or auto (default)${FX_RESET}

    ${FX_ITALIC}--decoder=X${FX_RESET}         ${FX_BOLD}Manually set the decoder used for markdown${FX_RESET}

    ${FX_ITALIC}--output-style=X${FX_RESET}    ${FX_BOLD}Style of output; normal (default) or json${FX_RESET}

    ${FX_ITALIC}--quiet${FX_RESET}             ${FX_BOLD}Run script quietly${FX_RESET}
    ${FX_ITALIC}-q${FX_RESET}

    ${FX_ITALIC}--verbose${FX_RESET}           ${FX_BOLD}Alias for --no-skip-warn and --no-skip-ok${FX_RESET}
    ${FX_ITALIC}-v${FX_RESET}


    """
    printf "%b" "$HELPSTRING"
}

SKIP_OK=true
SKIP_WARN=false
COLOR=auto
OUTPUT_STYLE=normal
EXIT_STATUS=0
HELP=false
QUIET=false
for i in "$@"; do
    case $i in
        --color=*)
            COLOR="${i#*=}"
            ;;
        --help|-h)
            HELP=true
            ;;
        --skip-ok)
            SKIP_OK=true
            ;;
        --skip-warn)
            SKIP_WARN=true
            ;;
        --no-skip-warn)
            SKIP_WARN=false
            ;;
        --no-skip-ok)
            SKIP_OK=false
            ;;
        --verbose|-v)
            SKIP_OK=false
            SKIP_WARN=false
            ;;
        --decoder=*)
            DECODER="${i#*=}"
            ;;
        --output-style=*)
            OUTPUT_STYLE="${i#*=}"
            ;;
        --quiet|-q)
            QUIET=true
            ;;
        --loud)
            QUIET=false
            ;;
    esac
done

auto_set_decoder() {
    DECODER="cat"
    if has_command glow; then
        DECODER="glow -"
    else
        if has_command bat; then
            DECODER="bat -pp --decorations=always --color=always --language markdown"
            if [ "$QUIET" != false ]; then
                printf "Markdown rendering will be done by bat. (Glow is recommended)\n"
            fi
        elif has_command pygmentize; then
            DECODER="pygmentize -l markdown"
            if [ "$QUIET" != false ]; then
                printf "Markdown rendering will be done by pygmentize. (Glow is recommended)\n"
            fi
        elif has_command highlight; then
            DECODER="highlight --out-format ansi --syntax markdown"
            if [ "$QUIET" != false ]; then
                printf "Markdown rendering will be done by highlight. (Glow is recommended)\n"
            fi
        else
            if [ "$QUIET" != false ]; then
                printf "Markdown rendering not available. (Glow is recommended)\n"
                printf "Output will be raw markdown and might look weird.\n"
            fi
        fi
        if [ "$QUIET" != false ]; then
            printf "Install glow for easier reading & copy-paste.\n"
        fi
    fi
}

auto_set_decoder
set_colors
[ $HELP = "true" ] && help

if [ -z "${XDG_DATA_HOME}" -a "$QUIET" != false ]; then
    printf '%b%s%b\n' "${FX_BOLD}${FG_CYAN}" "The \$XDG_DATA_HOME environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!" "${FX_RESET}"
    printf "%b    ⤷ The recommended value is: %b\$HOME/.local/share%b\n" "${FX_BOLD}${FG_CYAN}" "${FX_BOLD}${FX_ITALIC}" "${FX_RESET}"
fi
if [ -z "${XDG_CONFIG_HOME}" -a "$QUIET" != false ]; then
    printf '%b%s%b\n' "${FX_BOLD}${FG_CYAN}" "The \$XDG_CONFIG_HOME environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!" "${FX_RESET}"
    printf "%b    ⤷ The recommended value is: %b\$HOME/.config%b\n" "${FX_BOLD}${FG_CYAN}" "${FX_BOLD}${FX_ITALIC}" "${FX_RESET}"
fi
if [ -z "${XDG_STATE_HOME}" -a "$QUIET" != false ]; then
    printf '%b%s%b\n' "${FX_BOLD}${FG_CYAN}" "The \$XDG_STATE_HOME environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!" "${FX_RESET}"
    printf "%b    ⤷ The recommended value is: %b\$HOME/.local/state%b\n" "${FX_BOLD}${FG_CYAN}" "${FX_BOLD}${FX_ITALIC}" "${FX_RESET}"
fi
if [ -z "${XDG_CACHE_HOME}" -a "$QUIET" != false ]; then
    printf '%b%s%b\n' "${FX_BOLD}${FG_CYAN}" "The \$XDG_CACHE_HOME environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!" "${FX_RESET}"
    printf "%b    ⤷ The recommended value is: %b\$HOME/.cache%b\n" "${FX_BOLD}${FG_CYAN}" "${FX_BOLD}${FX_ITALIC}" "${FX_RESET}"
fi
if [ -z "${XDG_RUNTIME_DIR}" -a "$QUIET" != false ]; then
    printf '%b%s%b\n' "${FX_BOLD}${FG_CYAN}" "The \$XDG_RUNTIME_DIR environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!" "${FX_RESET}"
    printf "%b    ⤷ The recommended value is: %b/run/user/\$UID%b\n" "${FX_BOLD}${FG_CYAN}" "${FX_BOLD}${FX_ITALIC}" "${FX_RESET}"
fi

if ! command -v jq >/dev/null 2>/dev/null; then
    printf "jq is needed to run this script, but it wasn't found. Please install it to be able to use this script.\n"
    exit
fi

if [ "$QUIET" != false ]; then
    printf "\n"
fi

# Function to expand environment variables in string
# https://stackoverflow.com/a/20316582/11110290
apply_shell_expansion() {
    data="$1"
    delimiter="__apply_shell_expansion_delimiter__"
    command=$(printf "cat <<%s\n%s\n%s" "$delimiter" "$data" "$delimiter")
    eval "$command"
}

# Returns 0 if the path doesn't lead anywhere
# Returns 1 if the path leads to something
check_if_file_exists() {
    FILE_PATH=$(apply_shell_expansion "$1")
    if [ -e "$FILE_PATH" ]; then
        return 1
    else
        return 0
    fi
}

decode_string() {
    printf "%s" "$1" | sed -e 's/\\n/\
/g' -e 's/\\\"/\"/g' -e '$ s/\n*$/\
\
/' # Replace \n with literal newline and \" with ", normalize number of trailing newlines to 2
}

# Function to handle the formatting of output
log() {
    MODE="$1"
    NAME="$2"
    FILENAME="$3"
    HELP="$4"

    case "$MODE" in

    ERR)
        printf '[%b%s%b]: %b%s%b\n' "${FX_BOLD}${FG_RED}" "$NAME" "${FX_RESET}" "${FX_BOLD}${FX_ITALIC}" "$FILENAME" "${FX_RESET}"
        ;;

    WARN)
        printf '[%b%s%b]: %b%s%b\n' "${FX_BOLD}${FG_YELLOW}" "$NAME" "${FX_RESET}" "${FX_BOLD}${FX_ITALIC}" "$FILENAME" "${FX_RESET}"
        ;;

    INFO)
        printf '[%b%s%b]: %b%s%b\n' "${FX_BOLD}${FG_CYAN}" "$NAME" "${FX_RESET}" "${FX_BOLD}${FX_ITALIC}" "$FILENAME" "${FX_RESET}"
        ;;

    SUCS)
        [ "$SKIP_OK" = false ] &&
            printf '[%b%s%b]: %b%s%b\n' "${FX_BOLD}${FG_GREEN}" "$NAME" "${FX_RESET}" "${FX_BOLD}${FX_ITALIC}" "$FILENAME" "${FX_RESET}"
        ;;

    HELP)
        decode_string "$HELP" | $DECODER
        ;;

    esac
}

# Checks that the given file does not exist, otherwise outputs help
check_file() {
    NAME="$1"
    FILENAME="$2"
    MOVABLE="$3"
    HELP="$4"
    JSON_FILE="$5"

    check_if_file_exists "$FILENAME"

    case $? in

    0)
        [ "$SKIP_OK" = false ] && [ "$OUTPUT_STYLE" = "json" ] && cat "$JSON_FILE" && return
        log SUCS "$NAME" "$FILENAME" "$HELP"
        ;;

    1)
        if [ "$MOVABLE" = true ]; then
            EXIT_STATUS=$(expr $EXIT_STATUS + 1)
            [ "$OUTPUT_STYLE" = "json" ] && cat "$JSON_FILE" > "$XDG_RUNTIME_DIR"/xdg-ninja"$NAME".json && return
            log ERR "$NAME" "$FILENAME" "$HELP"
        else
            [ "$SKIP_WARN" = true ] && return
            [ "$OUTPUT_STYLE" = "json" ] && cat "$JSON_FILE" > "$XDG_RUNTIME_DIR"/xdg-ninja/"$NAME".json && return
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

# Reads files from programs/, calls check_file on each file specified for each program
do_check_programs() {
    [ "$OUTPUT_STYLE" = "json" ] && mkdir "$XDG_RUNTIME_DIR"/xdg-ninja/
    while IFS="
" read -r name; read -r filename; read -r movable; read -r help; read -r json_file;  do
        check_file "$name" "$filename" "$movable" "$help" "$json_file"
    done <<EOF
$(jq 'inputs as $input | $input.files[] as $file | $input.name, $file.path, $file.movable, $file.help, input_filename' "$(dirname "$0")"/programs/* | sed -e 's/^"//' -e 's/"$//')
EOF
    [ "$OUTPUT_STYLE" = "json" ] && jq $JQ_COLOR_VAR -s . "$XDG_RUNTIME_DIR"/xdg-ninja/* && rm -rf "$XDG_RUNTIME_DIR/xdg-ninja"
# sed is to trim quotes
}

check_programs() {
    if [ "$QUIET" != false ]; then
        printf "%bStarting to check your %b\$HOME%b.\n" "${FX_BOLD}${FX_ITALIC}" "${FG_CYAN}" "${FX_RESET}"
        printf "\n"
    fi
    do_check_programs
    if [ "$QUIET" != false ]; then
        printf "%bDone checking your %b\$HOME.%b\n" "${FX_BOLD}${FX_ITALIC}" "${FG_CYAN}" "${FX_RESET}"
        printf "\n"
        printf "%bIf you have files in your %b\$HOME%b that shouldn't be there, but weren't recognised by xdg-ninja, please consider creating a configuration file for it and opening a pull request on github.%b\n" "${FX_ITALIC}" "${FG_CYAN}" "${FX_RESET}${FX_ITALIC}" "${FX_RESET}"
        printf "\n"
    fi
}


check_programs
exit $EXIT_STATUS
