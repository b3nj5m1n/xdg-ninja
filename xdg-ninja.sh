#!/usr/bin/env sh

# placeholder for PR/77#issuecomment-1146584127
unalias -a

has_command() {
    command -v "$1" >/dev/null 2>/dev/null
}

# function to display message for a missing XDG variable, taking arguments as what is missing, and what should be there.
xdgprint() {
    printf '\033[0;37m%s \033[1;36m%s \033[0;37m%s\n' "The" "$1" "environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!"
    printf "\033[1;37m%s: \033[1;36m%s \033[0m\n\n" "	The recommended value is" "$2"
}

# Function to expand environment variables in string
# https://stackoverflow.com/a/20316582/11110290
apply_shell_expansion() {
    data="$1"
    delimiter="__apply_shell_expansion_delimiter__"
    command=$(printf "cat <<%s\n%s\n%s" "$delimiter" "$data" "$delimiter")
    eval "$command"
}


check_if_file_exists() {
    FILE_PATH=$(apply_shell_expansion "$1")
	# check if the file does NOT exist, return 1 if it does.
	[ ! -e "$FILE_PATH" ]
}

# function to handle markdown characters for $HELP
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

	# takes only the color argument, as the parameters are already set.
	print() {
		printf '[\033[1;%sm%s\033[1;0m]: \033[1;3m%s\033[1;0m\n' "$1" "$NAME" "$FILENAME"
	}

    case "$MODE" in
		ERR) print '31' ;;
		WARN) print '33' ;;
		INFO) print '36' ;;
		SUCS) [ "$SKIP_OK" = false ] && print '32' ;;
		HELP)
			case $MDRENDERER in
				glow) decode_string "$HELP" | glow - ;;
				bat) decode_string "$HELP" | bat -pp --decorations=always --color=always --language markdown ;;
				pygmentize) decode_string "$HELP" | pygmentize -l markdown ;;
				hightlight) decode_string "$HELP" | highlight --out-format ansi --syntax markdown ;;
				*)
					printf "\033[1;31m%s. %s\n" "Markdown rendering not available," "Output will be raw markdown."
					decode_string "$HELP"
				;;
			esac
        ;;
    esac
}

# Checks that the given file does not exist, prints appropiate message for that file.
check_file() {
    NAME="$1"
    FILENAME="$2"
    MOVABLE="$3"
    HELP="$4"

	logsc() {
		log "$1" "$NAME" "$FILENAME" "${HELP:-$2}" # use 2nd argument if it exists, otherwise default $HELP
	}

	check_if_file_exists "$FILENAME" || {
		# if file exists, check if it is movable, error if it is, otherwise warn
		[ "$MOVABLE" = true ] && logsc ERR || logsc WARN
		# if help exists, output it, otherwise display that it is not available.
		[ "$HELP" ] && logsc HELP || logsc HELP "No help available."
	} && logsc SUCS # if previous hadn't already matched, display success for $SKIP_OK
}

# Reads files from programs/, calls check_file on each file specified for each program
check_programs() {
    while IFS="
" read -r name; read -r filename; read -r movable; read -r help; do
        check_file "$name" "$filename" "$movable" "$help"
    done <<EOF
"$(jq 'inputs as $input | $input.files[] as $file | $input.name, $file.path, $file.movable, $file.help' "$(dirname "$0")"/programs/* | sed -e 's/^"//' -e 's/"$//')"
EOF
# sed is to trim quotes
}

# variables
SKIP_OK=true
HELPSTRING="""\

    \033[37;45;1mxdg-ninja\033[0m

    \033[1;3mCheck your \$HOME for unwanted files.\033[1;0m

    ────────────────────────────────────

    \033[3m--help\033[0m              \033[1mThis help menu\033[0m
    \033[3m-h\033[0m

    \033[3m--no-skip-ok\033[0m        \033[1mDisplay messages for all files checked (verbose)\033[0m
    \033[3m-v\033[0m

    \033[3m--skip-ok\033[0m           \033[1mDon't display anything for files that do not exist (default)\033[0m
    \033[3m-s\033[0m

"""

main() {
	case "$1" in
		-h|--help) printf "%b" "$HELPSTRING" && return 0 ;;
		-s|--skip-ok) SKIP_OK=true ;;
		-v|--no-skip-ok|--verbose) SKIP_OK=false ;;
	esac
	
	# check for xdg varaibles
	printf "\n"
	[ -z "${XDG_DATA_HOME}" ] && xdgprint "\$XDG_DATA_HOME" "\$HOME/.local/share"
	[ -z "${XDG_CONFIG_HOME}" ] && xdgprint "\$XDG_CONFIG_HOME" "\$HOME/.config"
	[ -z "${XDG_STATE_HOME}" ] && xdgprint "\$XDG_STATE_HOME" "\$HOME/.local/state"
	[ -z "${XDG_CACHE_HOME}" ] && xdgprint "\$XDG_CACHE_HOME" "\$HOME/.cache"
	[ -z "${XDG_RUNTIME_DIR}" ] && xdgprint "\$XDG_RUNTIME_DIR" "/run/user/\$UID"
	
	# check if jq exists, if not, display message and exit
	has_command jq || {
		printf "\033[1;31m%s\033[0m\n\n" \
		   "jq is needed to run this script, but it wasn't found. Please install it to be able to use this script."
		exit 1
	}

	# loop for all available markdown renderers, stop looping when matched
	for rend in glow bat pygmentize highlight; do
		has_command "$rend" && MDRENDERER="$rend" && break
	done

    printf "\033[1m%s \033[1;36m%s.\033[0m\n\n" "Starting to check your" "\$HOME"
    check_programs
    printf "\033[1;39m%s \033[1;36m%s.\033[1;0m\n" "Done checking your" "\$HOME"
    printf "\033[3m%s \033[1;36m%s \033[1;0m%s\033[0m\n\n" \
		"If you have files in your" "\$HOME" \
		"that shouldn't be there, but weren't recognised by xdg-ninja, please consider creating a configuration file for it and opening a pull request on github."
}

main "$@"
