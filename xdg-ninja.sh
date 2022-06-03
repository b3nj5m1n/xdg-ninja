#!/usr/bin/env bash

MDRENDERER="glow -"
PROG=${0##*/}
RESOURCES=${0%%$PROG}

has_command() {
	command -v "$1" >/dev/null || {
		printf '%s\n' "Dependency $1 not met, Please install it."
		exit 1
	}
}

decode_string() {
	data="${1//\\n/$'\n'}"
	data="${data//\\\"/\"}"
	printf "%s\n" "$data"
}

# Function to handle the formatting of output
log() {
    MODE="$1"
    NAME="$2"
    FILENAME="$3"
    HELP="$4"

	print() {
		printf '[\033[1;%sm%s\033[1;0m]: \033[1;3m%s\033[1;0m\n' "$1" "$NAME" "$FILENAME"
	}

    case "$MODE" in
    	ERR) print '31' ;;
    	WARN) print '33' ;;
    	INFO) print '36' ;;
    	HELP) decode_string "$HELP" | $MDRENDERER ;;
    esac
}

check_file() {
    NAME="$1"
    FILENAME="$2"
    MOVABLE="$3"
    HELP="$4"

	logsc() {
		log $1 "$NAME" "$FILENAME" "${HELP:-$2}"
	}

    [ ! -e "$(eval echo $FILENAME)" ] || {
		[ $MOVABLE = true ] && logsc ERR || logsc WARN
		[ "$HELP" ] && logsc HELP || logsc HELP "No help available."
	}
}

xdgprint() {
    printf '\033[0;37m%s \033[1;36m%s \033[0;37m%s\n' "The" "$1" "environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!"
    printf "\033[1;37m%s: \033[1;36m%s \033[0m\n\n" "	The recommended value is" "$2"
}

[ -z "${XDG_DATA_HOME}" ] && xdgprint '$XDG_DATA_HOME' '$HOME/.local/share'
[ -z "${XDG_CONFIG_HOME}" ] && xdgprint '$XDG_CONFIG_HOME' '$HOME/.config'
[ -z "${XDG_STATE_HOME}" ] && xdgprint '$XDG_STATE_HOME' '$HOME/.local/state'
[ -z "${XDG_CACHE_HOME}" ] && xdgprint '$XDG_CACHE_HOME' '\$HOME/.cache'
[ -z "${XDG_RUNTIME_DIR}" ] && xdgprint '$XDG_RUNTIME_DIR' '/run/usr/$UID'

has_command jq
has_command glow

printf "\033[1;3mStarting to check your \033[1;36m\$HOME.\033[1;0m\n\n"

while read -r name; read -r filename; read -r movable; read -r help; do
	check_file "$name" "$filename" "$movable" "$help"
done <<< $(jq 'inputs as $input | $input.files[] as $file | $input.name, $file.path, $file.movable, $file.help' $RESOURCES/programs/*.json | sed -e 's/^"//' -e 's/"$//')

printf "\033[1;3mDone checking your \033[1;36m\$HOME.\033[1;0m\n\n"

printf "\033[3mIf you have files in your \033[1;36m\$HOME\033[1;0m that shouldn't be there, but weren't recognised by xdg-ninja, please consider creating a configuration file for it and opening a pull request on github.\033[1;0m\n\n"
