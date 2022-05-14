#!/bin/sh

USE_GLOW=true
if ! command -v glow >/dev/null; then
		printf "Glow not found, markdown rendering not available.\n" >&2
		printf "Output will be raw markdown and might look weird.\n" >&2
		printf "Install glow for easier reading & copy-paste.\n" >&2
		USE_GLOW=false
fi

if ! command -v jq >/dev/null; then
		printf "jq is needed to run this script, but it wasn't found. Please install it to be able to use this script.\n" >&2
		exit 1
fi

SKIP_OK=true
for i in "$@"; do case $i in
		-h|--help)
				printf "\033[37;45;1mxdg-ninja\033[m\n"
				printf "\033[1;3mCheck your \$HOME for unwanted files.\033[m\n"
				printf "\n"
				printf "────────────────────────────────────\n"
				printf "\n"
				printf "\033[3m--help\033[m              \033[1mThis help menu\033[m\n"
				printf "\033[3m-h\033[m\n"
				printf "\n"
				printf "\033[3m--no-skip-ok\033[m        \033[1mDisplay messages for all files checked (verbose)\033[m\n"
				printf "\033[3m-v\033[m\n"
				printf "\n"
				printf "\033[3m--skip-ok\033[m           \033[1mDon't display anything for files that do not exist (default)\033[m\n"
			exit
			;;

		-v|--no-skip-ok)
			SKIP_OK=false
			;;

		--skip-ok)
			SKIP_OK=true
			;;
esac; done
  
if [ -z "${XDG_DATA_HOME}" ]; then
		printf "\033[1;36mThe \$XDG_DATA_HOME environment variable is not set, make sure to add it to your shell configuration before setting any of the other environment variables!\033[m\n" >&2
		printf '\033[1;36m    ⤷ \033[1mThe recommended value is: \033[1;3m$HOME/.local/share\033[m\n' >&2
fi
if [ -z "${XDG_CONFIG_HOME}" ]; then
		printf "\033[1;36mThe \$XDG_CONFIG_HOME environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!\033[m\n" >&2
		printf '\033[1;36m    ⤷ \033[1mThe recommended value is: \033[1;3m$HOME/.config\033[m\n' >&2
fi
if [ -z "${XDG_STATE_HOME}" ]; then
		printf "\033[1;36mThe \$XDG_STATE_HOME environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!\033[m\n" >&2
		printf '\033[1;36m    ⤷ \033[1mThe recommended value is: \033[1;3m$HOME/.local/state\033[m\n' >&2
fi
if [ -z "${XDG_CACHE_HOME}" ]; then
		printf "\033[1;36mThe \$XDG_CACHE_HOME environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!"
		printf '\033[1;36m    ⤷ \033[1mThe recommended value is: \033[1;3m$HOME/.cache\033[m\n' >&2
fi
if [ -z "${XDG_RUNTIME_DIR}" ]; then
		printf "\033[1;36mThe \$XDG_RUNTIME_DIR environment variable is not set, make sure to add it to your shell's configuration before setting any of the other environment variables!\033[m\n" >&2
		printf '\033[1;36m    ⤷ \033[1mThe recommended value is: \033[1;3m/run/user/%d\033[m\n' "$(id -u)" >&2
fi

# Returns 0 if the path doesn't lead anywhere
# Return 1 if the path points to a file, 2 if it points to a directory
check_not_exists_file() {
    # Apply shell expansion
		eval "FILE_PATH=$1"

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
      printf '[\033[1;31m%s\033[m]: \033[1;3m%s\033[m\n' "$NAME" "$FILENAME" >&2
      ;;

    WARN)
      printf '[\033[1;33m%s\033[m]: \033[1;3m%s\033[m\n' "$NAME" "$FILENAME" >&2
      ;;

    INFO)
      printf '[\033[1;36m%s\033[m]: \033[1;3m%s\033[m\n' "$NAME" "$FILENAME" >&2
      ;;

    SUCS)
      [ "$SKIP_OK" = false ] &&
        printf '[\033[1;32m%s\033[m]: \033[1;3m%s\033[m\n' "$NAME" "$FILENAME" >&2
      ;;

    HELP)
      if "$USE_GLOW"; then
          printf '%s' "$HELP" | glow - >&2
      else
          printf '%s' "$HELP" >&2
      fi
      ;;

    esac
}

# Checks that the given file does not exist, otherwise outputs help
check_file() {
    INPUT="$1"
    NAME="$2"

    FILENAME=$(printf '%s' "$INPUT" | jq -r .path)
    MOVABLE=$(printf '%s' "$INPUT" | jq -r .movable)
    HELP=$(printf '%s' "$INPUT" | jq -r .help)

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
		INPUT=$1
		NAME=$(jq -r .name < "$INPUT")

		while IFS= read -r file; do
			check_file "$file" "$NAME"
		done <<EOF
$(jq -rc '.files[]' < "$INPUT")
EOF
}

# Loops over all files in the programs/ directory and calls check_program
enumerate_programs() {
		printf "\033[1;3mStarting to check your \033[1;36m\$HOME.\033[m\n\n" >&2

		for prog_filename in "${0%/*}"/programs/*; do
			check_program "$prog_filename"
		done

		printf "\033[1;3mDone checking your \033[1;36m\$HOME.\033[m\n" >&2
		printf "\033[3mIf you have files in your \033[1;36m\$HOME\033[1;0m that shouldn't be there, but weren't recognised by xdg-ninja, please consider creating a configuration file for it and opening a pull request on github.\033[m\n" >&2
}

enumerate_programs
