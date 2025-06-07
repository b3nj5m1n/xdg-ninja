<!-- markdownlint-disable MD041 -->
<!-- markdownlint-configure-file { "no-inline-html": { "allowed_elements": [div, h1, h4, p, i, img] } } -->

<div align="center">
  <h1>xdg-ninja</h1>
  <h4>
    Because you wouldn't let just anyone into your <i>$HOME</i>
  </h4>
</div>

A shell script that checks your `$HOME` for unwanted files and directories.

<p align="center">
  <img src="https://github.com/user-attachments/assets/4b406636-8a00-41da-9ed2-46f02c5789ab" width="500" alt="xdg-ninja command output" />
</p>

When `xdg-ninja` encounters a file or directory it knows about, it will tell you whether it's possible to move it to the appropriate location, and how to do it.

The configurations are from the [arch wiki page on XDG_BASE_DIR](https://wiki.archlinux.org/title/XDG_Base_Directory), [antidot](https://github.com/doron-cohen/antidot) (thanks to Scr0nch for writing a conversion tool), and crowdsourced by other users.

## Installing

### Manual Installation

Clone the repository, then run the [`./xdg-ninja.sh`](./xdg-ninja.sh) script.

```sh
git clone https://github.com/b3nj5m1n/xdg-ninja
cd xdg-ninja
./xdg-ninja.sh
```

This will run every test in the default configuration.

### [Nix](https://nixos.org)

Turn on [flakes](https://nixos.wiki/wiki/Flakes), then run the following command:

```sh
nix run github:b3nj5m1n/xdg-ninja
```

### [Homebrew](https://brew.sh)

> [!NOTE]
> Due to how `xdg-ninja` is developed, releases are not cut, so Homebrew ships a stale version, therefore you have to install and upgrade `xdg-ninja` from the git HEAD. ref: [#204](https://github.com/b3nj5m1n/xdg-ninja/issues/204)
>
> Homebrew will not upgrade `xdg-ninja` when running a generic `brew upgrade`, you must specifically upgrade `xdg-ninja` from the git HEAD, _see below_

Install:

```sh
brew install xdg-ninja --HEAD
```

Upgrade:

```sh
brew upgrade xdg-ninja --fetch-HEAD
```

### Other Package Managers

`xdg-ninja` is available in many other package managers.

The full list is available on the [repology page](https://repology.org/project/xdg-ninja/versions).

Follow the instructions for your package manager to install `xdg-ninja`.

## Contributing

### Dependencies

- Your favorite POSIX-compliant shell ([bash](https://repology.org/project/bash/packages), [zsh](https://repology.org/project/zsh/packages), [dash](https://repology.org/project/dash-shell/packages), etc.)
- [jq](https://repology.org/project/jq/packages) for parsing the json files
- [find](https://repology.org/project/findutils/versions)

#### Optional

- [glow](https://repology.org/project/glow/packages) for rendering Markdown in the terminal ([bat](https://repology.org/project/bat-cat/packages), [pygmentize](https://repology.org/project/pygments/versions) or [highlight](https://repology.org/project/highlight/packages) can be used as a fallback, but glow's output is clearer therefore glow is recommended)

### Configuration

The configuration is done in the [`./programs/`](./programs/) directory, which should be located in the same working directory as the [`xdg-ninja.sh`](./xdg-ninja.sh) script. This can be overridden with the `XN_PROGRAMS_DIR` environment variable.

You define a program, and then a list of files and directories which that program ruthlessly puts into your `$HOME` directory.

For each file/directory, you specify if it can be (re)moved.

If this is the case, you also specify instructions on how to accomplish this in Markdown.

Files in this directory can have any name, but using the name of the program is recommended.

### Automatically Generating Configuration

For x86_64 Linux systems, you can download the `xdgnj` binary from the [releases page](https://github.com/b3nj5m1n/xdg-ninja/releases).

Alternatively, you can build it from source using `cabal` or `stack`, use the nix flake or use the provided docker image.

> To be clear, this is just a tool that will help you automatically generate the config files, you still only need your shell to run the tests

#### Available commands

```sh
xdgnj add # Adds a new configuration
xdgnj prev programs/FILE.json # Preview the configuration for a program
xdgnj edit programs/FILE.json # Edit the configuration for a program
xdgnj run # Mostly the same as running the shell script
```

#### Prebuilt Binaries

> [!IMPORTANT]
> The binaries only run on x86_64 Linux systems.

```sh
curl -fsSL -o xdgnj https://github.com/b3nj5m1n/xdg-ninja/releases/latest/download/xdgnj
chmod +x xdgnj
```

#### Building from source

You can use `cabal build` or `stack build`

#### Nix

```sh
nix run github:b3nj5m1n/xdg-ninja#xdgnj-bin ...
```

#### Docker

Use the provided dockerfile in [`./haskell/build/`](./haskell/build/).

### Manually Creating Configuration

We're going to use `git` as an example.

By default, it puts the file `.gitconfig` into `$HOME`.

Luckily, the XDG spec is supported by git, so we can simply move the file to `$XDG_CONFIG_HOME/git/config`.

We can use that last sentence as our instructions. In this case, there are no newlines, so escaping this string for use in json is trivial, however, this is how you should generally approach it:

```sh
echo "Luckily, the XDG spec is supported by git, so we can simply move the file to _$XDG_CONFIG_HOME/git/config_." | jq -aRs .
```

Let's see what the output of this command looks like for something a little more sophisticated.

Here's an example file:

```sh
cat example.md
```

```text
Currently not fixable.

_(But you can probably just delete the dir)_
```

Here's what catting this file into `jq` produces:

```sh
cat example.md | jq -aRs .
```

```text
"Currently not fixable.\n\n_(But you can probably just delete the dir)_\n"
```

Now, we can assemble our final json file:

```json
{
    "name": "git",
    "files": [
        {
            "path": "$HOME/.gitconfig",
            "movable": true,
            "help": "Luckily, the XDG spec is supported by git, so we can simply move the file to _$XDG_CONFIG_HOME/git/config_.\n"
        }
    ]
}
```

Saving this as `git.json` in the [`./programs/`](./programs/) directory will result in the script picking it up and checking the file.

If you've created a configuration for a file that isn't in the official repository yet, make sure to create a pull request so that other people can benefit from it as well.
