<div>
  <h1 align="center">xdg-ninja</h1>
  <h5 align="center">Because you wouldn't let just anyone into your <i>$HOME</i></h5>
</div>

A shell script which checks your _$HOME_ for unwanted files and directories.

<p align="center">
  <img src="https://s8.gifyu.com/images/Peek-2022-05-13-16-07.gif" width="500"/>
</p>

When it encounters a file it knows about, it will tell you whether it's possible to move this file to an appropriate location, and how to do it.

The configurations are from the [arch wiki page on XDG_BASE_DIR](https://wiki.archlinux.org/title/XDG_Base_Directory), [antidot](https://github.com/doron-cohen/antidot) (thanks to Scr0nch for writing a conversion tool), and contributed by other users.

## Running

### Using nix

If you're using [nix](https://nixos.org) and have flakes turned on, you can just run the following command:
```sh
nix run github:b3nj5m1n/xdg-ninja
```

### Cloning Manually

Clone the repository somewhere, then run the _./xdg-ninja.sh_ script.

This will run every test in the default configuration.

### Installing with Homebrew

To install xdg-ninja with [Homebrew](https://brew.sh), run `brew install xdg-ninja` to install the script and all of its dependencies, then run the `xdg-ninja` command.

## Dependencies

- your favorite POSIX-compliant shell ([bash](https://repology.org/project/bash/packages), [zsh](https://repology.org/project/zsh/packages), [dash](https://repology.org/project/dash-shell/packages), ...)
- [jq](https://repology.org/project/jq/packages) for parsing the json files

### Optional

- [glow](https://repology.org/project/glow/packages) for rendering Markdown in the terminal ([bat](https://repology.org/project/bat-cat/packages), [pygmentize](https://repology.org/project/pygments/versions) or [highlight](https://repology.org/project/highlight/packages) can be used as fallback, but glow's output is clearer and therefore glow is recommended)

## Configuration

The configuration is done in the _programs/_ directory, which should be located in the same working directory as the xdg-ninja.sh script. This can be overriden with the `XN_PROGRAMS_DIR` environment variable.

You define a program, and then a list of files and directories which this program ruthlessly puts into your _$HOME_ directory.

For each file/directory, you specify if it can be (re)moved.

If this is the case, you also specify instructions on how to accomplish this in Markdown.

Files in this directory can have any name, but using the name of the program is encouraged.

### Automatically Generating Configuration

You can download the _xdgnj_ executable from the releases page. Alternatively, you can use the nix flake or build it from scratch using _cabal_, _stack_, or the provided docker image in _build/_. (To be clear, this is just a tool that will help you automatically generate the config files, you still only need your shell to run the tests)

Available commands:
```sh
xdgnj add # Adds a new configuration
xdgnj prev programs/FILE.json # Preview the configuration for a program
xdgnj edit programs/FILE.json # Edit the configuration for a program
xdgnj run # Mostly the same as running the shell script
```

#### Using nix

If you're using [nix](https://nixos.org) and have flakes turned on, you can just run the following command:
```sh
nix run github:b3nj5m1n/xdg-ninja#xdgnj-bin ...
```

#### Building from scratch

You can use `cabal build`, `stack build`, or the provided dockerfile in _build/_.

### Manually

We're going to use _git_ as an example.

It puts the file _.gitconfig_ into _$HOME_.

Luckily, the XDG spec is supported by git, so we can simply move the file to _XDG_CONFIG_HOME/git/config_.

We can use that last sentence as our instructions. In this case, there are no newlines, so escaping this string for use in json is trivial, however, this is how you should generally approach it:
```sh
echo "Luckily, the XDG spec is supported by git, so we can simply move the file to _XDG_CONFIG_HOME/git/config_." | jq -aRs .
```

Let's see what the output of this command looks like for something a little more sophisticated.
Here's an example file:
```sh
cat example.md
```
```
Currently not fixable.

_(But you can probably just delete the dir)_
```
Here's what catting this file to the _jq_ command produces:
```sh
cat example.md | jq -aRs .
```
```
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
            "help": "Luckily, the XDG spec is supported by git, so we can simply move the file to _XDG_CONFIG_HOME/git/config_.\n"
        }
    ]
}
```

Saving this as _git.json_ in the _programs/_ directory will result in the script picking it up and checking the file.

If you've created a configuration for a file that isn't in the official repository yet, make sure to create a pull request so that other people can benefit from it as well.
