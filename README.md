<div>
  <h1 align="center">xdg-ninja</h1>
  <h5 align="center">Because you wouldn't let just anyone into your <i>$HOME</i></h5>
</div>

A shell script which checks your _$HOME_ for unwanted files and directories.

<p align="center">
  <img src="https://s8.gifyu.com/images/Peek-2022-05-13-16-07.gif" width="500"/>
</p>

When it encounters a file it knows about, it will tell you whether it's possible to move this file to an appropriate location, and how to do it.

Currently, a subset of the [arch wiki page on XDG_BASE_DIR](https://wiki.archlinux.org/title/XDG_Base_Directory) is implemented as configurations.

## Running

Clone the repository somewhere, then run the _./xdg-ninja.sh_ script.

This will run every test in the default configuration.

## Dependencies

- /bin/sh, obviously
- [jq](https://repology.org/project/jq/packages) for parsing the json files

### Optional

- [glow](https://repology.org/project/glow/packages) for rendering markdown in the terminal
- [cabal](https://repology.org/project/cabal/packages) for compiling the helper program for creating configurations

## Configuration

The configuration is done in the _programs/_ directory.

You define a program, and then a list of files and directories which this program ruthlessly puts into your _$HOME_ directory.

For each file/directory, you specify if it can be (re)moved.

If this is the case, you also specify instructions on how to accomplish this in markdown.

Files in this directory can have any name, but using the name of the program is encouraged.

### Automatically Generating Configuration

You need _haskell_ and _cabal_ installed. (To be clear, this is just for a tool that will help you automatically generate the config files, you still only need bash to run the tests)

Run the following command:
```bash
cabal build
```

You should now have a binary which you can run. You'll find it in somewhere in _dist-newstyle/build_, for example in _dist-newstyle/build/x86_64-linux/ghc-9.0.2/add-program-0.1.0.0/x/add-program/build/add-program/add-program_.

Execute this binary in this directory. It will guide you through the process.

### Manually

We're going to use _git_ as an example.

It puts the file _.gitconfig_ into _$HOME.

Luckily, the XDG spec is supported by git, so we can simply move the file to _XDG_CONFIG_HOME/git/config_.

We can use that last sentence as our instructions. In this case, there are no newlines, so escaping this string for use in json is trivial, however, this is how you should generally approach it:
```bash
echo "Luckily, the XDG spec is supported by git, so we can simply move the file to _XDG_CONFIG_HOME/git/config_." | jq -aRs .
```

Let's see what the output of this command looks like for something a little more sophisticated.
Here's an example file:
```bash
cat example.md
```
```
Currently not fixable.

_(But you can probably just delete the dir)_
```
Here's what catting this file to the _jq_ command produces:
```bash
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

If you've created a configuration for a file that isn't in the offical repository yet, make sure to create a pull request so that other people can benefit from it as well.
