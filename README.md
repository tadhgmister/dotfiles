# dotfiles

This is my personal configuration for GNU guix system, it should contain all the necessary details to reconstruct my system
on a new machine with minimal effort. This isn't perfect in practice, I occasionally use "guix install" for something I want available for a while and then build a further dependency on it forgetting it isn't in any of my saved configurations
and there are often configurations for applications that don't make it into these configs such as installing/updating brave (I get it through nix) or browser extensions. 

If you wanted to use this you'd get an installation image of GNU guix from [here][1] probably install the default stuff like gnome so if my config fails you can roll back to a usable OS, then afterwards clone this repo and do `guix home reconfigure home-config.scm && sudo guix system reconfigure os.scm` from the repo directory, it will probably break things with your desktop manager until you reboot, but when you restart it should give you a tty to log into instead of a gui login screen and after logging in it runs dwm. you can use windows+space to get dmenu which works similarly to quicklook on mac, starting to type "alacritty" to launch a terminal or just doing windows+shift+space will open alacritty directly. You probably want to install a browser, I use brave installed through nix so you'd need to do something like `nix-env -iA brave` or temporarily you can do `guix shell ungoogled-chromium -- chromium` to get a browser to look up the relevant nix commands.

[1]: https://guix.gnu.org/en/download/

I use alacritty as my terminal and dwm as my window manager, I specifically set up dwm to hide the top bar unless I'm holding the windows key and any time a window asks for attention it shows the top bar such that tapping the windows key hides it. alacritty sends this signal when it prints the bel character so by putting a bel character in my prompt message I get a noticable but non intrusive alert when a command finishes in another space. Similarly my chat client dino raises the same signal wen I get a new message so chat notifications are noticable but unintrusive to my work.

I am a fan of duckduckgo's bangs and have added a list of bangs I find useful to the list of auto-completes in dmenu so they open in brave directly. This lets me type windows+space "!wea" to autofill the whole command needed to visit environment canadas website for ottawa weather.

Dino uses an outdated version, when they updated to version 4 to implement "proper" notifications they removed the x signals that most other desktop environments ignore but I rely on for my notifications.

Also the info bar in dwm displays in dozonal by default, if you click on it while holding the windows key it switches to a normal clock display, changing `int usedoz = 1` to be 0 in the dwm\_personal.diff and then `guix home reconfigure home-config.scm` would change it to default to the normal display.


## Notes about initial install

once the system is fully setup my `guixman` script can be used for the main operations, running without a recognized command prints the source file which serves as the documentation for the viable options since it is a pretty simple switch case. However, 'bootstrapping' this environment is not trivial since there are some dependencies on nongnu software and nix stuff which can get in the way. I will try to explain the dependency chains here (mainly for my own sake) and intend to setup scripts to help this initial setup phase.

### nix
- guix home creates the `.nix-channels` file that specifies which channel should be used and `.nix-defexpr/default.nix` which specify what packages to install
- `nix-channels --update` updates the channels based on the .nix-channels file and populates `~/.nix-defexpr/channels` directory
- `nix-env -i -r` uses both things in `.nix-defexpr` mentioned above to install the packages desired through nix.

### non gnu guix channels

- guix system must be used with libre kernel, it also specifies the substitute server for nonguix
- guix home must be used without non free packages included, this sets up the channels as a home service
- guix pull updates the index of the new channels (may be skippable but kind of doubt it)
- guix system and guix home can now use non free packages.

### partitioning / initial disk creation

TODO: write a script to automate this process and write the details to a scheme module that exports relevant variables

- partition device with gpt partition table with one partition of 30MB and a second using the rest of the harddrive
  - the 30MB will hold the grub bootloader, it uses less than 12MB on my system but 30 is plenty and having the bootloader fail to install because there isn't enough space sucks.
  - if your device is relatively old it may not support gpt partition table, use mbr if you are unsure.
- format the first partition as fat32
- encrypt the second partition with `cryptosetup` and `luks`
- mount the encrypted drive and format the mapper device with btrfs
- make subvolumes as desired, at minimum one to put the swap file in.
- make swapfile in swap subvolume
- OPTIONAL: make keyfile for luks ([not supported properly on guix atm][luks-mapped-device-with-options])

[luks-mapped-device-with-options]: https://issues.guix.gnu.org/70826

### hard coded paths

- os.scm defines the home path of the main user as `/home/tadhg` which is also hard coded in several places in home-config.scm.
- mainly guixman but possibly others hard code the paths of dwm and dotfiles under `~/src/`, which would be ideal to change and de-dup with smarter code construction.