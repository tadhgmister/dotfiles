# dotfiles

This is my personal configuration for GNU guix system, it should contain all the necessary details to reconstruct my system
on a new machine with minimal effort. This isn't perfect in practice, I occasionally use "guix install" for something I want available for a while and then build a further dependency on it forgetting it isn't in any of my saved configurations
and there are often configurations for applications that don't make it into these configs such as installing/updating brave (I get it through nix) or browser extensions. 

If you wanted to use this you'd get an installation image of GNU guix from [here][1] probably install the default stuff like gnome so if my config fails you can roll back to a usable OS, then afterwards clone this repo and do `guix home reconfigure home-config.scm && sudo guix system reconfigure os.scm` from the repo directory, it will probably break things with your desktop manager until you reboot, but when you restart it should give you a tty to log into instead of a gui login screen and after logging in it runs dwm. you can use windows+space to get dmenu which works similarly to quicklook on mac, starting to type "alacritty" to launch a terminal or just doing windows+shift+space will open alacritty directly. You probably want to install a browser, I use brave installed through nix so you'd need to do something like `nix-env -i -r` once guix home is configured or temporarily you can do `guix shell ungoogled-chromium -- chromium` to get a browser to trouble shoot nix or guix home or networking or all the things that could go wrong before getting to that point.

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
  - TODO: add some flag to disable the few non free packages (steam and emojis mostly) instead of commenting out then uncommenting after first successful home reconfigure
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
- if ~/src/dwm is not present it breaks the personal diff, which is expected but annoying.

### brave fonts

Initially brave may not be at all happy with default font setup, the work around I found was to do this:

```bash
guix shell fontconfig font-gnu-freefont
fc-cache -v -r
brave
exit
```
The first line enters a shell with font config and some gnu free fonts available, then the second forces the font config cache to rebuild, then run brave (note that if another brave window such as messenger in kiosk mode on space 1 which opens by default this won't open brave fresh) and go into the settings -> content -> customize fonts and set to the font that is desired so it can render. then close brave and exit the guix shell. Assuming you chose a font that is avaialable outside the shell brave should now work as expected.




## SETUP GUIDE TODO STEPS
###  obtain guix installation image

[get an installation image from [here][guixinstaller] choose **GNU Guix System** (if you ctrl+f "installer" it is the one with that in the description) and `dd` it to a flashdrive. If you alreaedy have guix installed and are using nonguix you can also make an installer with non free firmware to use wifi instead of ethernet during installation.

If using the nonguix installer you need to switch to another tty and run `rfkill unblock 1` to allow using the wifi, it may not be `1`, run `rfkill` to list the blocked cards.

[guixinstaller]: https://guix.gnu.org/download/

### Partitioning and formatting drive

This script has not been tested, you should not just run it all in one go and expect it to work (at minimum change the variables at the top to match your actual system, most of these commands need root access and reformatting the wrong drive would obviously be bad.) You should run each command individually to understand what is going on, the code is reasonably commented for what each is attempting to do so if it doesn't actually do it you can debug it yourself.

```bash
# the root drive that we want to put guix on
DRIVE=/dev/sda
# the seperating character for partitions
# when drive is like `sdN` this will probably be empty, for nvme drives this is probably p
# you can run lsblk after the parted command to see what character this is
# for example if the root drive is 'nvme0n1' and the partition is 'nvme0n1p1' this will be p
PARTITION_PREFIX=p
# the hostname we want to use, by my own convension I use this
# appended with "drive" as the mapper name since the mapper name is arbitrary but must be distinct for multiple drives loaded at the same time
# and since when installing a new operating system I need the mapper name to be different from the currently booted system and often want to change the hostname as well.
NEW_HOSTNAME=tadhgframework
# the amount of space for our swapfile, can probably be increased later but may be a pain.
SWAP_SPACE=8G

# partition the drive, gpt partition table, one partition of ~30MiB for the EFI grub
# and another partition to be encrypted to take up rest of drive.
# the esp flag indicates the first partition will hold EFI data
parted --script ${DRIVE} \
   mklabel gpt \
   mkpart primary fat 1MiB 30MiB \
   mkpart primary 30MiB 100% \
   set 1 esp on
# `lsblk | grep ${DRIVE}${PARTITION_PREFIX}1` should print results, if not ensure PARTITION_PREFIX is set properly.
# format EFI partition for fat, -F32 is what the guix manual suggests I don't actually know what it does
mkfs.fat -F32 ${DRIVE}${PARTITION_PREFIX}1

# make keyfile early, this will be removed once the partition is mounted and the cpio file for early boot is setup
# so don't worry about it being written to the root directory, that is the only way I have figured out to get the cpio archive to work reliably.
dd bs=512 count=4 if=/dev/random of=/keyfile.bin iflag=fullblock
chmod -v 0400 /keyfile.bin # probably unnecessary but these flags get propogated into cpio file.
chown root:root /keyfile.bin

# setup the encryption, this command will print warnings and give a confirmation and then ask for the encryption password
# TODO: automate encryption password?
# !! YOU NEED TO REMEMBER THIS PASSWORD, !! literally all the data on the harddrive (other than grub) will need this to access 
cryptsetup luksFormat ${DRIVE}${PARTITION_PREFIX}2 --type luks2
# adds the keyfile as a viable unlocking mechanism so guix can use the keyfile and linux won't have to ask for the decryption password a second time on boot.
cryptsetup luksAddKey $(DRIVE)$(PARTITION_PREFIX)2 /keyfile.bin
# opens the drive, this will put it under /dev/mapper
cryptseetup open ${DRIVE}${PARTITION_PREFIX}2 ${NEW_HOSTNAME}_drive --key-file=/keyfile.bin
# format as btrfs, is fast and has compression options.
mkfs.btrfs /dev/mapper/${NEW_HOSTNAME}_drive

# mount the btrfs root at /mnt
mount /dev/mapper/${NEW_HOSTNAME}_drive /mnt
# create the directory that the efi partition will be mounted to and then mount it (only needed for guix system init)
mkdir -pf /mnt/boot/efi
mount ${DRIVE}${PARTITION_PREFIX}1 /mnt/boot/efi
# create a subvolume for the swapfile and keyfile. Important to have swapfile in a seperate subvolume as snapshotting can't work when a swapfile is present
# since it is a file you should never touch after creation (except maybe to resize it) we will put the keyfile here too so it is similarly hard to access.
btrfs subvolume create /mnt/swap
btrfs filesystem mkswapfile --size ${SWAP_SPACE} /mnt/swap/swapfile

# now create the cpio file from the keyfile
echo "/keyfile.bin" | cpio -ov > /mnt/swap/keyfile.cpio
# now that the keyfile is encoded in the cpio archive it can be recaptured by running `cpio -iv < /swap/keyfile.cpio` (assuming the drive we are setting up is mounted as the root, otherwise add /mnt to the path) which will put the bin file back in the root directory.
# so we don't need the keyfile itself now
rm /keyfile.bin
# also set permissions to the keyfile to lock it down and also lock down the swap folder, no one should be poking around there
chmod -v 0400 /mnt/swap/keyfile.cpio
chown root:root /mnt/swap/keyfile.cpio
chmod -v 0400 /mnt/swap
# we assume the btrfs command already put the correct permissions on the swap file so no need to do that too.

# read some variables that will go into our config
RESUME_OFFSET="$(btrfs inspect-internal map-swapfile -r /mnt/swap/swapfile)"
ROOT_UUID="$(lsblk -o UUID /dev/${DRIVE}${PARTITION_PREFIX}2 -n)"
EFI_UUID="$(lsblk -o UUID /dev/${DRIVE}${PARTITION_PREFIX}1 -n)"

# echo relevent defines to fit into the os definition
# these would be copied to a file used by the operating system definition.

echo ';; uuid of luks encrypted root partition'
echo '(define ROOT-UUID (uuid "${ROOT_UUID}"))'
echo
echo ';; uuid of efi boot partition'
echo '(define EFI-UUID (uuid "${EFI_UUID}"))'
echo
echo ';; the physical offset of swapfile as reported by `btrfs inspect-internal map-swapfile -r /swap/swapfile`'
echo '(define RESUME-OFFSET "${RESUME_OFFSET}")'
echo
echo ';; hostname of machine'
echo '(define HOSTNAME "${NEW_HOSTNAME}")'
echo
echo '(define mapper-target (string-append HOSTNAME "_drive")'
echo '(define root-drive (string-append "/dev/mapper/" mapper-target))'

# if on the installation image enable the cow-store service to copy files over as they are built
# this is needed so when building stuff it is copied over to the flashdrive instead of trying to store everything in ram and running out of memory
# if you are in a live guix system and not the installer this is not an issue and thie command will just fail
herd start cow-store /mnt || echo 'if you are on a guix system ignore this, if you are on an install image cow-store failed to start and you will likely run out of memory if you continue with the install.
```


- after first boot guix pull and guix home reconfigure
- nix-env and brave font workaround
- FONT WORK AROUND `ln -sr .guix-home/profile/share/fonts .local/share/fonts`
  - this gives brave fonts, idk how to specify it in my home config, idk how to get brave to properly read the XDG_DATA_DIRS