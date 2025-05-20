# dotfiles

This is my personal configuration for GNU guix system, it should contain all the necessary details
to reconstruct my system on a new machine with minimal effort. This isn't perfect in practice, I
occasionally use "guix install" for something I want available for a while and then build a further
dependency on it forgetting it isn't in any of my saved configurations and there are often
configurations for applications that don't make it into these configs such as installing/updating
brave (I get it through nix) or browser extensions.

## quirks and info

When the computer boots up it drops you into a tty with no graphics stuff until after you
login. When you login to the default tty it starts up [dwm] a minimalist tiling window manager. A
few useful keybindings to know:

- win+\` moves to space 0
- win+1 moves to space 1 where I keep IM clients
- win+2 moves to space 2 where I keep brave my internet browser
- win+3 moves to space 3 where I keep emacs
- win+*other number* moves to the rest of the spaces, '-' is the 11th space
- win+shift+number moves the currently selected window to the selected space
- win+shift+spacebar opens alacritty my terminal
- win+spacebar opens dmenu to run programs (similar to quicklook on mac)


The top bar is set to showup while the windows key is held or when a window generates an alert,
which alacritty does when a command finishes or I get a message through [Dino] my chat client so
notifications are unintrusive. By default the top bar shows time and date in my dozonal system,
clicking on it with windows key held down switches to normal display.

[dwm]: https://dwm.suckless.org/
[Dino]: https://dino.im/

I am a fan of duckduckgo's bangs and have added a list of bangs I find useful to the list of
auto-completes in dmenu so they open in brave directly. This lets me type windows+space "!wea" to
autofill the whole command needed to visit environment canadas website for ottawa weather.

## Setup instructions for existing guix installation

If you already have a guix installation the process to start using my definitions is not too complicated, the only detail is that you will need to pass `-L packages` to point to the `packages` subfolder in the guile load path, otherwise you will get errors saying `no code for module (tadhg ...)`.

Also note that I use non-guix for some packages, `packages/tadhg/packagelist.scm` is where they are specified it would be easy enough to remove the ones that fail when trying to reconfigure. Similarly in `os.scm` commenting out the 3 lines below `;; COMMENT OUT THESE LINES TO REMOVE DEPENDENCY ON NON GUIX` as well as the imports will remove dependencies on non-guix channels.

<!-- Alternatively setup the `system-info` module as described in the next section and before reconfiguring based on `os.scm` do this first:

```sh
guix home reconfigure -L packages -e "(@ (tadhg initial-setup) tmp-home)"
sudo guix system reconfigure -L packages -e "(@ (tadhg-initial-setup) tmp-os)"
guix pull
nix-channel --update
```

NOTE: I have not properly tested the initial-setup tmp-os and tmp-home, they probably don't work -->

### installing the os configuration from an existing guix system

The os definition is setup to rely on boot configuration and filesystems that are stored under packages/system-info/` which is not directly commited to the repo as it will vary for every machine. You can generate the files there by running this sh code and then editing the files manually to accurately represent your own system:

```sh
mkdir packages/system-info
cp template-system-info.scm packages/system-info/setup.scm
cat > packages/system-info/details.scm <<EOF
(define-module (system-info details))
;; uuid of luks encrypted root partition
(define-public ROOT-UUID "ROOT_UUID")

;; uuid of efi boot partition
(define-public EFI-UUID "EFI_UUID")

;; the physical offset of swapfile as reported by "btrfs inspect-internal map-swapfile -r /swap/swapfile"
(define-public RESUME-OFFSET "RESUME_OFFSET")
;; hostname of machine
(define-public HOSTNAME "$(hostname)")
;; main user of the machine
(define-public USERNAME "${USER}")
EOF
```
If you'd rather keep everything in one file you can also copy the variables from `details.scm` into `setup.scm` and remove the corresponding import, only the `wrap-os` function is used outside of that folder so the way you set it up is up to you.

Once `packages/system-info` contains a viable definition you can run `sudo guix system reconfigure os.scm -L packages` to reconfigure the system

### using home configuration from existing guix system

The home configuration also installs packages from nix on reconfigure, if it throws errors it shouldn't stop the rest of the home config from being used but to get that working you will need to first enable `(service nix-service-type)` in your operating system and also update nix-channels, I have setup a temporary home config to help with this, so you can run this:

```sh
guix home reconfigure -L packages -e "(@ (tadhg initial-setup) tmp-home)"
nix-channel --update
guix home reconfigure -L packages home-config.scm
```
note that this will also update guix channels so running a `guix pull` along with the nix-channels update may be a good idea if you need to setup non-guix channel.

## installing from scratch

**Please note** that I do not recommend trying to go through this process without Tadhg's assistance, as much as the process has been automated it hasn't been thoroughly tested and I wouldn't expect it to work seamlessly.


First get yourself a [guix installation media][1] and put it on a flashdrive based on their instructions (the iso file can be `dd`ed directly to the block device of the usb)

[1]: https://guix.gnu.org/manual/en/html_node/USB-Stick-and-DVD-Installation.html

Once you are booted into the guix install media do ctrl+alt+F3 (possibly add fn key) to navigate to a tty and hit enter to get a shell prompt. The following commands can be used to setup internet and clone this repo:

```sh
# SKIP TO BELOW COMMENT IN ALL CAPS IF YOU HAVE ETHERNET
# on my machine the wifi card was index 1, use 'rfkill list' to see if your wifi needs to be unblocked
rfkill unblock 1
# create a wpa supplicant config file with wifi network details
# you can also do 'nano wpa_sup.conf' and type in the contents which would likely be easier
cat > wpa_sup.conf  <<EOF
network={
  key_mgmt=WPA-PSK
  ssid="NETWORK_NAME"
  psk="WIFI_PASSWORD"
}
EOF

# run 'ip addr' to see what the interface name of your wifi is, is going to start with w.
WIFI_INTERFACE=wlp166s0
# start wpa_supplicant to connect to wifi
wpa_supplicant -c wpa_sup.conf -i ${WIFI_INTERFACE} -B


# SKIP TO HERE IF YOU HAVE ETHERNET

# start dhcp client to obtain IP address so we can do networking,
# if you skipped the wifi stuff get the interface of ethernet to put here.
dhclient -V ${WIFI_INTERFACE}
# clone the dotfiles repo and go into the folder
git clone https://github.com/tadhgmister/dotfiles.git
cd dotfiles
```

next `nano initial_setup_script.sh` and change the variables at the top to your preference, after saving and closing that file establish the device you want to install guix on by doing `lsblk`, run the initial setup with `./initial_setup_script.sh /dev/sdX` replacing `sdX` with the device as it is shown in lsblk.

The setup script has these prompts:
- a 'YES' confirmation as it will override all data in the selected device
- an encryption password that will need to be typed in once at boot
- it will setup the whole operating system which will take a while
- asks for root password, this will probably never be used unless you get locked out of your own user
- asks for your user password which once you setup fingerprint will be rarely used, but is used once for first login.

while the script is running you can switch to other tty (ctrl+alt+F#), F2 has the guix info pages to view documentation and 3-9 have shells to potentially view code in this repo while it runs.

Once the script is done `poweroff` and unplug the install media, then power on and it should ask for the decryption password. Once it is booted type in the login name you set in the setup script and password entered at the end of the script, once logged in you can run `~/src/dotfiles/after_first_boot.sh` to setup fingerprint and home config. Once that has finished you can `exit` the tty and when you log back in it should put you into dwm.