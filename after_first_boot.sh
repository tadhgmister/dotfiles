#!/bin/sh

# note that these variables evaluate BEFORE the sudo happens, so user=tadhg, home=/home/tadhg

# ~/src/dotfiles gets created by the initial setup script which is run by root so these are all owned by root
# start by fixing that.
sudo chown -R ${USER}:users ${HOME}

# use nmtui to setup wifi
sudo nmtui

echo "- getting fprintd and enrolling fingerprint"
# next enroll a fingerprint so we can login with fingerprint nstead of password, also for sudo
guix shell fprintd -- sudo fprintd-enroll ${USER}

echo "- reconfiguring with temporary home definition with channels"
guix home reconfigure -L ~/src/dotfiles/packages -e "(@ (tadhg initial-setup) tmp-home)"

echo "- doing a full guix pull to update channels"
# pull channels to have access to non-guix stuff
guix pull #--url=https://codeberg.org/guix/guix-mirror

echo "- updating nix channels"
# update nix channels with newly put ~/.nix-channels file
nix-channel --update

echo "- reconfiguring home with full definition"
# do the home config again, this time it will run the nix-env command all the way.
guix home reconfigure ~/src/dotfiles/home-config.scm -L ~/src/dotfiles/packages

echo "- FINISHED, now logout of tty and log back in to get into dwm"
