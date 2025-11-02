#!/bin/sh

# create src folder in home folder and move /tmp/dotfiles to there (as it was created by the initial setup script)
mkdir ~/src
sudo mv /tmp/dotfiles ~/src/
# make it owned by current user, I.E. same owner and group as the current home folder
sudo chown -R --reference=~/ ~/src/dotfiles

# use nmtui to setup wifi
sudo nmtui


echo "- reconfiguring with temporary home definition with channels"
guix home reconfigure -L ~/src/dotfiles/packages -e "(@ (tadhg initial-setup) tmp-home)" --max-jobs=8

echo "- doing a full guix pull to update channels"
# pull channels to have access to non-guix stuff
guix pull #--url=https://codeberg.org/guix/guix-mirror

echo "- updating nix channels and rebuilding os "
# update nix channels with newly put ~/.nix-channels file
nix-channel --update &

guix system build -L ~/src/dotfiles/packages ~/src/dotfiles/os.scm --max-jobs=8

echo "- reconfiguring home with full definition"
# do the home config again, this time it will run the nix-env command all the way.
guix home reconfigure ~/src/dotfiles/home-config.scm -L ~/src/dotfiles/packages --max-jobs=8

echo "- reconfiguring system"
sudo guix system reconfigure ~/src/dotfiles/os.scm -L ~/src/dotfiles/packages --max-jobs=8

echo "- getting fprintd and enrolling fingerprint"
# next enroll a fingerprint so we can login with fingerprint instead of password, also for sudo
# done at the end so initial install can make use of the os-tiny.scm which doesn't include relevant services for fprintd
guix shell fprintd -- sudo fprintd-enroll ${USER}

echo "- FINISHED, now logout of tty and log back in to get into dwm"
