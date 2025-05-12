#!/usr/bin/env sh

set -e

if [[ ! $USER =~ ^root$ ]]; then
    echo "this script must be run as root, it partitions drives and such so you have to run it with sudo"
    exit 1
else
    if [ -z $SUDO_USER ]; then
	# running directly as root, assume this is on the installation media
	# ideally we would set sudo_user to something read from the scm definitions but this is fine for my usage
	ON_INSTALL_MEDIA=t
	SUDO_USER=tadhg
    else
	# running in sudo, will
	ON_INSTALL_MEDIA=f
    fi
fi

if [ $# -ne 1 ]; then
    echo "must specify device to partition as argument"
    exit 1
fi
# the root drive that we want to put guix on
DRIVE=$1
# check if the drive or some subtext of the drive is mounted, do not proceed if so
if mount | grep ${DRIVE} > /dev/null; then
    echo "it looks like the drive or a subpartition is mounted, exiting"
    exit 1
fi
# start building dependencies in background, this way if they are not present the 'shell' commands below won't get blocked as long
guix build parted cryptsetup cpio --max-jobs=8 > /dev/null &


# mimic the prompt from 'cryptsetup luksFormat' since we want to get conformation before rewriting the partition table
echo "WARNING: this operation will make all data on ${DRIVE} unrecoverable"
read -p "to continue type 'yes' in all capitals: " choice
if [[ ! $choice =~ ^YES$ ]]; then
   echo "did not get 'yes' in all capitals, aborting"
   exit 1
fi
# the seperating character for partitions
# when drive is like `sdN` this will probably be empty, for nvme drives this is probably p
# you can run lsblk after the parted command to see what character this is
# for example if the root drive is 'nvme0n1' and the partition is 'nvme0n1p1' this will be p
PARTITION_PREFIX=
# the hostname we want to use, by my own convension I use this
# appended with "drive" as the mapper name since the mapper name is arbitrary but must be distinct for multiple drives loaded at the same time
# and since when installing a new operating system I need the mapper name to be different from the currently booted system and often want to change the hostname as well.
NEW_HOSTNAME=tadhgframework
# the amount of space for our swapfile, can probably be increased later but may be a pain.
SWAP_SPACE=12G


echo "- Writing partition table"
# partition the drive, gpt partition table, one partition of ~30MiB for the EFI grub
# and another partition to be encrypted to take up rest of drive.
# the esp flag indicates the first partition will hold EFI data
guix shell parted -- parted --script ${DRIVE} \
   mklabel gpt \
   mkpart primary fat16 1MiB 500MiB \
   mkpart primary btrfs 500MiB 100% \
   set 1 esp on

# now check that the partition we expect to exist does exist,
# TPDP this could probably be setup to automatically detect the partition prefix instead of needing it manually
if ls ${DRIVE}${PARTITION_PREFIX}1 > /dev/null ; then
    echo "- efi partition ${DRIVE}${PARTITION_PREFIX}1 is detected, formatting"
else
    echo "efi partition ${DRIVE}${PARTITION_PREFIX}1 NOT detected, run lsblk and double check PARTITION_PREFIX variable in this script"
    exit 1
fi
# format EFI partition for fat, -F32 is what the guix manual suggests
mkfs.fat -F32 ${DRIVE}${PARTITION_PREFIX}1

echo "- making luks encryption keyfile"
# make keyfile early, this will be removed once the partition is mounted and the cpio file for early boot is setup
# so don't worry about it being written to the root directory, that is the only way I have figured out to get the cpio archive to work reliably.
dd bs=512 count=4 if=/dev/random of=/keyfile.bin iflag=fullblock
chmod -v 0400 /keyfile.bin # probably unnecessary but these flags get propogated into cpio file.


echo "- Encrypting drive and opening"
# setup the encryption, this command will print warnings and give a confirmation then encrypt with the generated keyfile as the key
# the -q skips confirmation, we already rewrote the parition table
# note that apparently with luks2 grub install just can't decrypt the drive? so using luks1
guix shell cryptsetup -- cryptsetup luksFormat --type luks1 -q ${DRIVE}${PARTITION_PREFIX}2 /keyfile.bin
# adds a password which will be kind of important to actually get into the drive once booted since the keyfile is going to live on the encrypted drive
guix shell cryptsetup -- cryptsetup luksAddKey ${DRIVE}${PARTITION_PREFIX}2 --key-file=/keyfile.bin
# opens the drive, this will put it under /dev/mapper
guix shell cryptsetup -- cryptsetup open ${DRIVE}${PARTITION_PREFIX}2 ${NEW_HOSTNAME}_drive --key-file=/keyfile.bin
echo "- drive opened to /dev/mapper/${NEW_HOSTNAME}_drive, Formatting as btrfs and mounting"
# format as btrfs, is fast and has compression options.
mkfs.btrfs /dev/mapper/${NEW_HOSTNAME}_drive

# mount the btrfs root at /mnt, will use very high compression for initial creation so files that are effectively never modified
# are highly compressed, including kernel and init scripts etc in guix store.
mount /dev/mapper/${NEW_HOSTNAME}_drive /mnt --options=compress-force=zstd:7
# create the directory that the efi partition will be mounted to and then mount it (only needed for guix system init)
mkdir -p /mnt/boot/efi
mount ${DRIVE}${PARTITION_PREFIX}1 /mnt/boot/efi
# create several subvolumes for guix and nix store that will have different compression options
# the home directory that may be desirable to mount to other operating systems
# and the swap subvolume so the swap file doesn't interfere with any snapshotting stuff
# since we will lock down the swap subvolume we will also keep the keyfile in there
btrfs subvolume create /mnt/swap /mnt/gnu /mnt/nix /mnt/home
echo "- mounted under /mnt and created subvolumes, making swapfile and keyfile.cpio"
btrfs filesystem mkswapfile --size ${SWAP_SPACE} /mnt/swap/swapfile

# now create the cpio file from the keyfile
echo "/keyfile.bin" | guix shell cpio -- cpio -ov > /mnt/swap/keyfile.cpio
# now that the keyfile is encoded in the cpio archive it can be recaptured by running `cpio -iv < /swap/keyfile.cpio` (assuming the drive we are setting up is mounted as the root, otherwise add /mnt to the path) which will put the bin file back in the root directory.
# so we don't need the keyfile itself now
rm /keyfile.bin
# also set permissions to the keyfile to lock it down and also lock down the swap folder, no one should be poking around there
chmod -v 0400 /mnt/swap/keyfile.cpio
chmod -v 0400 /mnt/swap
# we assume the btrfs command already put the correct permissions on the swap file so no need to do that too.

echo "- partition is ready, writing system-info.scm"

# read some variables that will go into our config
RESUME_OFFSET="$(btrfs inspect-internal map-swapfile -r /mnt/swap/swapfile)"
# note that after being opened the partitions shows the mapper device as well
# so use dedup on size to only print the first one
ROOT_UUID="$(lsblk -o UUID ${DRIVE}${PARTITION_PREFIX}2 -n --dedup SIZE)"
EFI_UUID="$(lsblk -o UUID ${DRIVE}${PARTITION_PREFIX}1 -n)"

# copy this folder (assuming this is being run from the dotfiles folder) to the mount
# in order to setup system-info module there without touching the one in this folder
mkdir -p /mnt/home/$SUDO_USER/src/dotfiles/
cp -r ./ /mnt/home/$SUDO_USER/src/dotfiles/
# echo relevent defines to fit into the os definition
# these would be copied to a file used by the operating system definition.
mkdir -p /mnt/home/$SUDO_USER/src/dotfiles/packages/system-info
cat > /mnt/home/$SUDO_USER/src/dotfiles/packages/system-info/details.scm <<EOF
(define-module (system-info details))
;; uuid of luks encrypted root partition
(define-public ROOT-UUID "${ROOT_UUID}")

;; uuid of efi boot partition
(define-public EFI-UUID "${EFI_UUID}")

;; the physical offset of swapfile as reported by "btrfs inspect-internal map-swapfile -r /swap/swapfile"
(define-public RESUME-OFFSET "${RESUME_OFFSET}")
;; hostname of machine
(define-public HOSTNAME "${NEW_HOSTNAME}")
EOF

# and copy the template system info to the setup file, the system-info is in .gitignore so will not be edited by updates to the repo
# so only this script (with above variables) and the template file need to be in sync (and the interface that os.scm depends on from it)
cp -p template-system-info.scm /mnt/home/$SUDO_USER/src/dotfiles/packages/system-info/setup.scm


echo "- details for drive saved to packages/system-info/ in current folder which has been copied to /mnt/home/${SUDO_USER}/src/dotfiles"
echo "  note that these will be owned by root, if you need to change them you can fix that but otherwise leaving them owned by root should be fine"

# if on the installation image enable the cow-store service to copy files over as they are built
if [[ $ON_INSTALL_MEDIA =~ t ]]; then
    herd start cow-store /mnt
fi

echo "- building system"
# do the system build first, then we will build the home config at the same time as copying over the system config
guix system build -L /mnt/home/$SUDO_USER/src/dotfiles/packages os.scm --max-jobs=8

echo "- system is built, copying over and building home config"
# NOTE: I would love to crank max-jobs as high as possible for the home build
# but if it is the one to start before the system init it is VITAL it doesn't use up ALL the job slots
# as that would force it to finish before the system init can begin which would totally defeat the purpose
# this order is preferable for the behaviour of fg below, so we can notify the user as soon as the system is copied and allow the home build to take longer if necessary
guix home build -L /mnt/home/$SUDO_USER/src/dotfiles/packages home-config.scm --max-jobs=3 &
guix system init -L /mnt/home/$SUDO_USER/src/dotfiles/packages os.scm /mnt
echo "- system is initialized, you can interrupt this script and go use your system now if you wish, or wait for home config to be copied"
# we need to use fg to move the home build back to foreground so if the user interrupts it cancels the build, using wait would not give ideal results
fg || echo "home finished building before system was copied over"
echo "- home config is built and will now be copied over, note you will still need to do 'guix home reconfigure' from the booted system"
# there might be a way to capture the path from the process before to avoid needing to recompute the derivation again but it is not a huge time waste
HOMECONFIGPATH=$(guix home build --no-substitutes -L /mnt/home/$SUDO_USER/src/dotfiles/packages home-config.scm)


# this uses the internal function copy-closure from the system init script to copy the path to the home config over to the drive
# for some reason the binding doesn't exist until the module is reloaded? might have something to do with autoload stuff
guix repl <<EOF
(reload-module (resolve-module (quote (guix scripts system))))
(use-modules (guix store) (guix monads))
(with-store store
 (run-with-store store
   ((@@ (guix scripts system) copy-closure) "${HOMECONFIGPATH}" "/mnt")))
EOF

echo "- files needed for home config are copied over, you should be able to run 'guix home reconfigure' from booted system quickly"
sync
echo "- FINISHED, unmounting drive"
umount -R /mnt
guix shell cryptsetup -- cryptsetup close ${NEW_HOSTNAME}_drive
