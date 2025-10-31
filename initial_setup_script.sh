#!/bin/sh

# the hostname we want to use, by my own convension I use this
# appended with "drive" as the mapper name since the mapper name is
# arbitrary but must be distinct for multiple drives loaded at the
# same time and since when installing a new operating system I need
# the mapper name to be different from the currently booted system and
# often want to change the hostname as well.
NEW_HOSTNAME=tadhg_aux

# the amount of space for our swapfile, can probably be increased later but may be a pain.
SWAP_SPACE=12G

# if running this script with sudo the username is automatically extracted from the current user
# otherwise (like in install media) this username is used
# it only affects where the dotfiles with system-info is stored which is important for reconfiguring system
USERNAME_TO_USE_IF_RUNNING_FROM_INSTALL_MEDIA=tadhg



set -e
# TODO: figure out how to early on chroot into the formatted drive
# instead of relying on cow-store to put everything in tmp directory
# and then copy over to gnu/store as a seperate step. Should also be
# able to get away with one guix pull in whole process.

if [[ ! $USER =~ ^root$ ]]; then
    echo "run this script like 'guix shell parted cryptsetup -- sudo ./initial_setup_script.sh /dev/??'"
    echo "it needs both sudo and those programs which are installed on guix installer by default"
    exit 1
else
    if [ -z $SUDO_USER ]; then
	# running directly as root, assume this is on the installation media
	# ideally we would set sudo_user to something read from the scm definitions but this is fine for my usage
	ON_INSTALL_MEDIA=t
	SUDO_USER=${USERNAME_TO_USE_IF_RUNNING_FROM_INSTALL_MEDIA}
    else
	# running in sudo, will skip the cow-store step which would fail and is unnecessary on normal guix system
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

# mimic the prompt from 'cryptsetup luksFormat' since we want to get conformation before rewriting the partition table
echo "WARNING: this operation will make all data on ${DRIVE} unrecoverable"
read -p "to continue type 'yes' in all capitals: " choice
if [[ ! $choice =~ ^YES$ ]]; then
   echo "did not get 'yes' in all capitals, aborting"
   exit 1
fi


echo "- Writing partition table"
# partition the drive, gpt partition table, one partition of ~300MiB for the EFI stuff
# and another partition to be encrypted to take up rest of drive.
# the esp flag indicates the first partition will hold EFI data
# grub is less than 20MB and linux kernel is similar, but 300 gives us enough to potentially have 2 or 3 options
# if setting up dedicated UKI stuff. Also running out of space there sucks real bad.
parted --script ${DRIVE} \
   mklabel gpt \
   mkpart efibooter fat16 1MiB 300MiB \
   mkpart guixroot btrfs 300MiB 100% \
   set 1 esp on

# let the dev folder get a moment to settle so that the subpartitions are consistently present
# by the time we check for them on the next line
udevadm settle

# List all devices matching the drive and its partitions
DEVICE_LIST=( $(ls ${DRIVE}* | sort) )

# Check count: root drive + 2 partitions = 3 entries where [0] is the root drive itself
# if we don't get the expected amount then exit
if [[ "${#DEVICE_LIST[@]}" -ne 3 || "${DEVICE_LIST[0]}" != "$DRIVE" ]]; then
    echo "Error: Expected 2 partitions and 1 root device for $DRIVE, found ${DEVICE_LIST}"
    exit 1
fi
# otherwise elements 1 and 2 are the 2 partitions we just created
EFI_PART=${DEVICE_LIST[1]}
ROOT_PART=${DEVICE_LIST[2]}
echo "- formatting efi partition ${EFI_PART}"

# format EFI partition for fat, -F32 is what the guix manual suggests
# but -F16 is needed if you want the partition to be as small as 30MiB
mkfs.fat -F16 ${EFI_PART}

echo "- making luks encryption keyfile"
# make keyfile early, this will be removed once the partition is mounted and the cpio file for early boot is setup
# so don't worry about it being written to the root directory, that is the only way I have figured out to get the cpio archive to work reliably.
dd bs=512 count=4 if=/dev/random of=/keyfile.bin iflag=fullblock
chmod -v 0400 /keyfile.bin # probably unnecessary but these flags get propogated into cpio file.


echo "- Encrypting drive and opening"
# setup the encryption for keyfile using second key slot so that the password uses first slot
# this is mainly because I suspect it makes grub work faster if the passphrase is in the first slot
# note that apparently with luks2 grub install just can't decrypt the drive? so using luks1
cryptsetup luksFormat --type luks1 -q ${ROOT_PART} /keyfile.bin --key-slot=1
# adds a password which will be kind of important to actually get into the drive once booted since the keyfile is going to live on the encrypted drive
# new-key-slot option doesn't exist on older models of cryptsetup, probably isn't necessary anyway
cryptsetup luksAddKey ${ROOT_PART} --key-file=/keyfile.bin #--new-key-slot=0
# will grab uuid now before opening causing this command to show 2 uuids
# note that technically we maybe should do the udev settle again since the UUID changes when the drive gets encrypted
# but because of the delay of user typing in password it is almost certainly fine.
ROOT_UUID="$(lsblk -o UUID ${ROOT_PART} -n)"
## TODO figure out how to catch errors and inform user how to unmount drives if something in the script goes wrong
# opens the drive, this will put it under /dev/mapper
cryptsetup open ${ROOT_PART} ${NEW_HOSTNAME}_drive --key-file=/keyfile.bin
echo "- drive opened to /dev/mapper/${NEW_HOSTNAME}_drive, Formatting as btrfs and mounting"
# format as btrfs, is fast and has compression options.
mkfs.btrfs /dev/mapper/${NEW_HOSTNAME}_drive

# mount the btrfs root at /mnt, will use very high compression for initial creation so files that are effectively never modified
# are highly compressed, including kernel and init scripts etc in guix store.
mount /dev/mapper/${NEW_HOSTNAME}_drive /mnt #--options=compress-force=zstd:7
# create the directory that the efi partition will be mounted to and then mount it (only needed for guix system init)
mkdir -p /mnt/boot/efi
mount ${EFI_PART} /mnt/boot/efi
# create several subvolumes for guix and nix store that will have different compression options
# the home directory that may be desirable to mount to other operating systems
# and the swap subvolume so the swap file doesn't interfere with any snapshotting stuff
# since we will lock down the swap subvolume we will also keep the keyfile in there
btrfs subvolume create /mnt/swap /mnt/gnu /mnt/nix /mnt/home
echo "- mounted under /mnt and created subvolumes, making swapfile and keyfile.cpio"
btrfs filesystem mkswapfile --size ${SWAP_SPACE} /mnt/swap/swapfile
##swapon /mnt/swap/swapfile
# now create the cpio file from the keyfile
# if on the installation image enable the cow-store service to copy files over as they are built
# do it here so that cpio that will be built if on the install media will be built after setting up cow-store
if [[ $ON_INSTALL_MEDIA =~ t ]]; then
    herd start cow-store /mnt
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! STARTED COWSTORE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    #guix pull --url="https://codeberg.org/guix/guix-mirror" --max-jobs=8
fi
# TODO: since cpio will likely need to be downloaded and this doesn't need to block other operations figure out how to do this in parallel with next steps, maybe with fork.
echo "/keyfile.bin" | guix shell cpio -- cpio -oH newc > /mnt/swap/keyfile.cpio
# now that the keyfile is encoded in the cpio archive it can be recaptured by running `cpio -iv < /swap/keyfile.cpio` (assuming the drive we are setting up is mounted as the root, otherwise add /mnt to the path) which will put the bin file back in the root directory.
# so we don't need the keyfile itself now
rm /keyfile.bin
# also set permissions to the keyfile to lock it down and also lock down the swap folder, no one should be poking around there
chmod -v 0400 /mnt/swap/keyfile.cpio
chmod -v 0700 /mnt/swap
# we assume the btrfs command already put the correct permissions on the swap file so no need to do that too.

echo "- partition is ready, writing system-info.scm"

# read some variables that will go into our config
RESUME_OFFSET="$(btrfs inspect-internal map-swapfile -r /mnt/swap/swapfile)"
EFI_UUID="$(lsblk -o UUID ${EFI_PART} -n)"
# TODO ideally we should get these directly to the new users home directory but doing that here would make their home folder owned by root
NEW_TMP_LOCATION_FOR_DOTFILES=/mnt/tmp/dotfiles
# copy this folder (assuming this is being run from the dotfiles folder) to the mount
# in order to setup system-info module there without touching the one in this folder
mkdir -p $NEW_TMP_LOCATION_FOR_DOTFILES
cp -r ./ $NEW_TMP_LOCATION_FOR_DOTFILES/
# echo relevent defines to fit into the os definition
# these would be copied to a file used by the operating system definition.
mkdir -p $NEW_TMP_LOCATION_FOR_DOTFILES/packages/system-info
cat > $NEW_TMP_LOCATION_FOR_DOTFILES/packages/system-info/details.scm <<EOF
(define-module (system-info details))
;; uuid of luks encrypted root partition
(define-public ROOT-UUID "${ROOT_UUID}")

;; uuid of efi boot partition
(define-public EFI-UUID "${EFI_UUID}")

;; the physical offset of swapfile as reported by "btrfs inspect-internal map-swapfile -r /swap/swapfile"
(define-public RESUME-OFFSET "${RESUME_OFFSET}")
;; hostname of machine
(define-public HOSTNAME "${NEW_HOSTNAME}")
;; main user of the machine
(define-public USERNAME "${SUDO_USER}")
EOF

# and copy the template system info to the setup file, the system-info is in .gitignore so will not be edited by updates to the repo
# so only this script (with above variables) and the template file need to be in sync (and the interface that os.scm depends on from it)
cp -p template-system-info.scm $NEW_TMP_LOCATION_FOR_DOTFILES/packages/system-info/setup.scm


echo "- details for drive saved to packages/system-info/ in current folder which has been copied to /mnt/home/${SUDO_USER}/src/dotfiles"
echo "  note that these will be owned by root, if you need to change them you can fix that but otherwise leaving them owned by root should be fine"

echo "- building system"
# do the system build first, then we will build the home config at the same time as copying over the system config
guix system build -L $NEW_TMP_LOCATION_FOR_DOTFILES/packages os.scm --max-jobs=8

echo "- system is built, copying over and building home config"
## TODO: doing the home building in parallel with system copying caused problems with fg not consistently waiting until both were done, figure out how to properly do parallel operations and re-enable this optimization
# NOTE: I would love to crank max-jobs as high as possible for the home build
# but if it is the one to start before the system init it is VITAL it doesn't use up ALL the job slots
# as that would force it to finish before the system init can begin which would totally defeat the purpose
# this order is preferable for the behaviour of fg below, so we can notify the user as soon as the system is copied and allow the home build to take longer if necessary
##guix home build -L /mnt/home/$SUDO_USER/src/dotfiles/packages home-config.scm --max-jobs=3 &
guix system init -L $NEW_TMP_LOCATION_FOR_DOTFILES/packages os.scm /mnt
echo "- system is initialized, you can interrupt this script and go use your system now if you wish, or wait for home config to be copied"
# we need to use fg to move the home build back to foreground so if the user interrupts it cancels the build, using wait would not give ideal results
##fg || echo "home finished building before system was copied over"
##echo "- home config is built and will now be copied over, note you will still need to do 'guix home reconfigure' from the booted system"
# there might be a way to capture the path from the process before to avoid needing to recompute the derivation again but it is not a huge time waste
##HOMECONFIGPATH=$(guix home build --no-substitutes -L /mnt/home/$SUDO_USER/src/dotfiles/packages home-config.scm)


# this uses the internal function copy-closure from the system init script to copy the path to the home config over to the drive
# for some reason the binding doesn't exist until the module is reloaded? might have something to do with autoload stuff
##guix repl <<EOF
##(reload-module (resolve-module (quote (guix scripts system))))
##(use-modules (guix store) (guix monads))
##(with-store store
## (run-with-store store
##   ((@@ (guix scripts system) copy-closure) "${HOMECONFIGPATH}" "/mnt")))
##EOF
##echo "- files needed for home config are copied over, you should be able to run 'guix home reconfigure' from booted system quickly"

echo "set a password for root user in new system"
passwd --root /mnt root
echo "set a password for ${SUDO_USER} in new system"
passwd --root /mnt ${SUDO_USER}

sync
echo "- FINISHED, unmounting drive"
##swapoff /mnt/swap/swapfile
umount -R /mnt
cryptsetup close ${NEW_HOSTNAME}_drive
