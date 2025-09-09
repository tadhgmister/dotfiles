#!/bin/sh

# this should probably not be run directly, the kernel name and initrd very much assume the direct parent directory is a guix store name
# and the kernel parameters are hard coded here

# determine root argument and other boot parameters from /run/current-system/parameters
PARAMS='root=/dev/mapper/tadhgfrmwrk_drive resume=/dev/mapper/tadhgfrmwrk_drive resume_offset=533760'
SPARAM='gnu.system=/var/guix/profiles/system gnu.load=/var/guix/profiles/system/boot'
KERNEL=$(realpath /run/current-system/kernel/bzImage)
INITRD=$(realpath /run/current-system/initrd)
KERNELNAME=$(basename $(dirname $KERNEL))-$(basename $KERNEL)
INITRDNAME=$(basename $(dirname $INITRD))-$(basename $INITRD)

ALLP="$PARAMS $SPARAM initrd=\EFI\\$INITRDNAME"
echo $ALLP
#sudo mount /dev/nvme0n1p3 /mnt
#sudo mkdir /mnt/EFI
#sudo cp $KERNEL /mnt/EFI/$KERNELNAME
#sudo cp $INITRD /mnt/EFI/$INITRDNAME

guix shell efibootmgr -- sudo efibootmgr --create --disk /dev/nvme0n1 --part 3 --label "guix rapid" --loader /EFI/$KERNELNAME --unicode "$ALLP"

#sudo umount /mnt
