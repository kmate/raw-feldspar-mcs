#!/bin/bash

cd $1

set -e

ESDK=${EPIPHANY_HOME}
ELIBS=${ESDK}/tools/host/lib
EINCS=${ESDK}/tools/host/include
ELDF=${ESDK}/bsps/current/fast.ldf

SCRIPT=$(readlink -f "$0")
EXEPATH=$(dirname "$SCRIPT")
cd $EXEPATH

CROSS_PREFIX=
case $(uname -p) in
	arm*)
		# Use native arm compiler (no cross prefix)
		CROSS_PREFIX=
		;;
	   *)
		# Use cross compiler
		CROSS_PREFIX="arm-linux-gnueabihf-"
		;;
esac

mkdir -p Debug

# Build HOST side application
${CROSS_PREFIX}gcc -std=gnu99 host.c -g -o Debug/host.elf -I ${EINCS} -L ${ELIBS} -le-hal -le-loader -I../ $2 -lpthread -DHOST

for f in core*.c
do
    ELF=Debug/${f%.*}.elf
    SREC=Debug/${f%.*}.srec

    # Build DEVICE side programs
	e-gcc -std=c99 -T ${ELDF} $f -g -o ${ELF} -le-lib -I../ $2
	# Convert ebinaries to SREC files
	e-objcopy --srec-forceS3 --output-target srec ${ELF} ${SREC}
done

cd ..

