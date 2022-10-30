#!/bin/bash

# Dynamically creates a dotfile for stow to use.

# the dotfile
cfg="${HOME}/.stow-cfg"

reinit=0

if [ -w "$cfg" ]; then
    echo "Setup already completed. Redo?"
    read -rn 1 -p "y/N: " ans
    printf "\n"
    if ! [[ "$ans" =~ ^[yY](es)*$ ]]; then
        echo "Ok, nothing to do."
        exit 0;
    else
        reinit=1
    fi
fi


# stow_config repo root
# stowdir=$(rlwrap -coe '' -S "stow_config root: " -P "$(pwd)" cat)
read -erp "stow_config root: " -i "$(pwd)" stowdir

if [ "$reinit" = "1" ]; then
    echo "Removing ${cfg}..."
    rm "${cfg}"
fi
echo "Creating ${cfg}..."
echo "export STOW_DIR=${stowdir}" > "$cfg";
echo "Done. source ${cfg} to and you're set";
