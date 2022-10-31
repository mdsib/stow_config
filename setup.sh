#!/bin/bash

# Dynamically creates a dotfile for stow to use.

# the dotfile
cfg="${HOME}/.stow-cfg"


# stow_config repo root
read -erp "stow_config root directory: " -i "$(pwd)" stow_dir
printf "%s\n" "Stow profile:"
select stow_profile in arch macos; do
    break
done
# read -erp "stow profile: " -i "arch" stow_dir -H



echo "Creating ${cfg}..."
echo "export STOW_DIR=${stow_dir}" > "$cfg"
echo "export STOW_PROFILE=${stow_profile}" >> "$cfg"

echo "Done. Run:
source ${cfg}
or start a new shell session to complete setup."
