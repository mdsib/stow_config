#!/bin/bash
#
# this is a wrapper around stow that renames conflicting dotfiles in tgt_dir

if [ -z "$1" ]; then
	echo "USAGE: my-stow <stow-dir>"
	exit 1;
fi

# check if we've set up
if ! [ -r "$STOW_DIR" ]; then
	read -rn 1 -p "Run setup? " do_setup
	printf '\n'
	if [[ "$do_setup" =~ ^[yY]$ ]]; then
		./setup.sh
	else
		echo "Nothing to do."
		exit 0;
	fi
fi

# dotfiles go in the home dir
tgt_dir="$HOME"

function move_it() {
	local file="$1"
	local newloc="${file}.old"
	echo "Conflict: ${file} exists. Moving it to ${newloc}."
	mv "$file" "$newloc"
}
export -f move_it


echo "my_stow: creating symlinks from ${1} to ${tgt_dir}"
stow --verbose=2 --no --target="$tgt_dir" "$1" 2>&1 | sed -n "s/^CONFLICT.*\?:\ //p" | xargs -I '{}' bash -c "move_it ${tgt_dir}/'{}'"
stow --target="$tgt_dir" "$1"
