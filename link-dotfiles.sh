#!/bin/bash

shopt -s globstar dotglob

tgt="$HOME"
dry=0
verbose=0

while getopts dv name
do
    case "$name" in
        d) dry=1;;
        v) verbose=1;;
        ?) printf "Usage: %s [-d] [-v]\n" "$0"
    esac
done

# check if we've set up
if [ ! -r "$STOW_DIR" ] || [ ! "$STOW_PROFILE" ]; then
    read -rn 1 -p "Run setup? " do_setup
    printf '\n'
    if [[ "$do_setup" =~ ^[yY]$ ]]; then
        ./setup.sh
        exit 0;
    else
        echo "You need to run the setup lol. Or fix something."
        exit 0;
    fi
fi

println() {
    printf "%s\n" "$@"
}

# dry run control
d() {
    if [ "$dry" -eq 0 ]; then
        v "Running command: $*"
        "$@"
    else
        println "DRY RUN: Running command: $*"
    fi
}

# verbose output
v() {
    if [ "$verbose" -eq 1 ]; then
        println "DEBUG: $*"
    fi
}


process() {
    local answer
    v "Processing ${1}"
    dst_file="${tgt}/${1#*/}"
    dst_dir=$(dirname "$dst_file")
    abs_origin="${STOW_DIR}/${1}"
    if ! [ -d "$dst_dir" ]; then
        d mkdir -p "$dst_dir"
    fi
    # now we try to make a symbolic link to the file, and ask what to do if it exists
    if [ -f "$dst_file" ] && [ "$(realpath -qe "$dst_file")" != "$abs_origin" ]; then
        while
            read -erp "
File ${dst_file} exists! What do you wanna do?
(V)iew the diff
(m)ove it to ${dst_file}.old
(s)kip it
(d)elete it
Your choice: " answer
            case "${answer:-v}" in
                v)
                    v "Diffing..."
                    less -R < <(println "Left is yours, right is stow_config's";
                                diff -s --color=always "$dst_file" "$abs_origin");;
                m)
                    v "Moving..."
                    d mv "$dst_file" "${dst_file}.old";;
                s)
                    v "Skipping...";;
                d)
                    v "Deleting..."
                    d rm "$dst_file";;
            esac
            [ "${answer:-v}" == "v" ]
        do true; done
    fi

    if [[ -f "$dst_file" && ! ("$dry" -eq 1 && "$answer" =~ ^[dm]$) ]]; then
        # already linked or explicitly skipped
        v "Skipping ${dst_file}"
    else
        v "Linking ${abs_origin} to ${dst_file}"
        d ln -s "$abs_origin" "$dst_file"
    fi
}

# TODO: allow multiple profiles and. figure out a way to combine them. zshrc-os => zshrc-{profile} ?
# and then loop through in there and source all profiles
for f in {,"${STOW_PROFILE}-"}dotfiles/**
do
    if [ -f "$f" ]; then
        process "$f"
    else
        v "Entering ${f}"
    fi

done
