#!/usr/bin/env bash

shopt -s globstar dotglob

tgt="$HOME"
dry=0
verbose=0
forced_action=""

while getopts dvms name
do
    case "$name" in
        d) dry=1;;
        v) verbose=$((verbose + 1));;
        m) forced_action="m";;
        s) forced_action="s";;
        ?) printf "Usage: %s [options]
   -d     dry run
   -v     verbose
   -vv    more verbose
   -vvv   too verbose
   -m     move conflicts to <...>.old
   -s     skip conflicts, keeping original file
" "$0"; exit 1;;
    esac
done

# check if we've set up
if [ ! -r "$STOW_DIR" ] || [ ! "$STOW_PROFILE" ]; then
    read -rn 1 -p "Run setup? " do_setup
    printf '\n'
    if [[ "$do_setup" =~ ^[yY]$ ]]; then
        ./setup.sh
        exit 0
    else
        echo "You need to run the setup lol. Or fix something."
        exit 0
    fi
fi

println() {
    printf "%s\n" "$@"
}

# dry run control
d() {
    if [ "$dry" -eq 0 ]; then
        vv "Running command: $*"
        "$@"
    else
        println "DRY RUN: Running command: $*"
    fi
}

# verbose output
v() {
    if [ "$verbose" -ge 1 ]; then
        println "INFO: $*"
    fi
}

vv() {
    if [ "$verbose" -ge 2 ]; then
        println "DEBUG: $*"
    fi
}

vvv() {
    if [ "$verbose" -ge 3 ]; then
        println "NOISE: $*"
    fi
}

mk_lnk() {
    v "Linking ${abs_origin} to ${dst_file}"
    d ln -s "$abs_origin" "$dst_file"
}

process() {
    local action=""
    vvv "Processing ${1}"

    local dst_file="${tgt}/${1#*/}"
    local abs_origin="${STOW_DIR}/${1}"

    # make dir
    local dst_dir
    dst_dir=$(dirname "$dst_file")
    if ! [ -d "$dst_dir" ]; then
        d mkdir -p "$dst_dir"
    fi

    # check if we have a conflict (file exists and isn't a symlink to stow_config)
    if [ -f "$dst_file" ] && [ "$(realpath -qe "$dst_file")" != "$abs_origin" ]; then
        v "Conflict!"
        if [ "$forced_action" != "" ]; then
            vv "Forcing action: $forced_action"
        fi
        # use the flagged action if there is one
        action="$forced_action"

        while [ ! "$action" = "m" ] && [ ! "$action" = "s" ]
        do
            printf "%s" "
File ${dst_file} exists! What do you wanna do?
(V)iew the diff
(m)ove it to ${dst_file}.old
(s)kip it
Your choice: "
            read -r action
            if [ "$action" = "" ]; then
                action="v"
            fi
            case "${action}" in
                v)
                    v "Diffing..."
                    less -R < <(println "Left is yours, right is stow_config's";
                                diff -s --color=always "$dst_file" "$abs_origin")
                    ;;
                m|s)
                    ;;
                *)
                    println "Invalid choice."
                    ;;
            esac
        done
    fi

    # conflict
    #   skip: no op
    #   move: move, then link
    # no conflict
    #   file exists: no op
    #   no file exists: link

    case "${action}" in
        m)
            v "Moving ${dst_file} -> ${dst_file}.old"
            d mv "$dst_file" "${dst_file}.old"
            mk_lnk
            ;;
        s)
            v "${dst_file} exists. Skipping..."
            ;;
        "")
            if [ -f $dst_file ]; then
                vvv "${dst_file} already linked. Moving on."
            else
                mk_lnk
            fi
            ;;
    esac
}

# TODO: allow multiple profiles and. figure out a way to combine them. zshrc-os => zshrc-{profile} ?
# and then loop through in there and source all profiles
for f in {,"${STOW_PROFILE}-"}dotfiles/**
do
    if [ -f "$f" ]; then
        process "$f"
    else
        vv "Entering ${f}"
    fi

done

println "Done!"
exit 0
