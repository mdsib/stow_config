#!/usr/bin/env bash

# globstar for recursive globs
# dotglob for all the hidden files
shopt -s globstar dotglob

# tgt: where the dotfiles are moved to
tgt="$HOME"
# dry: 1 means don't run any commands that make changes
dry=0
# verbose: ...
verbose=0
# forced action: what to automatically do, if anything, when a conflict happens
forced_action="none"

# i COULD add replace here
while getopts dvbs name
do
    case "$name" in
        d) dry=1;;
        v) verbose=$((verbose + 1));;
        b) forced_action="backup";;
        s) forced_action="skip";;
        ?) printf "Usage: %s [options]
   -d     dry run
   -v     verbose
   -vv    more verbose
   -vvv   too verbose
   -b     backup conflicting files
   -s     skip and don't link conflicting files
" "$0"; exit 1;;
    esac
done

# check if we've set up, and conveniently ask to do the setup if not.
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

# because echo is bad because a dash - in var interpreted as arg
# see https://github.com/anordal/shellharden/blob/master/how_to_do_things_safely_in_bash.md#echo--printf
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

# verbosity
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
    vvv "Processing ${1}"

    # what to do if there's a conflict
    # none means no conflict
    local action="none"
    # where the symlink should go
    local dst_file="${tgt}/${1#*/}"
    # absolute path of current dotfile
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

        # use the flagged action if there is one
        if [ "$forced_action" != "none" ]; then
            vv "Forcing action: $forced_action"
            action="$forced_action"
        fi

        # goad user (aka myself in the future) into making a decision
        while [ "$action" == "none" ]
        do
            printf "%s" "
File ${dst_file} exists! What do you wanna do?
(D)iff it
(b)ack it up
(s)kip it
(r)eplace it
Your choice: "
            read -r choice
            case "${choice:-v}" in
                d|D)
                    v "Diffing..."
                    less -R < <(println "Left is yours, right is stow_config's";
                                diff -s --color=always "$dst_file" "$abs_origin")
                    ;;
                b|B)
                    action="backup"
                    ;;
                s|S)
                    action="skip"
                    ;;
                r|R)
                    action="replace"
                    ;;
                *)
                    println "Invalid choice."
                    ;;
            esac
        done
    fi

    # skip: no op
    # backup: ln -sb
    # replace: ln -sf
    # none
    #   file exists: no op
    #   no file exists: ln -s
    case "${action}" in
        backup)
            v "Backing up and linking ${abs_origin} to ${dst_file}"
            d ln -s --backup=numbered "$abs_origin" "$dst_file"
            ;;
        skip)
            v "${dst_file} exists. Skipping..."
            ;;
        replace)
            v "Forcibly linking ${abs_origin} to ${dst_file}"
            d ln -sf "$abs_origin" "$dst_file"
            ;;
        none)
            if [ -f "$dst_file" ]; then
                vvv "${dst_file} already linked. Moving on."
            else
                v "Linking ${abs_origin} to ${dst_file}"
                d ln -s "$abs_origin" "$dst_file"
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
