target_string='{{TARGET_CFG}}'
base_dir='/home/mds/stow_config/dotfiles/'
target_dir="/home/mds/stow_config/$STWCFG_TARGET-dotfiles/"
build_dir="/home/mds/stow_config/build/"
rm -r $build_dir
mkdir $build_dir
cp -r $base_dir{.,}* $build_dir
cd $target_dir
while IFS= read -r -d '' dotfile; do
    echo "===" $dotfile "==="
    dotfile=${dotfile#./}

    base=$base_dir$dotfile
    target=$target_dir$dotfile
    dest=$build_dir$dotfile

    if test -n "$(find $base -type f -print -quit)"
    then
        echo "match found in base, resolving"
        grep_res=$(grep $target_string $base)
        target_contents=$(cat $target)
        if [[ -n $grep_res ]]
        then
            echo "found template, replacing with build stuffs"
            #find/replace file
            sed -i -e "s/$target_string/$target_contents/" $dest
        else
            echo "appending"
            #append file
            echo $target_contents >> $dest
        fi
    else
        echo "copying"
        # copy file
        mkdir -p $(dirname $dest)
        cp $target $dest 
    fi
done < <(find $target -type f -print0)
