#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set parameter.wait_before_and_after_a_modifier_key_event 5
/bin/echo -n .
$cli set private.hyper_mode 1
/bin/echo -n .
$cli set private.khmr 1
/bin/echo -n .
$cli set private.left_hyper 1
/bin/echo -n .
$cli set private.right_meta 1
/bin/echo -n .
$cli set private.symbol_hyper 1
/bin/echo -n .
$cli set repeat.initial_wait 200
/bin/echo -n .
$cli set repeat.wait 33
/bin/echo -n .
/bin/echo
