#!/bin/bash

echo "This script must be run under sudo or as root"

export ERLANG_LIB_DIR='/usr/local/lib/erlang/lib'
export ERLANG_REL_DIR='/usr/local/lib/erlang/releases'
export CONF_DIR='/etc'
export BIN_DIR='/bin'
export LOG_DIR='/var/log/pranayama'


cp -R lib/*  $ERLANG_LIB_DIR/
ls -las $ERLANG_LIB_DIR | grep pranayama

cp -R releases/* $ERLANG_REL_DIR/
ls -las $ERLANG_REL_DIR | grep pranayama

cp pranayama.conf $CONF_DIR | grep pranayama
cp pranayama.sh $BIN_DIR | grep pranayama

mkdir $LOG_DIR
touch $LOG_DIR/pranayama.log
touch $LOG_DIR/pranayama_access.log


which pranayama.sh

echo "Pranayama was apparently installed." 
echo "We advise to run 'pranayama.sh start' then go to "
echo "http://localhost:8888/spy/ping. You should see a yellow screen with config info."
echo "If you do not see such a screen, then you may want to run 'pranayama.sh start_in_console'."
echo "And if you are still having trouble: 'pranayama.sh console' then, inside the console "
echo " type application:start(pranayama)."
echo "Finally, you can refer to http://code.google.com/p/pranayama"