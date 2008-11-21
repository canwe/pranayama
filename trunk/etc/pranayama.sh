#!/bin/bash

export LIB_DIR='/usr/local/lib/erlang/lib/pranayama-0.1.0'
# Do not include the .boot suffix
export BOOT_FILE='/usr/local/lib/erlang/releases/Pranayama_0.1.0/start'
export LOG_CONFIG_FILE='/usr/local/lib/erlang/lib/pranayama-0.1.0/priv/log.config'


function console {

    headline "About to start a Console for Pranayama ..."
    erl -sname pranayama -pa $LIB_DIR/ebin -boot start_sasl  -config $LOG_CONFIG_FILE
}

function spy {

    headline "About to start a Console to spy on Pranayama ..."
    headline "Remember to use ctrl-g then q to exit *not* q() which will kill the node."
    erl -sname snoop_pranayama -remsh pranayama@`hostname`
    headline "Exited Console"
    status
}

function start_in_console {

    headline "About to start Pranayama in the Console ..."
    erl -sname pranayama -pa $LIB_DIR/ebin  -boot $BOOT_FILE -config $LOG_CONFIG_FILE
}

function start {

    headline "About to start Pranayama  ..."
    erl -detached -sname pranayama -pa $LIB_DIR/ebin  -boot $BOOT_FILE -config $LOG_CONFIG_FILE
    status
}

function status {
    headline "Waiting a few seconds to let things settle ..."
    sleep 3
    headline "If you see an erlexec process here, Pranayama is running, otherwise it isn't."
    ps -eax | grep "sname pranayama"
}

function stop {
    
    headline "About to *stop* Pranayama  ..."
    erl -sname stop_pranayama -noshell  -eval 'rpc:call(list_to_atom("pranayama@" ++ net_adm:localhost()),init,stop,[]).' -s init stop
    headline " stopped."
    status
}

function headline {
    
    echo "= $1 ==============================================================="

}

function printhelp  {

    echo "*********************************************************************************************"
    echo "* Pranayama (Production)                                                                        *"
    echo "*********************************************************************************************"
    echo ""
    echo "Please use one of the following commands:"
    echo ""
    echo "console: Enter a Console where you can manually run Pranayama using application:start(pranayama)."
    echo ""
    echo "spy: Presumes that Pranayama in already running as a daemon, as attaches a console to "
    echo "       its node. Be careful not to quit out of it using q(): use job control instead."
    echo ""
    echo "start_in_console: Starts Pranayama inside the Erlang console."
    echo ""
    echo "start: Starts Pranayama in the background"
    echo ""
    echo "status: Indicates whether or not Pranayama is running."
    echo ""
    echo "stop: Immediately stops Pranayama, presuming that it is already running as a daemon."
}


if  [ "$1" = "console" ]; then
    console
elif  [ "$1" = "spy" ]; then
    spy
elif  [ "$1" = "start" ]; then
    start
elif  [ "$1" = "start_in_console" ]; then
    start_in_console
elif  [ "$1" = "status" ]; then
    status
elif  [ "$1" = "stop" ]; then
    stop
else
    headline "Hmmm .. what did you want to do?"
    printhelp
fi