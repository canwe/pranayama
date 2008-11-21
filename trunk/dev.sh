#!/bin/bash



function console {
	
	compile
    headline "About to start a Console for Pranayama ..."
    erl -sname pranayama -pa ./ebin -boot start_sasl  -config priv/log.config
}



function check {

    clean
    runtests    
    xref
}

function compile_dev_script {

	erlc -o ./ebin ./src/pranayama_dev.erl;
	
}


function compile  {
    clean
	compile_dev_script
    headline "Compiling ..."	
    erl -pa ./ebin  -run pranayama_dev compile -noshell -s erlang halt;    
    ls -las ./ebin
    headline "... Done"
}

function compile_before_testing  {
    clean
	compile_dev_script
    headline "Compiling (before Testing) ..."
    erl -pa ./ebin  -run pranayama_dev compile_for_tests -noshell -s erlang halt;    
    ls -las ./ebin
    headline "... Done"
}

# We never remove the pranayama_dev.beam as it is used for this script. 
function clean {
    headline "Cleaning ..."
    rm -f ./ebin/*.beam 
    headline "... done."
}

function create_normal_release {
    
    compile
    makeboot
    doc
    headline "Creating normal release ..."
    erl -pa ./ebin  -run pranayama_dev create_normal_release -noshell -s erlang halt;    
    headline "... done."
    ls -las ./releases/normal
    mkdir ./releases/normal/tmp
    cp  ./releases/normal/*.tar.gz ./releases/normal/tmp
    cp ./etc/install.sh ./releases/normal/tmp
    cp  ./etc/pranayama.conf.rel ./releases/normal/tmp/pranayama.conf
    cp  ./etc/pranayama.sh ./releases/normal/tmp
    cp  ./README ./releases/normal/tmp
    cd ./releases/normal/tmp
    tar xvfz *.tar.gz
    rm *.tar.gz
    chmod a+x install.sh
    tar cvfz PranayamaRelease.tar.gz *
    headline "It looks like we prepared the next release. Please check ./releases/normal/tmp. You can run install.sh if you wish, but read it first."
    
}



function dialyze {
    clean
    compile
    headline "Dialyzing ..."
    dialyzer --build_plt  -c ./ebin/*.beam
    headline "... done."
}


function doc {
    headline "Refreshing edocs ..."
    erl -pa ./ebin  -run pranayama_dev doc -noshell -s erlang halt;    
    ls -las ./doc/edoc;
    headline "... done."
}

function headline {
    
    echo "= $1 ==============================================================="

    }

function makeboot  {
    compile
    headline "Refreshing boot files ..."
    erl -pa ./ebin  -run pranayama_dev makeboot -noshell -s erlang halt;   
    ls -las ./etc;
    headline "... done."
}

function printhelp  {

    echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
    echo "@ Pranayama Development                                                                     @"
    echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
    echo ""
    echo "Please use one of the following commands:"
    echo ""
    echo "console: Enter a Console where you can manually run Pranayama using application:start(pranayama)."
    echo ""
    echo "check: Runs all of the tests (after a clean compile) then does an xref check."
    echo ""
    echo "clean: Removes all beam files from ./ebin (except pranayama_dev)."
    echo ""
    echo "compile: Compiles all in ./src."
    echo ""
    echo "create_normal_release: Creates a normal release (non-standalone) in releases/normal."
    echo "                     : Please note: you must temporarily change the conf filename in "
    echo "                       ebin/pranayama.app to /etc/pranayama.conf." 
    echo "" 
    echo "dialyze: Cleans, recompiles, then runs Dialyzer for all beams."
    echo ""    
    echo "doc: Creates edocs from all files in ./src and locates them in ./doc/edoc."
    echo ""    
    echo "makeboot: Makes the boot files based on the .rel file in ./etc."
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
    echo ""
    echo "test: Cleans, compiles all *for testing* and runs test_pranayama."
    echo ""
}


function runtests  {
    compile_before_testing
    headline "Running tests ..."
    erl -pa ./ebin  -run pranayama_dev test -noshell -s erlang halt;    
    headline "... done."
}


function spy {

    headline "About to start a Console to spy on Pranayama ..."
    headline "Remember to use ctrl-g then q to exit *not* q() which will kill the node."
    erl -sname spy_on_pranayama -remsh pranayama@`hostname`
    headline "Exited Console"
    status
}

function start_in_console {
    makeboot
    headline "About to start Pranayama in the Console ..."
    erl -sname pranayama -pa ./ebin  -boot etc/Pranayama-0.1.0 -config priv/log.config
}

function start {
    makeboot
    headline "About to start Pranayama  ..."
    erl -detached -sname pranayama -pa ./ebin  -boot etc/Pranayama-0.1.0 -config priv/log.config
    status
}

function status {
    headline "If you see an erlexec process here, Pranayama is running, otherwise it isn't."
    ps -eax | grep 'snampranayama'
}

function stop {
    
    headline "About to *stop* Pranayama  ..."
    erl -sname stop_pranayama -noshell  -eval 'rpc:call(list_to_atom("pranayama@" ++ net_adm:localhost()),init,stop,[]).' -s init stop
    headline " stopped."
    status
}



function xref  {
    headline "Running xref ..."
    erl -pa ./ebin  -run pranayama_dev xref -noshell -s erlang halt;    
    headline "... done."
}



if  [ "$1" = "check" ]; then
    check
elif  [ "$1" = "console" ]; then
    console
elif  [ "$1" = "clean" ]; then
    clean
elif [ "$1" = "compile" ]; then
    compile
elif [ "$1" = "create_normal_release" ]; then
    create_normal_release
elif [ "$1" = "create_standalone_release" ]; then
    create_standalone_release
elif [ "$1" = "dialyze" ]; then
    dialyze
elif [ "$1" = "doc" ]; then
    doc
elif [ "$1" = "test" ]; then
    runtests
elif [ "$1" = "makeboot" ]; then
    makeboot
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
elif [ "$1" = "xref" ]; then
    xref
elif [ "$1" = "" ]; then
    printhelp
else
    headline "Hmmm .. what did you want to do?"
    printhelp
fi


