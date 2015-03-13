
```
    #!/bin/bash
    #
    # This script contain the basic functionality you need to run
    # Yatsy. However, you'll have to add statements for calling Erlang
    # itself -- the basic outlines of these calls are included below but
    # YMMV.
    #
    # If you improve upon this script, don't hesitate to contact
    # me. Ideally its should be improved to work pretty much
    # out-of-the-box (well, except for your own _SUITE files, of course).
    #
    # I'd be interested making available a more complete test example,
    # source, tests, scripts and all. But right now, this will have to do.
    #
    # To integrate this with CruiseControl, please insert this into you
    # build script.
    #
    # /zrajm [2007-11-08]
    
    DATESTAMPED_SUBDIR="yatsy_output_$(date +%F_%H.%M.%S)"
    ERL_FLAGS="-pa $YATSY_TOP_DIR/test/shared/ebin"
    START_LOCAL=false
    RUN_FUNC=quick
    
    #
    # Setup Yatsy environment variables
    #
    export YATSY_TOP_DIR="$(pwd)"
    export YATSY_OUTPUT_DIR="$(pwd)/$DATESTAMPED_SUBDIR"
    export YATSY_CC_OUTPUT_DIR="$YATSY_OUTPUT_DIR"
    export YATSY_COVER_OUTPUT_DIR="$YATSY_OUTPUT_DIR/cover"
    export YATSY_TARGET_NODE="$(id -un)@$(hostname)"
    export YATSY_TARGET_DIR="$(pwd)"
    export YATSY_RUN_IN_REMOTE_NODE=true
    export YATSY_GENERATE_CC=false
    export YATSY_GENERATE_HTML=false
    export YATSY_GENERATE_COVER=false
    export YATSY_TEST_SENARIO=suite
    export YATSY_QUIT_WHEN_FINISHED=true
    export YATSY_INTERACTIVE=false
    export YATSY_YAWS_HOST="$(hostname)"
    # Unset Yatsy environment variables:
    #   YATSY_YAWS_PORT
    #   YATSY_YAWS_LISTEN
    #   YATSY_EMAIL
    #   YATSY_COVER_CALLBACK_MODULE
    
    
    help()
    {
    cat <<-EOF
    	Usage: ${0##*/} [OPTION...]
    	Runs a series of _SUITE.erl tests through Yatsy.
    
    	  -cc                 generate CruiseControl output on disk
    	  -cover              generate code cover reports on disk
    	  -b                  do not run in the target node
    	  -ebin PATH          add PATH beginning of Erlang's path
                                  (add path to Yatsy's ebins this way)
    	  -email ADDRESS      email address, receiving error reports
    	  -html               generate HTML on disk
    	  -i                  run interactive
    	  -m                  start test cases manually
    	  -node USER@HOST     run test cases on Erlang node USER@HOST
    	  -path PATH          PATH to top directory (default = pwd)
    	  -r PATH             where to put output directory
    	  -s                  start local target
    	  -t NAME             set test senario
    	  -X                  don't exit when finished
    
    	EOF
        exit 1
    }
    
    
    
    while [[ $# -gt 0 ]]; do
      arg="$1"; shift
      case $arg in
          -node) YATSY_TARGET_NODE="$1"
    	     shift;;
          -ebin)
    	  if [[ ! -z "$1" ]]; then
    	      ERL_FLAGS="$ERL_FLAGS -pa $1"
    	      shift
    	  fi ;;
          -email)
    	  if [[ ! -z "$1" ]]; then
    	      export YATSY_EMAIL="$1"
    	      shift
    	  fi ;;
          -b)     YATSY_RUN_IN_REMOTE_NODE=false ;;
          -cc)    YATSY_GENERATE_CC=true ;;
          -cover) YATSY_GENERATE_COVER=true ;;
          -html)  YATSY_GENERATE_HTML=true ;;
          -i)     YATSY_INTERACTIVE=true ;;
          -m)     RUN_FUNC=start ;;
          -X)     YATSY_QUIT_WHEN_FINISHED=false ;;
          -r)     YATSY_OUTPUT_DIR="$1/$DATESTAMPED_SUBDIR"
    	      YATSY_CC_OUTPUT_DIR="$YATSY_OUTPUT_DIR"
    	      YATSY_COVER_OUTPUT_DIR="$YATSY_OUTPUT_DIR/cover"
    	      shift ;;
          -t)     YATSY_TEST_SENARIO="$1"
    	      shift ;;
          -s)     START_LOCAL=true ;;
          -k)     YATSY_TOP_DIR="$1"
                  shift ;;
          *)      help ;;
      esac
    done
    
    
    init () {
        rm -rf $YATSY_OUTPUT_DIR
        mkdir -p $YATSY_OUTPUT_DIR
    }
    
    end () {
        rm -f yatsy_output_current
        ln -fs ${YATSY_OUTPUT_DIR##*/} yatsy_output_current
    }
    
    start_local_target () {
        echo == Starting $YATSY_TARGET_NODE ==
        #erl -detached \
        #    -risky_shell \
        #    -pa PATHS_TO_ALL_EBINS \
        #     $ERL_FLAGS \
        #    -boot BOOTFILE \
        #    -sname ${YATSY_TARGET_NODE%%@*} \
        #    -connect_all false \
        #    -config CONFIGFILE \
        #    -s MODULE
        sleep 1
    }
    
    stop_local_target () {
        echo == Stopping $YATSY_TARGET_NODE ==
        #erl -noshell \
        #    -sname SOMETHINGUNIQUE \
        #    -connect_all false \
        #    -pa MODULE_PATH \
        #    -s MODULE STOPFUNC $YATSY_TARGET_NODE
        sleep 1
    }
    
    
    do_test() {
        init
        [[ "$START_LOCAL" == true ]] && start_local_target
        if [[ "$YATSY_INTERACTIVE" == true ]]; then
            #erl  $ERL_FLAGS \
            #    -pa PATHS_TO_ALL_EBINS \
            #    -sname SOMETHINGUNIQUE \
            #    -connect_all false \
            #    -s yatsy $RUN_FUNC
        else
            #erl  $ERL_FLAGS \
            #    -pa PATHS_TO_ALL_EBINS \
            #    -sname SOMETHINGUNIQUE \
            #    -connect_all false \
            #    -s yatsy $RUN_FUNC \
            #    -noshell \
            #    -detach
        fi
        [[ "$START_LOCAL" == true ]] && stop_local_target
        end
    }
    
    do_test
    
    #[[eof]]
```