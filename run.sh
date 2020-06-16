#~/system/bin/bash

SOURCE_TREE=`pwd`
ERLANG_DIR=erlang
JAVA_DIR=java
ERLANG_OUT_DIR=build/erlang/
JAVA_OUT_DIR=build/java/

show_usage()
{
  echo "Usage : run.sh [-e, -E, -r, -j, -J, -R, -a, -A, -Z]";
  echo "    -e : build erlang code";
  echo "    -E : run erlang code";
  echo "    -r : build and run erlang code";
  echo "    -j : build java code";
  echo "    -J : run java code";
  echo "    -R : build and run java code";
  echo "    -a : build erlang and java code";
  echo "    -A : run erlang and java code";
  echo "    -Z : build and run java and erlang code";
  echo ""
}

build_erlang()
{
  cd $ERLANG_DIR || exit;
	echo "####################################";
	echo "Building exchange erlang application";
	echo "####################################";

	echo "Compiling calling and exchange module module";
	erlc *.erl;
	cd "$SOURCE_TREE" || exit;
}

run_erlang()
{
  cd $ERLANG_DIR || exit;
	echo "Running exchange application using erlang";
	erl -noshell -s exchange start -s init stop;
	echo "Done running exchange application using erlang";
	cd "$SOURCE_TREE" || exit;
}

build_java()
{
	echo "####################################";
	echo "Building exchange erlang application";
	echo "####################################";
}

preapare_build()
{
  if [ ! -d $ERLANG_OUT_DIR ]; then
    mkdir -p $ERLANG_OUT_DIR;
  fi

  if [ ! -d $JAVA_OUT_DIR ]; then
    mkdir -p $JAVA_OUT_DIR
  fi
}


# initialize variables
doBuildErlang=0
doBuildJAVA=0
doRunErlang=0
doRunJAVA=0

while getopts "h?eErRjJaAZ" opt; do
  case "$opt" in
  h|\?)
    show_usage;
    exit 0;
    ;;
  e)
    doBuildErlang=1;
    ;;
  E)
    doRunErlang=1;
    ;;
  r)
    doBuildErlang=1;
    doRunErlang=1;
    ;;
  j)
    doBuildJava=1;
    ;;
  J)
    doRunJAVA=1;
    ;;
  R)
    doBuildJava=1;
    doRunJAVA=1;
    ;;
  a)
    doBuildJava=1;
    doBuildErlang=1;
    ;;
  A)
    doRunJAVA=1;
    doRunErlang=1;
    ;;
  Z)
    doBuildErlang=1;
    doBuildJava=1;
    doRunErlang=1;
    doRunJAVA=1;
    ;;
  *)
    echo "type -h for info";
    exit 1;
    ;;
  esac
done

if [ "$doBuildErlang" -eq 1 ]; then
    build_erlang;
fi

if [ "$doBuildJAVA" -eq 1 ]; then
    build_java;
fi

if [ "$doRunErlang" -eq 1 ]; then
    run_erlang;
fi

if [ "$doRunJAVA" -eq 1 ]; then
    run_java;
fi