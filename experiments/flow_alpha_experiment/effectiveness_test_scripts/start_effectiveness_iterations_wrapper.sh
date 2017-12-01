#!/bin/bash

while getopts 's:i:k:a:' OPTION ; do
    case "$OPTION" in
	s)
	    printf "Script:\t $OPTARG \n"
	    START_SCRIPT=$OPTARG;;
	i)
	    printf "Instance:\t $OPTARG \n"
	    INSTANCE=$OPTARG;;
	k)
	    printf "k:\t $OPTARG \n"
	    K=$OPTARG;;
	a)
	    printf "a:\t $OPTARG \n"
	    ALPHA=$OPTARG
    esac
done

if [ "x" == "x$START_SCRIPT" ]; then
  echo "-s [option] is required: start script"
  exit
fi

if [ "x" == "x$INSTANCE" ]; then
  echo "-i [option] is required: hypergraph instance"
  exit
fi

if [ "x" == "x$K" ]; then
  echo "-k [option] is required: block size"
  exit
fi

if [ "x" == "x$ALPHA" ]; then
  echo "-a [option] is required: max flow region size"
  exit
fi

#source common_setup.sh

GRAPH_NAME=`echo $INSTANCE | sed 's!.*/!!'`
rm -f *.results

###########################
# Experimental Setup
###########################
epsilon=0.03
###########################

#module unload compiler/gnu/4.9
#module unload devel/python/3.5.2
#module load lib/boost/1.55.0
#module load compiler/gnu/5.2
#module load devel/python/2.7.12

TOTAL_TIME=0.0
MAX_TIME=$(python max_graph_time.py $GRAPH_NAME $K)
for seed in `seq 1 100000`
do
    $START_SCRIPT $INSTANCE $K $epsilon $ALPHA $seed >> "$GRAPH_NAME.$K.$epsilon.$ALPHA.$seed.results"
	PARTITION_TIME=$(grep -oh 'totalPartitionTime=[^ ]*' "$GRAPH_NAME.$K.$epsilon.$ALPHA.$seed.results" | awk -F"=" '{print $2}')
	TOTAL_TIME=$(echo "$TOTAL_TIME + $PARTITION_TIME" | bc -l)
	if [ $(echo "if ( $TOTAL_TIME > $MAX_TIME ) 1 else 0" | bc -l) -eq 1 ]
	then 
		break
	fi
	REMAINING_PROB=$(echo "(($MAX_TIME - $TOTAL_TIME) / ($TOTAL_TIME / $seed) * 100.0)" | bc -l)
	RAND_INT=$(( RANDOM % 100 ))
	if [ $(echo "if ( $RAND_INT > $REMAINING_PROB) 1 else 0" | bc -l) -eq 1 ]
	then
		break
	fi
done
