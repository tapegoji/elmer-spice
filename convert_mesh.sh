#!/bin/bash

# Check if the first argument is "parallel"
if [ "$1" == "-parallel" ]; then
    # Run ElmerGrid with partitioning and parallel options
    echo "Running ElmerGrid in parallel mode"
    ElmerGrid 14 2 case.msh -partdual -metiskway 2 -autoclean
elif [ "$1" == "" ]; then
    # Run ElmerGrid with the default serial option
    echo "Running ElmerGrid in serial mode"
    ElmerGrid 14 2 case.msh -autoclean
else
    echo "Invalid argument. Please use -parallel leave it empty for serial"
    exit 1
fi
