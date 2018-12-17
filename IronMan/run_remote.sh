#!/bin/bash
set -x

export PROJECT_ROOT="/mnt/e/W/TheConsidition/IronMan/TheConsidition/cmake-build-debug"
export PROJECT_NAME="MyMpiCudaTest"
export PROJECT_BUILDDIR="cbuild"
export PROJECT_PATH=${PROJECT_ROOT}/${PROJECT_BUILDDIR}
export PROJECT_BUILDPATH=${PROJECT_ROOT}/${PROJECT_BUILDDIR}/${PROJECT_NAME}

export DISPLAY=L0:0.0

cd ${PROJECT_PATH}

sleep 2

chmod 755 ${PROJECT_NAME}
#${PROJECT_BUILDPATH}


mpirun -np 16 ${PROJECT_NAME}

sleep 10