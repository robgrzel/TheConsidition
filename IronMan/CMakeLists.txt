cmake_minimum_required(VERSION 3.11)
project(TheConsiditionTest)

set(CMAKE_CXX_STANDARD 17)

add_executable(TheConsiditionTest
        #Cpp/main.cpp
        #Cpp/objects.cpp
        #Cpp/objects.h
        CppTests/mapdraw.test.cpp
        )

target_link_libraries(TheConsiditionTest ncurses)

cmake_minimum_required(VERSION 3.11)
project(TheConsidition)

set(CMAKE_CXX_STANDARD 17)

add_executable(TheConsidition
        #Cpp/main.cpp
        #Cpp/objects.cpp
        #Cpp/objects.h
        Cpp/mapdraw.cpp Cpp/mapdraw.hpp
        Cpp/vector2d.cpp Cpp/vector2d.hpp
        Cpp/grafs.cpp Cpp/grafs.hpp

        )


target_link_libraries(TheConsidition ncurses)

###############################

cmake_minimum_required(VERSION 3.11)
project(NcursesWindowTest)

set(CMAKE_CXX_STANDARD 17)

add_executable(${PROJECT_NAME}
        CppTests/ncurses_window.cpp
        )

target_link_libraries(${PROJECT_NAME} ncurses)

############################


cmake_minimum_required(VERSION 3.11)
project(NcursesWindowTest1)

set(CMAKE_CXX_STANDARD 17)

add_executable(${PROJECT_NAME}
        CppTests/ncurses_window_1.cpp
        )

target_link_libraries(${PROJECT_NAME} ncurses)

############################



cmake_minimum_required(VERSION 3.11)
project(TCP_Transfer_Array)

set(CMAKE_CXX_STANDARD 17)

add_executable(${PROJECT_NAME}
        CppTests/tcp_transfer_array.cpp
        )

target_link_libraries(${PROJECT_NAME} ncurses)



############################

cmake_minimum_required(VERSION 3.11)
project(OpenCV_Skeletonize)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_BUILD_TYPE Release)

#INCLUDE(FindPkgConfig)

#set(OpenCV_FOUND 1)

#find_package(OpenCV 3.4 EXACT REQUIRED PATHS ${HOME}/bin/opencv/build/opencv-3.4 NO_DEFAULT_PATH)
#find_package(OpenCV REQUIRED)
#include_directories(/usr/include/opencv /usr/include/opencv2)

#message(`pkg-config --cflags --libs opencv`)
#message("${OpenCV_LIBS}")

add_executable(${PROJECT_NAME}
        CppTests/fast_hash.cpp
        CppTests/fast_hash.hpp)


#link_directories(/usr/share/opencv)
target_link_libraries(${PROJECT_NAME} ncurses)# ${OpenCV_LIBS})
############################

cmake_minimum_required(VERSION 3.11)
project(OpenCvTest)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_BUILD_TYPE Release)

INCLUDE(FindPkgConfig)

set(OpenCV_FOUND 1)


#find_package(OpenCV 3.4 EXACT REQUIRED PATHS ${HOME}/bin/opencv/build/opencv-3.4 NO_DEFAULT_PATH)
find_package(OpenCV REQUIRED)
include_directories(/usr/include/opencv /usr/include/opencv2)

#message(`pkg-config --cflags --libs opencv`)
message("${OpenCV_LIBS}")

add_executable(${PROJECT_NAME}
        CppTests/opencv.test.cpp
        CppTests/ncurses_window_1.cpp
        CppTests/draw_windows_2.cpp
        CppTests/astar_test.cpp

        )

link_directories(/usr/share/opencv)
target_link_libraries(${PROJECT_NAME} ncurses ${OpenCV_LIBS})



