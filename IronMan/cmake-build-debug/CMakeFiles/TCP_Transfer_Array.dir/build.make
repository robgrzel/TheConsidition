# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.11

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /opt/cmake/v3.11/bin/cmake

# The command to remove a file.
RM = /opt/cmake/v3.11/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /mnt/e/W/TheConsidition/IronMan/TheConsidition

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /mnt/e/W/TheConsidition/IronMan/TheConsidition/cmake-build-debug

# Include any dependencies generated for this target.
include CMakeFiles/TCP_Transfer_Array.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/TCP_Transfer_Array.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/TCP_Transfer_Array.dir/flags.make

CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.o: CMakeFiles/TCP_Transfer_Array.dir/flags.make
CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.o: ../CppTests/tcp_transfer_array.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/mnt/e/W/TheConsidition/IronMan/TheConsidition/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.o"
	/usr/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.o -c /mnt/e/W/TheConsidition/IronMan/TheConsidition/CppTests/tcp_transfer_array.cpp

CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.i"
	/usr/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /mnt/e/W/TheConsidition/IronMan/TheConsidition/CppTests/tcp_transfer_array.cpp > CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.i

CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.s"
	/usr/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /mnt/e/W/TheConsidition/IronMan/TheConsidition/CppTests/tcp_transfer_array.cpp -o CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.s

# Object files for target TCP_Transfer_Array
TCP_Transfer_Array_OBJECTS = \
"CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.o"

# External object files for target TCP_Transfer_Array
TCP_Transfer_Array_EXTERNAL_OBJECTS =

TCP_Transfer_Array: CMakeFiles/TCP_Transfer_Array.dir/CppTests/tcp_transfer_array.cpp.o
TCP_Transfer_Array: CMakeFiles/TCP_Transfer_Array.dir/build.make
TCP_Transfer_Array: CMakeFiles/TCP_Transfer_Array.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/mnt/e/W/TheConsidition/IronMan/TheConsidition/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable TCP_Transfer_Array"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/TCP_Transfer_Array.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/TCP_Transfer_Array.dir/build: TCP_Transfer_Array

.PHONY : CMakeFiles/TCP_Transfer_Array.dir/build

CMakeFiles/TCP_Transfer_Array.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/TCP_Transfer_Array.dir/cmake_clean.cmake
.PHONY : CMakeFiles/TCP_Transfer_Array.dir/clean

CMakeFiles/TCP_Transfer_Array.dir/depend:
	cd /mnt/e/W/TheConsidition/IronMan/TheConsidition/cmake-build-debug && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /mnt/e/W/TheConsidition/IronMan/TheConsidition /mnt/e/W/TheConsidition/IronMan/TheConsidition /mnt/e/W/TheConsidition/IronMan/TheConsidition/cmake-build-debug /mnt/e/W/TheConsidition/IronMan/TheConsidition/cmake-build-debug /mnt/e/W/TheConsidition/IronMan/TheConsidition/cmake-build-debug/CMakeFiles/TCP_Transfer_Array.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/TCP_Transfer_Array.dir/depend

