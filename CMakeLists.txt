##
##  CMakeFile for physTools
##  AF, 2015-04-20
##
##  Copyright 2007-2015 AstroFloyd - astrofloyd.org
##  
##  This file is part of the physTools package,
##  see: http://phystools.sf.net/
##  
##  physTools is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  physTools is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with physTools.  If not, see <http://www.gnu.org/licenses/>.
##
##  To install this software on your system, see the INSTALL file
##  in the root or doc/ directory of this package.
##


cmake_minimum_required( VERSION 2.8 )

# Set build type. Do this *before* we set the project name:
if( NOT CMAKE_BUILD_TYPE )
  set( CMAKE_BUILD_TYPE Release CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo Profile."
    FORCE )
endif( NOT CMAKE_BUILD_TYPE )
set( CMAKE_CONFIGURATION_TYPES "${CMAKE_BUILD_TYPE}" CACHE INTERNAL "internal" )


# Project name and language:
project( physTools Fortran )


# Search in the CMake/ directory for CMake modules:
list( APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake )


# Various compile/optimisation options that we may want to enable:
include( SetCompileOptions )


# Get the compiler name (for compiler flags and to find libraries):
get_filename_component( Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME )



# Find dependencies:
find_package( LibSUFR REQUIRED )
set( INCLUDE_FLAGS "-I${LibSUFR_INCLUDES}" )  # will be transferred to CompilerFlags



# Set FORTRAN compiler flags:
include( CompilerFlags_Fortran )


# Create the file code_version.f90, which contains the code version number/hash and date:
if( NOT EXISTS ${CMAKE_SOURCE_DIR}/src/code_version.f90 OR CREATE_VERSION )
  # Code version generator:
  add_custom_command(
    OUTPUT ${CMAKE_SOURCE_DIR}/src/code_version.f90
    COMMAND cd $(CMAKE_SOURCE_DIR)
    COMMAND . ${CMAKE_SOURCE_DIR}/code_version.sh $(CMAKE_SOURCE_DIR) src/code_version.f90 ${Fortran_COMPILER_NAME} ${OPT_FLAGS}
    )
  
  # Tell CMake the source won't be available until build time:
  set_source_files_properties( ${CMAKE_SOURCE_DIR}/src/code_version.f90 PROPERTIES GENERATED 1 )
endif( NOT EXISTS ${CMAKE_SOURCE_DIR}/src/code_version.f90 OR CREATE_VERSION )


# Put common source files in a static library, and link it to the other object files:
add_library( "Pt"  STATIC  src/functions.f90 src/code_version.f90 )       # Creates libPt.a
target_link_libraries( Pt  ${LibSUFR_LIBRARIES} )  # link libSUFR to libPt.a


# List of programs to compile:
set( PT_PROGRAMS 
  bike_power binoculars dewpoint distance_on_earth heartrate2power humidity_rel2abs wind_average
  )


# For each program, specify the binary name (pt_PROGRAM), source name (src/PROGRAM.f90), and compile and link the code:
foreach( program ${PT_PROGRAMS} )
  add_executable( pt_${program} "src/${program}.f90")
  target_link_libraries( pt_${program}  ${LibSUFR_LIBRARIES} Pt )
  set( PT_BINARIES ${PT_BINARIES} pt_${program} )          # For install()
endforeach( program )


# Install the binaries:
install( TARGETS  ${PT_BINARIES}  RUNTIME DESTINATION bin )

