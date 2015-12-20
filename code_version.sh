#!/bin/sh

##  code_version.sh:
##  Write the git hash or release-version number of the code and the compiler name to a Fortran source file
##  
##  2010-07-24, AF: initial version for AnalyseMCMC, svn
##  2011-10-06, AF: svn -> bzr
##  2011-11-09, AF: generate 2 files: PG/PLplot; use bzr rev.no or release version
##  2012-04-26, AF: ported to libSUFR
##  2012-11-25, AF: bzr -> git
##  2015-03-29, AF: ported to astroTools
##  2015-12-05, AF: ported to statTools
##  2015-12-20, AF: ported to physTools
##  
##  Copyright (c) 2009-2015 AstroFloyd - astrofloyd.org
##  
##  This file is part of the physTools package, 
##  see: http://phystools.sf.net/
##   
##  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
##  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
##  
##  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License along with this code.  If not, see 
##  <http://www.gnu.org/licenses/>.


if [[ ${#} -ne 2 && ${#} -ne 4 ]]; then
    
    echo -e "\n  Syntax:"
    echo -e "    code_version.sh  <CMake base dir>  <f90 output file>  <Fortran compiler name>  <Compiler flags>"
    echo -e "    code_version.sh  <CMake base dir>  <f90 output file>  # For release versions\n"

else
    
    BASEDIR=${1}                        # CMake base dir
    F90FILE=${1}/${2}                   # Fortran-90 output file
    
    RELEASE='no'
    if [[ ${#} -eq 2 ]]; then
	RELEASE='yes'                   # RELEASE (true if "yes")
    fi
    
    if [[ ${#} -eq 4 ]]; then
	COMPILER=${3}                   # Compiler name
	COMPILER_FLAGS=${4}             # Compiler flags
    fi
    
    
    
    
    
    cd ${BASEDIR}
    
    if [[ ! -e .git/  && -e ${F90FILE} ]]
    then
	echo "${F90FILE} already exists, no need to create it"
	exit 0
    else
	if [ ${RELEASE} == 'yes' ]; then
	    echo "Generating release-version of ${F90FILE}"
	else
	    echo "Generating ${F90FILE}"
	fi
    fi
    
    
    echo "!> \file code_version.f90  Source file generated by CMake to report the physTools version used" > ${F90FILE}
    echo "" >> ${F90FILE}
    
    echo "!***********************************************************************************************************************************" >> ${F90FILE}
    echo "!> \brief  Report physTools version number" >> ${F90FILE}
    echo "" >> ${F90FILE}
    echo "module PT_version" >> ${F90FILE}
    echo "  implicit none" >> ${F90FILE}
    echo "  save" >> ${F90FILE}
    echo "  " >> ${F90FILE}
    echo "contains" >> ${F90FILE}
    echo "  " >> ${F90FILE}
    echo "  " >> ${F90FILE}
    
    
    echo "  !*********************************************************************************************************************************" >> ${F90FILE}
    echo "  !> \brief  Subroutine generated by CMake to report the code version used" >> ${F90FILE}
    echo "  " >> ${F90FILE}
    echo "  subroutine print_physTools_version(unit)" >> ${F90FILE}
    echo "    implicit none" >> ${F90FILE}
    echo "    integer, intent(in) :: unit" >> ${F90FILE}
    if [ ${RELEASE} == 'yes' ]; then
	echo "    character :: physTools_version*(99) = 'v"`cat .version`"'" >> ${F90FILE}
	echo "    character :: release_date*(99) = '"`date +"%F"`"'" >> ${F90FILE}
	
	echo "    " >> ${F90FILE}
	echo "    write(unit,'(A)', advance='no') 'physTools '//trim(physTools_version)//' ('//trim(release_date)//')'" >> ${F90FILE}
    else
	if [ -e .git/ ]; then  # Prefer revision number over release number
	    echo "    character :: physTools_version*(99) = 'rev."`git rev-list --abbrev-commit HEAD | wc -l`", hash "`git log --pretty="%h (%ad)" --date=short -n1`"'" >> ${F90FILE}
	#echo "  character :: code_version*(99) = 'rev."`git rev-list --abbrev-commit HEAD | wc -l`", "`git describe --tags`" "`git log --pretty="(%ad)" --date=short -n1`"'" >> ${F90FILE}  # Doesn't work on Mac OS(?)
	elif [ -e .bzr/ ]; then  # Prefer bzr revision number over release number
	    echo "    character :: physTools_version*(99) = 'revision "`bzr revno`"'" >> ${F90FILE}
	elif [ -e VERSION ]; then
	    echo "    character :: physTools_version*(99) = 'v"`grep 'Release version' VERSION | awk '{print $3}'`"'" >> ${F90FILE}
	elif [ -e doc/VERSION ]; then
	    echo "    character :: physTools_version*(99) = 'v"`grep 'Release version' doc/VERSION | awk '{print $3}'`"'" >> ${F90FILE}
	else
	    echo "    character :: physTools_version*(99) = '(unknown version)'" >> ${F90FILE}
	fi
	
	echo "    character :: compile_date*(99) = '"`date`"'" >> ${F90FILE}
	echo "    character :: compiler*(99) = '"${COMPILER}"'" >> ${F90FILE}
	echo "    character :: compiler_flags*(99) = '"${COMPILER_FLAGS}"'" >> ${F90FILE}
	
	echo "    " >> ${F90FILE}
	echo "    write(unit,'(A)', advance='no') 'physTools '//trim(physTools_version)//', compiled on '//trim(compile_date)//' with '// &" >> ${F90FILE}
	echo "         trim(compiler)//' '//trim(compiler_flags)" >> ${F90FILE}
    fi
    echo "    " >> ${F90FILE}
    echo "  end subroutine print_physTools_version" >> ${F90FILE}
    echo "  !*********************************************************************************************************************************" >> ${F90FILE}
    
  
    echo "  " >> ${F90FILE}
    echo "end module PT_version" >> ${F90FILE}
    echo "!***********************************************************************************************************************************" >> ${F90FILE}
    echo "" >> ${F90FILE}
    
    
    # touch -d doesn't work on FreeBSD
    #touch -d "1 Jan 2001" ${F90FILE}            # Make the file look old
    
fi


