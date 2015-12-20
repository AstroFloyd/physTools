!> \file functions.f90  General procedures for physTools


!  Copyright (c) 2009-2015  AstroFloyd - astrofloyd.org
!   
!  This file is part of the physTools package, 
!  see: http://phystools.sourceforge.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.



!***********************************************************************************************************************************
!> \brief  General procedures for physTools

module PT_general
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Initialise an physTools program
  !!
  !! \param banner  Print pT banner with version number (optional, default: true)
  
  subroutine physTools_init(banner)
    use SUFR_constants, only: set_SUFR_constants, stdOut
    use PT_version, only: print_physTools_version
    
    implicit none
    logical, intent(in), optional :: banner
    logical :: lbanner
    
    lbanner = .true.
    if(present(banner)) lbanner = banner
    
    ! Initialise libSUFR constants:
    call set_SUFR_constants()
    
    ! Print version:
    if(lbanner) then
       write(*,'(/,A)', advance='no') '  '
       call print_physTools_version(stdOut)
       write(*,*) ' -  phystools.sf.net'
    end if
    
  end subroutine physTools_init
  !*********************************************************************************************************************************
  
  
  
end module PT_general
!***********************************************************************************************************************************
