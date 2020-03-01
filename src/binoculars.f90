!> \file  binoculars.f90:  Properties of AxB binoculars

!***********************************************************************************************************************************
!> \brief  Properties of binoculars

program binoculars
  use SUFR_kinds, only: double
  use SUFR_constants, only: r2as
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_text, only: d2s
  use PT_general, only: physTools_init
  
  implicit none
  real(double) :: mag, diam, pup,expup, llost,lgain, theta
  
  call physTools_init(.true.)  ! Initialise physTools and libSUFR
  
  select case(command_argument_count())
  case(2)
     call get_command_argument_d(1, mag)
     call get_command_argument_d(2, diam)
  case default
     call syntax_quit('<magnification>  <diameter objective (mm)>', 0, 'Compute properties of "MxD" binoculars')
  end select
  
  pup = 7.d0       ! Average pupil for dark-adapted young adult ~7mm (5-9mm?), smaller for older people
  expup = diam/mag
  llost = (1.d0 - min((pup/expup)**2, 1.d0)) * 100
  lgain = (diam/pup)**2
  theta = 1.22d0 * 550d-9/(diam*1.d-3) * r2as  ! Angular resolution: 1.22 * lambda / D; rad -> arcseconds
  
  write(*,*)
  write(*,'(A,T30,A,A)') '  Magnification: ',       d2s(mag,1),' x'
  write(*,'(A,T30,A,A)') '  Diameter objective: ',  d2s(diam,1),' mm'
  write(*,'(A,T30,A,A)') '  Exit pupil: ',          d2s(expup,1),' mm'
  write(*,'(A,T30,A,A)') '  Light lost: ',          d2s(llost,1),' %'
  write(*,'(A,T30,A,A)') '  Light gain: ',          d2s(lgain,1),' x'
  write(*,'(A,T30,A,A)') '  Light gain: ',          d2s(2.5*log10(lgain),1),' mag'
  write(*,'(A,T30,A,A)') '  Angular resolution: ',  d2s(theta,1),'"'
  write(*,*)
  
end program binoculars
!***********************************************************************************************************************************

