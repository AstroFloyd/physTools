!> \file bike_power.f90  Estimate the power and energy used to bike a given distance with a given velocity

!***********************************************************************************************************************************
!> \brief  Estimate the power and energy used to bike a given distance with a given velocity
!!

program bike_power
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_statistics, only: median
  use SUFR_sports, only: cycling_power
  use SUFR_text, only: dbl2str
  use SUFR_time2string, only: hms
  use PT_general, only: physTools_init
  
  implicit none
  integer :: nArg
  real(double) :: mass, slope, speed,dist,time, Vair,Vground,  Pmech,Pair,Pclimb, Ptot, Ebike,Ebody
  
  call physTools_init(.true.)
  
  nArg = command_argument_count()
  if(nArg.lt.1 .or. nArg.gt.4) call syntax_quit('<speed (km/h)> [<distance (km)>] [<weight (kg)>] [slope (%)]', 0, &
       'Estimate the power and energy used to bike a given distance with a given velocity')  ! 0: Don't print "***  (STOP) 1"
  
  
  ! Default values:
  speed = 30.d0  ! km/h
  dist  = 50.d0  ! km
  mass  = 100    ! kg
  slope = 0.d0   ! %
  
  ! Read command-line variables:
  call get_command_argument_d(1, speed)
  if(nArg.ge.2) call get_command_argument_d(2, dist)
  if(nArg.ge.3) call get_command_argument_d(3, mass)
  if(nArg.ge.4) call get_command_argument_d(4, slope)
  slope = slope/100.d0  ! % -> fraction
  
  dist = dist * 1000     ! km -> m
  Vair = speed/3.6d0     ! km/h -> m/s
  Vground = speed/3.6d0  ! km/h -> m/s
  
  call cycling_power(mass, slope, Vair, Vground,  Pmech, Pair, Pclimb)
  Ptot = Pmech + Pair + Pclimb
  
  write(*,*)
  write(*,'(A,F7.1,A)')                ' Speed:             ', speed, ' km/h'
  if(nArg.ge.2) write(*,'(A,F7.1,A)')  ' Distance:          ', dist/1000.d0, ' km'
  write(*,'(A,F7.1,A)')                ' Mass:              ', mass, ' kg'
  write(*,'(A,F7.1,A)')                ' Slope:             ', slope*100, ' %'
  write(*,*)
  write(*,'(A,F7.1,A)')                ' Total power:       ', Ptot, ' W'
  write(*,'(A,F7.1,A)')                ' Mechanical power:  ', Pmech, ' W  ('//dbl2str(Pmech/Ptot*100,1)//'%)'
  write(*,'(A,F7.1,A)')                ' Air resistance:    ', Pair, ' W  ('//dbl2str(Pair/Ptot*100,1)//'%)'
  if(nArg.ge.4) write(*,'(A,F7.1,A)')                ' Climbing power:    ', Pclimb, ' W'
  write(*,*)
  
  if(nArg.ge.2) then  ! Have distance, can compute energy:
     time = dist/Vground/3600.d0
     Ebike = Ptot*dist/Vground/1000.d0    ! kJ
     Ebody = Ebike / (0.24d0 * 4.1868d0)  ! 24% efficient, kJ -> kcal
     write(*,'(A,A)')      ' Biking time:                      ', hms(time)
     write(*,'(A,F7.1,A)') ' Total energy provided to bike:  ', Ebike, ' kJ'
     write(*,'(A,F7.1,A)') ' Total energy provided to bike:  ', Ebody, ' kcal'
     write(*,*)
  end if
  
end program bike_power
!***********************************************************************************************************************************
