!> \file dewpoint.f90  Calculates the dew point from the relative humidity and temperature
!! 
!! \see http://en.wikipedia.org/wiki/Dew_point

!***********************************************************************************************************************************
program dewpoint
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_text, only: d2s
  use SUFR_weather, only: dew_point
  use PT_general, only: physTools_init
  
  implicit none
  real(double) :: temp,RH,dp
  
  call physTools_init(.true.)  ! Initialise physTools and libSUFR
  
  if(command_argument_count().ne.2)  call syntax_quit('<T(C)> <RH(%)>', 0, &
       'Calculate the dew point from the temperature and relative humidity')
  
  call get_command_argument_d(1, temp)
  call get_command_argument_d(2, RH)
  
  if(RH.ge.1.d0) RH = RH/100.d0  ! Assume relative humidity is in percent; convert to fraction
  
  dp = dew_point(temp, RH)  ! Temp in C, RH as fraction, dp in degrees C
  
  write(*,'(/,A)')    '  Temperature:        '//d2s(temp,1)//' degrees C'
  write(*,'(A,I0,A)') '  Relative humidity:  ',nint(RH*100),' %'
  write(*,'(/,A,/)')  '  Dew point:          '//d2s(dp,1)//' degrees C'
  
end program dewpoint
!***********************************************************************************************************************************
