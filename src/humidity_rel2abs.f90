!> \file humidity.f90  Calculates the absolute humidity from the relative humidity and temperature, 
!!

!***********************************************************************************************************************************
!> \brief  Calculates the absolute humidity from the relative humidity and temperature

program humidity
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_text, only: d2s
  use SUFR_weather, only: water_vapor_saturated_density
  use PT_general, only: physTools_init
  
  implicit none
  real(double) :: temp,RH,sat,AH
  
  call physTools_init(.true.)  ! Initialise physTools and libSUFR
  if(command_argument_count().ne.2)  call syntax_quit('<T(C)> <RH(%)>', 0, &
       'Calculate the absolute humidity (water-vapor density) as a function of temperature (degrees C) and relative humidity (%).')
  
  call get_command_argument_d(1, temp)
  call get_command_argument_d(2, RH)
  
  sat = water_vapor_saturated_density(temp)  ! Saturated wator-vapor density in air for the given temperature in deg C
  
  if(RH.ge.1.d0) RH=RH/100.d0  ! Assume is percentage - want fraction
  AH  = sat*RH
  
  write(*,'(/,A)') '  Saturated vapor density at '//d2s(temp,1)//' deg C:      '//d2s(sat,2)//' g/m3'
  write(*,'(A,/)') '  Absolute water vapor density at RH='//d2s(RH*100,1)//'%:   '//d2s(AH,2)//' g/m3'
  
end program humidity
!***********************************************************************************************************************************
      
      
      
