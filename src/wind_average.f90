!> \file wind_average.f90  Determine the average wind velocity and direction from a 
!!                         number of values of velocity and direction.


!***********************************************************************************************************************************
!> \brief Determine the average wind velocity and direction from a number of values of 
!!        velocity and direction.

program wind_average
  use SUFR_kinds, only: double
  use SUFR_system, only: quit_program_error
  use SUFR_constants, only: d2r,r2d
  use SUFR_text, only: uppercase
  use SUFR_angles, only: wdstr_ens, rev
  use PT_general, only: physTools_init
  
  implicit none
  integer :: Nw,Iw,WDi,Wi, status
  real(double) :: wv,wd, wvx,wvy,wvabs
  real(double), allocatable :: wvs(:),wds(:)
  character :: wdstr*(9),wvstr*(9), wds_en(0:15)*(3),wds_nl(0:15)*(3)
  logical :: wdmatch
  
  call physTools_init(.true.)  ! Initialise physTools and libSUFR
  wds_en = (/'N  ','NNE','NE ','ENE','E  ','ESE','SE ','SSE','S  ','SSW','SW ','WSW','W  ','WNW','NW ','NNW'/)
  wds_nl = (/'N  ','NNO','NO ','ONO','O  ','OZO','ZO ','ZZO','Z  ','ZZW','ZW ','WZW','W  ','WNW','NW ','NNW'/)
  
  
  write(*,'(/,A)', advance='no') '  Enter the number of data points (wind direction, velocity pairs) to average:  '
  read(*,*, iostat=status) Nw
  if(status.ne.0) call quit_program_error('I expected a number', 0)
  if(Nw.lt.2) call quit_program_error('Averaging only makes sense when using multiple data points...', 0)
  
  allocate(wvs(Nw), wds(Nw))
  
  write(*,'(/,A,/)') '  Enter the wind direction (N,SSE, or 0=N,90=E) and velocity (in arbitraty units):'
  
  do Iw=1,Nw
     write(*,'(2x,A,I0,A)', advance='no') 'Data point ',Iw, ' (dir,vel):  '
     read(*,*) wdstr,wvstr
     
     read(wvstr,*, iostat=status) wv
     if(status.ne.0) call quit_program_error('Wind velocity should be a number', 0)
     wdstr = uppercase(wdstr)
     
     ! Try to match wdstr to a WD string (N, SSE) + Dutch (Z, ZZO)
     wdmatch = .false.
     do WDi=0,15
        if(trim(wdstr).eq.trim(wds_en(WDi)) .or. trim(wdstr).eq.trim(wds_nl(WDi))) then
           wd = dble(WDi)/16.d0*360.d0
           wdmatch = .true.
           exit
        end if
     end do
     status = 0
     if(.not. wdmatch) read(wdstr,*, iostat=status) wd  ! Try to read a value from the string
     if(status.ne.0) call quit_program_error('I could not determine the wind direction', 0)
     
     wvs(Iw) = wv
     wds(Iw) = wd
  end do  ! Iw
  
  
  ! Average:
  wvx = 0.d0  ! Average wind vector
  wvy = 0.d0  ! Average wind vector
  wvabs = 0.d0  ! Average absolute wind velocity
  
  write(*,'(/,A)') '  Input provided:'
  write(*,'(A4,A13,A10)') '#','Direction','Velocity'
  do Wi=1,Nw
     write(*,'(I4,F8.1,A5,F10.2)') Wi,wds(Wi),wdstr_ens(wds(Wi)*d2r),wvs(Wi)  ! Reiterate input
     wvx = wvx + wvs(Wi)*cos(wds(Wi)*d2r)
     wvy = wvy + wvs(Wi)*sin(wds(Wi)*d2r)
     wvabs = wvabs + abs(wvs(Wi))
  end do
  wvx = wvx/dble(Nw)
  wvy = wvy/dble(Nw)
  wvabs = wvabs/dble(Nw)
  
  wv = sqrt(wvx**2+wvy**2)
  wd = rev(atan2(wvy,wvx))*r2d
  
  
  write(*,'(/,A,F8.2,A5,F10.2)', advance='no') '  Average wind vector:            ',wd,wdstr_ens(wd*d2r),wv
  if(abs(wv).lt.1.d-9) write(*,'(A)', advance='no') ',  i.e. the average wind direction cannot be determined since the vector = 0'
  write(*,'(/,A,13x,F10.2,/)')     '  Average absolute wind velocity: ',wvabs
  
end program wind_average
!***********************************************************************************************************************************

