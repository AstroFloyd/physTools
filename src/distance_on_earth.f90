!> \file distance_on_earth.f90  Calculate the distance between two points on the Earth's globe


!***********************************************************************************************************************************
!> \brief   Calculate the distance between two points on the Earth's globe
!!
program distance_on_earth
  use SUFR_kinds, only: double
  use SUFR_constants, only: d2r
  use SUFR_angles, only: rev2, asep
  use SUFR_angle2string, only: dmss,dmss2
  use PT_general, only: physTools_init
  
  implicit none
  real(double) :: a,fl,x1,x2,x3,l1,l2,b1,b2,  f,g,l,s,c,o,r,d,h1,h2,dist
  
  call physTools_init(.true.)  ! Initialise physTools and libSUFR
  
  a   = 6378.14d0              ! Earth's radius in km
  fl  = 1.d0/298.257d0         ! Earth's flattening
  
  
  write(*,'(/,A)') "  This program calculates the distance between two points on the Earth's globe in degrees and kilometres."
  
  write(*,'(/,A)') '  Location 1:'
  write(*,'(A)', advance='no') '    Please give the longitude (d,m,s):  '
  read*,x1,x2,x3
  l1 = rev2((abs(x1) + abs(x2)/60.d0 + abs(x3)/3600.d0)*d2r)
  if(min(x1,x2,x3).lt.0) l1 = -l1
  
  write(*,'(A)', advance='no') '    Please give the latitude (d,m,s):   '
  read*,x1,x2,x3
  b1 = rev2((abs(x1) + abs(x2)/60.d0 + abs(x3)/3600.d0)*d2r)
  if(min(x1,x2,x3).lt.0) b1 = -b1
  
  
  write(*,'(/,A)') '  Location 2:'
  write(*,'(A)', advance='no') '    Please give the longitude (d,m,s):  '
  read*,x1,x2,x3
  l2 = rev2((abs(x1) + abs(x2)/60.d0 + abs(x3)/3600.d0)*d2r)
  if(min(x1,x2,x3).lt.0) l2 = -l2
  
  write(*,'(A)', advance='no') '    Please give the latitude (d,m,s):   '
  read*,x1,x2,x3
  b2 = rev2((abs(x1) + abs(x2)/60.d0 + abs(x3)/3600.d0)*d2r)
  if(min(x1,x2,x3).lt.0) b2 = -b2
  
  
  
  f = (b1+b2)*0.5d0
  g = (b1-b2)*0.5d0
  l = (l1-l2)*0.5d0
  
  s = sin(g)**2*cos(l)**2 + cos(f)**2*sin(l)**2
  c = cos(g)**2*cos(l)**2 + sin(f)**2*sin(l)**2
  o = atan2(sqrt(s),sqrt(c))
  r = sqrt(s*c)/o
  d = 2*o*a
  h1 = (3*r-1)/(2*c)
  h2 = (3*r+1)/(2*s)
  
  dist = d*(1.d0 + fl*h1*sin(f)**2*cos(g)**2 - fl*h2*cos(f)**2*sin(g)**2)
  
  
  write(*,'(//)')
  write(*,'(A13,2A20)') 'Location 1:',dmss2(l1),dmss2(b1)
  write(*,'(A13,2A20)') 'Location 2:',dmss2(l2),dmss2(b2)
  write(*,*)
  write(*,'(A13,2A20)') 'Difference:',dmss(dabs(l2-l1)),dmss(dabs(b2-b1))
  write(*,'(A13,A20)') 'Separation:',dmss(asep(l1,l2,b1,b2))
  write(*,*)
  
  if(dist.gt.0.999d0) then
     write(*,'(A13,F20.3,A4,F5.3,A3)') 'Distance:  ',dist,' +- ',dist*fl**2,'km'
  else
     write(*,'(A13,F20.3,A4,F5.3,A3)') 'Distance:  ',dist*1000,' +- ',dist*fl**2*1000,'m'
  end if
  write(*,'(37x,A)') '(the error is on the order of 10^-5)'
  write(*,'(//)')
  
  
  
end program distance_on_earth
!***********************************************************************************************************************************

