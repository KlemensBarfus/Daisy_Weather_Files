module mod_check_avail_of_parameter

! checks the availability of a specific parameter in daisy weather file
! either filled by missing value or not available at all

contains

logical function check_avail_of_parameter(dwf,param,missing_value)
use mod_daisy_weather_type
use mod_find_string_in_array

implicit none

type(daisy_weather_type), intent(in):: dwf
character(100), intent(inout):: param
real, intent(in):: missing_value

integer:: i, i_times
real:: eps
logical:: found

eps = 0.000000001

i = find_string_in_array(dwf%n_vars, dwf%varnames, param)
if(i.lt.0)then
  check_avail_of_parameter = .false.
 else
  check_avail_of_parameter = .false. 
  found = .false.
  i_times = 1
  do 
    if(found.eqv.(.true.))exit
    if(abs(dwf%var(i_times,i)-missing_value).gt.eps)then
      check_avail_of_parameter = .true.
      found = .true. 
     else
      i_times = i_times + 1
    endif
    if(i_times.gt.dwf%n_times)then
      found = .true.
    endif
  enddo
endif   

end function

end module



