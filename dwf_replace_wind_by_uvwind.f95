module mod_dwf_replace_wind_by_uvwind

contains

subroutine dwf_replace_wind_by_uvwind(dwf,missing_value)
use mod_daisy_weather_type
use mod_wind_conversion
use mod_find_string_in_array

implicit none 

type(daisy_weather_type), intent(inout):: dwf
real, intent(in):: missing_value

integer:: index_speed, index_dir, i_times
character(100):: search_string
real, dimension(:), allocatable:: uwind, vwind
logical:: valid
real:: eps, utemp, vtemp

eps = 0.000001


search_string = "Wind"
index_speed = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)
search_string = "Winddir"
index_dir = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)

if(allocated(uwind)) deallocate(uwind)
allocate(uwind(1:dwf%n_times))
if(allocated(vwind)) deallocate(vwind)
allocate(vwind(1:dwf%n_times))

do i_times = 1, dwf%n_times
  valid = .false.
  if(abs(dwf%var(i_times,index_speed)-missing_value).gt.eps)then
    if(abs(dwf%var(i_times,index_dir)-missing_value).gt.eps)then
      call wind_to_uvwind(dwf%var(i_times,index_speed),dwf%var(i_times,index_dir),utemp,vtemp)
      uwind(i_times) = utemp
      vwind(i_times) = vtemp
      valid = .true.
    endif
  endif
  if(valid.eqv.(.false.))then
    uwind(i_times) = missing_value
    vwind(i_times) = missing_value
  endif
enddo

dwf%varnames(index_speed) = "Uwind"
dwf%varunits(index_speed) = "m/s"
dwf%var(1:dwf%n_times,index_speed) = uwind
dwf%varnames(index_dir) = "Vwind"
dwf%varunits(index_dir) = "m/s"
dwf%var(1:dwf%n_times,index_dir) = vwind

end subroutine

end module
   


