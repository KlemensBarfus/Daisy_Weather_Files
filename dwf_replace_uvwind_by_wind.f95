module mod_dwf_replace_uvwind_by_wind

contains

subroutine dwf_replace_uvwind_by_wind(dwf,missing_value)
use mod_daisy_weather_type
use mod_wind_conversion
use mod_find_string_in_array

implicit none 

type(daisy_weather_type), intent(inout):: dwf
real, intent(in):: missing_value

integer:: index_uwind, index_vwind, i_times
character(100):: search_string
real, dimension(:), allocatable:: dir, speed
logical:: valid
real:: eps, dir_temp, speed_temp

eps = 0.000001


search_string = "Vwind"
index_vwind = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)
search_string = "Uwind"
index_uwind = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)

if(allocated(dir)) deallocate(dir)
allocate(dir(1:dwf%n_times))
if(allocated(speed)) deallocate(speed)
allocate(speed(1:dwf%n_times))

do i_times = 1, dwf%n_times
  valid = .false.
  if(abs(dwf%var(i_times,index_uwind)-missing_value).gt.eps)then
    if(abs(dwf%var(i_times,index_vwind)-missing_value).gt.eps)then
      call uvwind_to_wind(dwf%var(i_times,index_uwind),dwf%var(i_times,index_vwind),speed_temp,dir_temp)
      speed(i_times) = speed_temp
      dir(i_times) = dir_temp
      valid = .true.
    endif
  endif
  if(valid.eqv.(.false.))then
    speed(i_times) = missing_value
    dir(i_times) = missing_value
  endif
enddo

dwf%varnames(index_uwind) = "Wind"
dwf%varunits(index_uwind) = "m/s"
dwf%var(1:dwf%n_times,index_uwind) = speed
dwf%varnames(index_vwind) = "Winddir"
dwf%varunits(index_vwind) = "m/s"
dwf%var(1:dwf%n_times,index_vwind) = dir

end subroutine

end module
