program dwf_generate_LARSWG_files
use mod_daisy_weather_type
use mod_read_daisy_file
use mod_find_string_in_array

implicit none

! purpose: generates LARS-WG files from Daisy weather files
character(100):: path_in, filename_daisy
type(daisy_weather_type)::d
integer:: len_temp
character(100), dimension(:), allocatable:: vars
character(100):: station, filename_temp, filename_site, filename_data
character(100):: len_temp_str, fmt_string, n_var_str, fmt_string_temp, search_string
integer, dimension(:), allocatable:: index_vars
integer:: i_vars, n_vars, i_temp, i_times, i_rad
real, dimension(:), allocatable:: temp_data
real:: missing_value, eps

n_vars = 4
if(allocated(vars)) deallocate(vars)
allocate(vars(1:n_vars))
vars = (/'T_min  ','T_max  ','Precip ','GlobRad'/)

missing_value = -99.0
eps = 0.0001


station = 'Freising_1995_2016_2'
path_in = 'c:\Users\Klemens\Projects\Sabines_Data\Freising\'
filename_daisy = trim(adjustl(station))//'.dwf'
filename_temp = trim(adjustl(path_in))//trim(adjustl(filename_daisy))
call read_daisy_file(filename_temp,d)

if(allocated(index_vars)) deallocate(index_vars)
allocate(index_vars(1:n_vars))

! conversion of Radiation W/m^2 -> J/(m^2*day)
search_string = 'GlobRad'
i_rad = find_string_in_array(d%n_vars, d%varnames, search_string)
do i_times = 1, d%n_times
  if(abs(d%var(i_times,i_rad)-missing_value).gt.eps)then
    d%var(i_times,i_rad) = d%var(i_times,i_rad) / 11.574
  endif
enddo  


do i_vars = 1, n_vars
  i_temp = find_string_in_array(d%n_vars, d%varnames, vars(i_vars))
  index_vars(i_vars) = i_temp
enddo  


! site file
filename_site = trim(adjustl(path_in))//trim(adjustl(station))//'.st'
filename_data = trim(adjustl(path_in))//trim(adjustl(station))//'.dat'
open(1, file=filename_site)
write(1,fmt='(A6)') '[SITE]'
len_temp = len(trim(adjustl(station)))
write(len_temp_str, '(i10)' ) len_temp
fmt_string = '(A'//trim(adjustl(len_temp_str))//')'
write(1, fmt=fmt_string) station
write(1,fmt='(A18)') '[LAT, LON and ALT]'
write(1, fmt='(F5.1,1x,F6.1,1x,I4)') d%latitude, d%longitude, floor(d%elevation)
write(1,fmt='(A15)') '[WEATHER FILES]'
len_temp = len(trim(adjustl(filename_data)))
write(len_temp_str, '(i10)' ) len_temp
fmt_string = '(A'//trim(adjustl(len_temp_str))//')'
write(1, fmt=fmt_string) filename_data
write(1,fmt='(A8)') '[FORMAT]'
write(1,fmt='(A31)') 'YEAR MONTH DAY MIN MAX RAIN RAD'
write(1,fmt='(A5)') '[END]'
close(1)

! data file
if(allocated(temp_data)) deallocate(temp_data)
allocate(temp_data(1:n_vars))
open(1, file=filename_data)
write(n_var_str, '(i10)' ) n_vars-1
fmt_string_temp = trim(adjustl(n_var_str))//'(F7.3,1x),F7.3)'
fmt_string = '(I4,1x,I2,1x,I2,1x,'//trim(adjustl(fmt_string_temp))
do i_times = 1, d%n_times
  ! resort data
  do i_vars = 1, n_vars
    temp_data(i_vars) = d%var(i_times,index_vars(i_vars))
  enddo  
  write(1,fmt=fmt_string) d%year(i_times), d%month(i_times), d%day(i_times), temp_data
enddo
close(1)
 



end program dwf_generate_LARSWG_files
