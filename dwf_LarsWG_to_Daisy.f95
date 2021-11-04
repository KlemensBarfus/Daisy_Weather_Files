program dwf_LarsWG_to_Daisy
use mod_strsplit
use mod_daisy_weather_type
use mod_find_string_in_array
use mod_get_daisy_temp_larswg
use mod_write_daisy_file
use mod_read_daisy_file

implicit none

! reads simulated LarsWG file
! calculates average temperature
! adds relHum and WindSpeed

type(daisy_weather_type):: d
character(100):: station, path_in, filename_LarsWG_adds, path_daisy, filename_daisy_old
character(100):: filename_daisy_new, search_string
character(150):: temp_str
character(100):: filename_LarsWG_data
character(1):: delimiter
character(100), dimension(:), allocatable:: varnames_lars
integer:: i, k, n_times
integer, dimension(1:12):: days_per_month
integer, dimension(:), allocatable:: year_lars, day_lars
real, dimension(:,:), allocatable:: var_lars
integer:: i_vars_lars, n_vars_lars, i_times, i_vars
integer:: index_t_min_lars, index_t_max_lars, index_precip_lars, index_rad_lars
!character(100), dimension(:), allocatable:: vars_lars
logical:: found
integer:: j
integer:: index_relhum_daisy, index_t_max_daisy, index_t_min_daisy, index_t_mean_daisy
integer:: index_rad_daisy, index_wind_daisy, index_precip_daisy
real:: missing_value, TAverage, TAmplitude
integer:: doy_max



! read LarsWG-File
station = 'Freising_1995_2016'
path_in = 'c:\Users\Klemens\Projects\Sabines_Data\Freising\'
filename_LarsWG_adds = trim(adjustl(path_in))//trim(adjustl(station))//'WG.st'
path_daisy = 'c:\Users\Klemens\Projects\Sabines_Data\Freising\'
filename_daisy_old = trim(adjustl(path_daisy))//trim(adjustl(station))//'_1.dwf'
filename_daisy_new = trim(adjustl(path_daisy))//trim(adjustl(station))//'_3.dwf'

missing_value = -99.0

open(1, file=filename_LarsWG_adds)
! read header data
do i = 1, 7
  read(1, fmt='(A100)') temp_str
enddo
! read format data
read(1, fmt='(A100)') temp_str
delimiter = ' '
call strsplit(temp_str,delimiter)
n_vars_lars = n_substring-2
if(allocated(varnames_lars)) deallocate(varnames_lars)
allocate(varnames_lars(1:n_vars_lars))
varnames_lars = substring(3:n_vars_lars+2)
close(1)
!! read data
filename_LarsWG_data = trim(adjustl(path_in))//trim(adjustl(station))//'WG.dat' 
open (1, file=filename_LarsWG_data)
n_times = 0
k = 1
do
  if(k.lt.0)exit
  read(1, fmt='(A100)', iostat=k) temp_str
  n_times = n_times + 1
enddo
n_times = n_times - 1
rewind(1)
if(allocated(year_lars)) deallocate(year_lars)
allocate(year_lars(1:n_times))
if(allocated(day_lars)) deallocate(day_lars)
allocate(day_lars(1:n_times))
if(allocated(var_lars)) deallocate(var_lars)
allocate(var_lars(1:n_times,1:n_vars_lars))
do i_times = 1, n_times
  if(i_times.eq.2033)then
    print *, 'stop'
  endif  
  read(1, fmt='(A100)') temp_str
  delimiter = ' '
  call strsplit(temp_str,delimiter)
  read(substring(1), '(I4)' ) year_lars(i_times)
  read(substring(2), '(I3)' ) day_lars(i_times)
  do i_vars_lars = 1, n_vars_lars
    read(substring(2+i_vars_lars), '(F6.2)' ) var_lars(i_times,i_vars_lars)
  enddo
enddo
close(1)

! generate daisy weather type
days_per_month = (/31,59,90,120,151,181,212,243,273,304,334,365/)
! read daisy file
call read_daisy_file(filename_daisy_old,d)
! relate indexes
!  RelHum T_Max T_Min AirTemp GlobRad Wind Precip
search_string = 'MIN'
index_t_min_lars = find_string_in_array(n_vars_lars, varnames_lars, search_string)
search_string = 'MAX'
index_t_max_lars = find_string_in_array(n_vars_lars, varnames_lars, search_string)
search_string = 'RAIN'
index_precip_lars = find_string_in_array(n_vars_lars, varnames_lars, search_string)
search_string = 'RAD'
index_rad_lars = find_string_in_array(n_vars_lars, varnames_lars, search_string)
! daisy indices
search_string = 'RelHum'
index_relhum_daisy = find_string_in_array(d%n_vars, d%varnames, search_string)
search_string = 'T_max'
index_t_max_daisy = find_string_in_array(d%n_vars, d%varnames, search_string)
search_string = 'T_min'
index_t_min_daisy = find_string_in_array(d%n_vars, d%varnames, search_string)
search_string = 'AirTemp'
index_t_mean_daisy = find_string_in_array(d%n_vars, d%varnames, search_string)
search_string = 'GlobRad'
index_rad_daisy = find_string_in_array(d%n_vars, d%varnames, search_string)
search_string = 'Wind'
index_wind_daisy = find_string_in_array(d%n_vars, d%varnames, search_string)
search_string = 'Precip'
index_precip_daisy = find_string_in_array(d%n_vars, d%varnames, search_string)

d%n_times = n_times
if(allocated(d%year)) deallocate(d%year)
allocate(d%year(1:d%n_times))
if(allocated(d%month)) deallocate(d%month)
allocate(d%month(1:d%n_times))
if(allocated(d%day)) deallocate(d%day)
allocate(d%day(1:d%n_times))
if(allocated(d%var)) deallocate(d%var)
allocate(d%var(1:d%n_times,1:d%n_vars))
! run through timesteps and assign values
do i_times = 1, d%n_times
  if(i_times.eq.2033)then
    print *, 'stop'
  endif
  d%year(i_times) = year_lars(i_times)
  ! get month
  found = .false.
  j = 1
  do
    if(found.eq.(.true.))exit
    if(day_lars(i_times).le.days_per_month(j))then
      d%month(i_times) = j
      if(j.eq.1)then
        d%day(i_times) = day_lars(i_times)
       else
        d%day(i_times) = day_lars(i_times) - days_per_month(j-1)
      endif
      found = .true.
     else
      j = j + 1
    endif
  enddo
  d%var(i_times,index_relhum_daisy) = missing_value
  d%var(i_times,index_t_max_daisy) = var_lars(i_times,index_t_max_lars)
  d%var(i_times,index_t_min_daisy) = var_lars(i_times,index_t_min_lars)
  d%var(i_times,index_t_mean_daisy) = (var_lars(i_times,index_t_max_lars) + var_lars(i_times,index_t_min_lars)) / 2.0
  d%var(i_times,index_rad_daisy) = var_lars(i_times,index_rad_lars) * 11.574
  d%var(i_times,index_wind_daisy) = missing_value
  d%var(i_times,index_precip_daisy) = var_lars(i_times,index_precip_lars)
enddo
! get start and stop dates
d%start_year = d%year(1)
d%start_month = d%month(1)
d%start_day = d%day(1)
d%stop_year = d%year(d%n_times)
d%stop_month = d%month(d%n_times)
d%stop_day = d%day(d%n_times)
! calculate temperatures
call get_daisy_temp_larswg(d%n_times,d%year,d%month,d%day,d%var(1:d%n_times,index_t_mean_daisy),missing_value,&
           TAverage,doy_max,TAmplitude)
d%TAverage = TAverage
d%TAmplitude = TAmplitude
d%MaxTDay = doy_max

! write daisy file
call write_daisy_file(filename_daisy_new,d)

print *, 'fertig'

end program dwf_LarsWG_to_Daisy
