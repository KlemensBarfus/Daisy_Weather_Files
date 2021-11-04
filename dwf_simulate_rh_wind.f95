program dwf_simulate_rh_wind
use mod_daisy_weather_type
use mod_read_daisy_file
use mod_find_string_in_array
use mod_write_daisy_file
use mod_get_index_time_period
use mod_get_daisy_temp_larswg
use mod_analog_approach

implicit none

! replaces missing values in daisy file by the analog approach

character(100):: path_in, filename_daisy_in, filename_daisy_out
character(100):: filename_temp, filename_daisy_ref
type(daisy_weather_type):: d, ref
integer:: start_year_final, start_month_final, start_day_final
integer:: stop_year_final, stop_month_final, stop_day_final
real:: missing_value
integer:: n_predictors
character(100), dimension(1:4):: predictor_names
character(100):: predictand, search_string
integer, dimension(:), allocatable:: year_new, month_new, day_new
real, dimension(:,:),allocatable:: var_new
integer:: i_times, index_temp, index_temperature
real:: TAverage,TAmplitude
integer:: doy_max

start_year_final = 1
start_month_final = 1
start_day_final = 1
stop_year_final = 500
stop_month_final = 12
stop_day_final = 31

path_in = 'c:\Users\Klemens\Projects\Sabines_Data\Freising\'
filename_daisy_in = 'Freising_1995_2016_3.dwf'
filename_daisy_out = 'Freising_1995_2016_4.dwf'
filename_daisy_ref = 'Freising_1995_2016_2.dwf'

filename_temp = trim(adjustl(path_in))//trim(adjustl(filename_daisy_in))
call read_daisy_file(filename_temp,d)
filename_temp = trim(adjustl(path_in))//trim(adjustl(filename_daisy_ref))
call read_daisy_file(filename_temp,ref)


missing_value = -99.0
! relHumidity
n_predictors = d%n_vars-3 !d%n_vars-2
!predictor_names = (/'T_max  ','T_min  ','AirTemp','GlobRad','Precip '/)
predictor_names = (/'T_max  ','T_min  ','GlobRad','Precip '/)
predictand = 'RelHum'
call analog_approach(n_predictors, predictor_names, predictand, ref, d, missing_value)
! Wind
predictand = 'Wind'
call analog_approach(n_predictors, predictor_names, predictand, ref, d, missing_value)


! indexx_time_period
! n_times_period



call get_index_time_period(d%n_times,d%year,d%month,d%day,start_year_final,start_month_final,start_day_final,&
         stop_year_final,stop_month_final,stop_day_final)


if(allocated(year_new)) deallocate(year_new)
allocate(year_new(1:n_times_period))
if(allocated(month_new)) deallocate(month_new)
allocate(month_new(1:n_times_period))
if(allocated(day_new)) deallocate(day_new)
allocate(day_new(1:n_times_period))
if(allocated(var_new)) deallocate(var_new)
allocate(var_new(1:n_times_period,1:d%n_vars))


do i_times = 1, n_times_period
  index_temp = indexx_time_period(i_times)
  year_new(i_times) = d%year(index_temp)
  month_new(i_times) = d%month(index_temp)
  day_new(i_times) = d%day(index_temp)
  var_new(i_times,1:d%n_vars) = d%var(index_temp,1:d%n_vars)
enddo

d%start_year = start_year_final
d%start_month = start_month_final
d%start_day = start_day_final
d%stop_year = stop_year_final
d%stop_month = stop_month_final
d%stop_day = stop_day_final
d%n_times = n_times_period
if(allocated(d%year))deallocate(d%year)
allocate(d%year(d%n_times))
d%year = year_new  
if(allocated(d%month))deallocate(d%month)
allocate(d%month(d%n_times))
d%month = month_new  
if(allocated(d%day))deallocate(d%day)
allocate(d%day(d%n_times))
d%day = day_new  
if(allocated(d%var))deallocate(d%var)
allocate(d%var(d%n_times,d%n_vars))
d%var = var_new

! readjust temperature statistics
search_string = 'AirTemp'
index_temperature = find_string_in_array(d%n_vars, d%varnames, search_string)

call get_daisy_temp_larswg(d%n_times,d%year,d%month,d%day,d%var(1:d%n_times,index_temperature),&
         missing_value,TAverage,doy_max,TAmplitude)
d%TAverage = TAverage
d%TAmplitude = TAmplitude
d%MaxTDay = doy_max

! write file
filename_temp = trim(adjustl(path_in))//trim(adjustl(filename_daisy_out))
call write_daisy_file(filename_temp,d)
 
 

end program dwf_simulate_rh_wind
