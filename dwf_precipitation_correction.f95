program dwf_precipitation_correction
use mod_read_daisy_file
use mod_daisy_weather_type
use mod_find_string_in_array
use mod_write_daisy_file
use mod_Richter_correction

implicit none

! applies the Richter correction to precipitation data

character(100):: path_in, filename_in, filename_out, filename_temp
character(100):: search_string
integer:: i_precipitation, i_temperature
real:: missing_value, eps, snow_index, temp_precip
type(daisy_weather_type):: d
integer:: region, lev_prot, i
character(1):: region_str, lev_prot_str

call getarg(1, path_in)
call getarg(2, filename_in)
call getarg(3, filename_out)
call getarg(4, region_str)
call getarg(5, lev_prot_str)

! path_in = 'c:\Users\Klemens\Projects\Sabines_Data\Freising\'
! filename_in = 'Freising_1995_2016_1.dwf'
! filename_out = 'Freising_1995_2016_2.dwf'

filename_temp = trim(adjustl(path_in))//trim(adjustl(filename_in))


call read_daisy_file(filename_temp,d)
search_string = 'Precip'
i_precipitation = find_string_in_array(d%n_vars, d%varnames, search_string)
search_string = 'AirTemp'
i_temperature = find_string_in_array(d%n_vars, d%varnames, search_string)
missing_value = -99.0
eps = 0.0000001

read(region_str, '(I1)' )  region   ! region = 8
if(len(trim(adjustl(lev_prot_str))).gt.0)then
  read(lev_prot_str, '(I1)' ) lev_prot
 else 
  lev_prot = 1
endif  

do i = 1, d%n_times
  if(abs(d%var(i,i_precipitation)-missing_value).gt.eps)then
    ! get snow index 
    if(abs(d%var(i,i_temperature)-missing_value).gt.eps)then 
      if(d%var(i,i_temperature).ge.0.0)then
        snow_index = 0.0
       else
        snow_index = 1.0
      endif
      temp_precip = Richter_correction(d%var(i,i_precipitation), d%month(i), region, lev_prot, snow_index)
     else
      temp_precip = Richter_correction(d%var(i,i_precipitation), d%month(i), region, lev_prot)
    endif
    print *, i, d%var(i,i_precipitation), temp_precip 
    d%var(i,i_precipitation) = temp_precip
  endif
enddo 

filename_temp = trim(adjustl(path_in))//trim(adjustl(filename_out))
call write_daisy_file(filename_temp,d)



end program dwf_precipitation_correction
