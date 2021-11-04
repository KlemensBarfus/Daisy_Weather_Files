module mod_write_daisy_file

contains 

subroutine write_daisy_file(filename,dwf,missing_value)
use mod_strsplit
use mod_daisy_weather_type
use mod_concatenate_string_array
use mod_get_string_format_string
use mod_get_real_format_string
use mod_get_integer_format_string


implicit none

character(*), intent(in):: filename
type(daisy_weather_type), intent(inout):: dwf
real, intent(in), optional:: missing_value

character(150):: temp_str, temp_str2
character(100):: fmt_string
character(100):: temp_str1, temp_str3
integer:: i_times, i_vars, decimal_places, i_comments
character(1):: tab
character(100):: t_str1, t_str2, t_str3, t_str4
character(3):: nan_string
real:: mv, eps

tab = char(9)
decimal_places = 2
nan_string = 'NaN'
eps = 0.0000001

if(present(missing_value))then
  mv = missing_value
 else
  mv = -99.0
endif  

! reads a daisy file in a predefined structure
open(1, file=filename, access='sequential', form='formatted', status='replace')
print *, filename
write(1, fmt='(A7)') 'dwf-0.0'
temp_str = 'Station: '//trim(adjustl(dwf%station))
temp_str2 = get_string_format_string(temp_str)
fmt_string = '('//trim(adjustl(temp_str2))//')'
write(1,fmt=fmt_string) temp_str
temp_str2 = get_real_format_string(dwf%elevation,decimal_places)
fmt_string = '(A10,1x,'//trim(adjustl(temp_str2))//',1x,A1)'
write(1,fmt=fmt_string) 'Elevation: ',dwf%elevation,'m'
temp_str = get_real_format_string(dwf%longitude,decimal_places)
temp_str2 = get_string_format_string(dwf%longitude_unit)
fmt_string = '(A10,1x,'//trim(adjustl(temp_str))//',1x,'//trim(adjustl(temp_str2))//')'
write(1,fmt=fmt_string) 'Longitude:', dwf%longitude, dwf%longitude_unit
temp_str = get_real_format_string(dwf%latitude,decimal_places)
temp_str2 = get_string_format_string(dwf%latitude_unit)
fmt_string = '(A9,1x,'//trim(adjustl(temp_str))//',1x,'//trim(adjustl(temp_str2))//')'
write(1,fmt=fmt_string) 'Latitude:', dwf%latitude, dwf%latitude_unit
temp_str = get_integer_format_string(dwf%timezone)
temp_str2 = get_string_format_string(dwf%timezone_unit)
fmt_string = '(A9,1x,'//trim(adjustl(temp_str))//',1x,'//trim(adjustl(temp_str2))//')'
write(1,fmt=fmt_string) 'TimeZone:', dwf%timezone, dwf%timezone_unit
temp_str = get_string_format_string(dwf%surface)
fmt_string = '(A8,1x,'//trim(adjustl(temp_str))//')'
write(1,fmt=fmt_string)'Surface:', dwf%surface
temp_str = get_integer_format_string(dwf%screen_height)
fmt_string = '(A13,1x,'//trim(adjustl(temp_str))//',1x,A1)'
write(1,fmt=fmt_string) 'ScreenHeight:', dwf%screen_height, 'm'
temp_str1 = get_integer_format_string(dwf%start_year)
temp_str2 = get_integer_format_string(dwf%start_month)
temp_str3 = get_integer_format_string(dwf%start_day)
fmt_string = '(A6,1x,'//trim(adjustl(temp_str1))//',A1,'//trim(adjustl(temp_str2))//',A1,'//trim(adjustl(temp_str3))//')'
write(1,fmt=fmt_string)'Begin:', dwf%start_year,'-',dwf%start_month,'-',dwf%start_day
temp_str1 = get_integer_format_string(dwf%stop_year)
temp_str2 = get_integer_format_string(dwf%stop_month)
temp_str3 = get_integer_format_string(dwf%stop_day)
fmt_string = '(A4,1x,'//trim(adjustl(temp_str1))//',A1,'//trim(adjustl(temp_str2))//',A1,'//trim(adjustl(temp_str3))//')'
write(1,fmt=fmt_string)'End:', dwf%stop_year,'-',dwf%stop_month,'-',dwf%stop_day
write(1,fmt='(A10,1x,F9.6,1x,A20)') 'NH4WetDep:', dwf%NH4WetDep, dwf%NH4WetDep_unit
write(1,fmt='(A10,1x,F9.6,1x,A20)') 'NH4DryDep:', dwf%NH4DryDep, dwf%NH4DryDep_unit
write(1,fmt='(A10,1x,F9.6,1x,A20)') 'NO3WetDep:', dwf%NO3WetDep, dwf%NO3WetDep_unit
write(1,fmt='(A10,1x,F9.6,1x,A20)') 'NO3DryDep:', dwf%NO3DryDep, dwf%NO3DryDep_unit
! write comments
if(dwf%n_comments.ge.1)then
  do i_comments = 1, dwf%n_comments
    temp_str1 = get_string_format_string(dwf%comments(i_comments))
    fmt_string = '(A1,1x,'//trim(adjustl(temp_str1))//')'
    write(1,fmt=fmt_string)'# ',trim(adjustl(dwf%comments(i_comments)))
  enddo
endif
write(1,fmt='(A9,1x, F8.5,1x,A3)') 'TAverage:', dwf%TAverage,'dgC'
write(1,fmt='(A11,1x,F8.4,1x,A3)') 'TAmplitude:', dwf%TAmplitude, 'dgC'
write(1,fmt='(A8,1x,I3,1x,A4)')'MaxTDay:', dwf%MaxTDay, 'yday'
write(1,fmt='(A80)') '--------------------------------------------------------------------------------'
temp_str = concatenate_string_array(dwf%n_vars,dwf%varnames,tab)
if(allocated(dwf%hour))then
  temp_str2 = 'Year'//tab//'Month'//tab//'Day'//tab//'Hour'//tab//trim(adjustl(temp_str))
 else
  temp_str2 = 'Year'//tab//'Month'//tab//'Day'//tab//trim(adjustl(temp_str))
endif
fmt_string = '('//trim(adjustl(get_string_format_string(temp_str2)))//')'
write(1,fmt=fmt_string) temp_str2
! parameter units
temp_str = concatenate_string_array(dwf%n_vars,dwf%varunits,tab)
if(allocated(dwf%hour))then
  temp_str2 = 'year'//tab//'month'//tab//'mday'//tab//'hour'//tab//trim(adjustl(temp_str))
 else 
  temp_str2 = 'year'//tab//'month'//tab//'mday'//tab//trim(adjustl(temp_str))
endif  
fmt_string = '('//trim(adjustl(get_string_format_string(temp_str2)))//')'
write(1,fmt=fmt_string) temp_str2
!write(temp_str,fmt='(I2)') dwf%n_vars-1
!fmt_string = '(I4,1x,A2,1x,A2,1x,'//trim(adjustl(temp_str))//'(F7.3,1x),F7.3)'
do i_times = 1, dwf%n_times
  t_str1 = get_integer_format_string(dwf%year(i_times)) 
  t_str2 = get_integer_format_string(dwf%month(i_times)) 
  t_str3 = get_integer_format_string(dwf%day(i_times))
  if(allocated(dwf%hour))then
    t_str4 = get_integer_format_string(dwf%hour(i_times))
    fmt_string = '('//trim(adjustl(t_str1))//',A1,'//trim(adjustl(t_str2))//',A1,'//trim(adjustl(t_str3))//',A1,'//&
       trim(adjustl(t_str4))//',A1)'
   else 
    fmt_string = '('//trim(adjustl(t_str1))//',A1,'//trim(adjustl(t_str2))//',A1,'//trim(adjustl(t_str3))//',A1)'
  endif
  if(allocated(dwf%hour))then
    write(1, fmt=fmt_string, advance='no') dwf%year(i_times), tab, dwf%month(i_times), tab, dwf%day(i_times), &
           tab, dwf%hour(i_times), tab 
   else  
    write(1, fmt=fmt_string, advance='no') dwf%year(i_times), tab, dwf%month(i_times), tab, dwf%day(i_times), tab 
  endif  
  do i_vars = 1, dwf%n_vars
    if(abs(dwf%var(i_times,i_vars)-mv).lt.eps)then
      if(i_vars.lt.dwf%n_vars)then
        write(1, fmt='(A3,A1)', advance='no') nan_string, tab
       else
        write(1, fmt='(A3)') nan_string
      endif
     else  
      t_str1 = get_real_format_string(dwf%var(i_times,i_vars),decimal_places)
      if(i_vars.lt.dwf%n_vars)then
        fmt_string = '('//trim(adjustl(t_str1))//',A1)'
        write(1, fmt=fmt_string, advance='no') dwf%var(i_times,i_vars), tab
       else
        fmt_string = '('//trim(adjustl(t_str1))//')'
        write(1, fmt=fmt_string) dwf%var(i_times,i_vars)
      endif
    endif  
  enddo 
enddo
close(1)

 
end subroutine write_daisy_file

end module mod_write_daisy_file

