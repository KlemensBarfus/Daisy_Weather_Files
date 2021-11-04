module mod_read_daisy_file

contains 

subroutine read_daisy_file(filename,dwf,missing_value)
use mod_strsplit
use mod_find_string_in_array
use mod_daisy_weather_type

implicit none

character(*), intent(in):: filename
!character(150), intent(in):: filename
type(daisy_weather_type), intent(inout):: dwf
real, intent(in), optional:: missing_value

character(100):: temp_str, temp_str2, search_string, nan_string
integer:: k, i_times, i_vars, i_hour
integer:: i_year, i_month, i_day
integer:: n_header_lines ! number of header lines until underscore line
integer:: a, i_comments, i_header_lines
real:: mv
logical:: found_end_of_header

if(present(missing_value))then
  mv = missing_value
 else
  mv = -99.0
endif 

n_header_lines = 0
dwf%n_comments = 0

nan_string = 'NaN'
  

found_end_of_header = .false. 

! reads a daisy file in a predefined structure
open(1, file=filename, readonly, access='sequential', form='formatted')
print *, filename
do
  if(found_end_of_header.eq.(.true.))exit
  read(1, fmt='(A100)') temp_str
  !print *, temp_str 
  temp_str2 = trim(adjustl(temp_str))
  n_header_lines = n_header_lines+1
  if(temp_str2(1:1).eq.'#')then
    dwf%n_comments = dwf%n_comments+1
   else
    if(temp_str2(1:3).eq.'dwf')then !dwf-0.0
      a = 0 ! nothing happens, just read
    endif  
    if(temp_str2(1:7).eq.'Station')then !Station: Gera-Leumnitz
      call strsplit(temp_str)
      dwf%station = substring(2)
    endif
    if(temp_str2(1:9).eq.'Elevation')then !Elevation: 50 m
      call strsplit(temp_str)
      read(substring(2), fmt='(F7.2)') dwf%elevation
    endif
    if(temp_str2(1:9).eq.'Longitude')then !Longitude: 12 dgEast
      call strsplit(temp_str)
      read(substring(2), fmt='(F8.3)') dwf%longitude
      dwf%longitude_unit = substring(3) 
    endif  
    if(temp_str2(1:8).eq.'Latitude')then !Latitude: 51 dgNorth
      call strsplit(temp_str)
      read(substring(2), fmt='(F7.3)') dwf%latitude
      dwf%latitude_unit = substring(3) 
    endif
    if(temp_str2(1:8).eq.'TimeZone')then !TimeZone: 15 dgEast
      call strsplit(temp_str)
      read(substring(2), fmt='(I4)') dwf%timezone
      dwf%timezone_unit = substring(3) 
    endif
    if(temp_str2(1:7).eq.'Surface')then !Surface: ref
      call strsplit(temp_str)
      dwf%surface = substring(2) 
    endif
    if(temp_str2(1:12).eq.'ScreenHeight')then !ScreenHeight: 2 m
      call strsplit(temp_str)
      read(substring(2), fmt='(I3)') dwf%screen_height
    endif
    if(temp_str2(1:5).eq.'Begin')then !Begin: 1953-01-01
      call strsplit(temp_str)
      call strsplit(substring(2),'-')
      read(substring(1), fmt='(I4)') dwf%start_year
      read(substring(2), fmt='(I2)') dwf%start_month
      read(substring(3), fmt='(I2)') dwf%start_day
    endif  
    if(temp_str2(1:3).eq.'End')then   !End: 1990-12-31
      call strsplit(temp_str)
      call strsplit(substring(2),'-')
      read(substring(1), fmt='(I4)') dwf%stop_year
      read(substring(2), fmt='(I2)') dwf%stop_month
      read(substring(3), fmt='(I2)') dwf%stop_day
    endif  
    if(temp_str2(1:9).eq.'NH4WetDep')then !NH4WetDep: 0.900000 ppm
      call strsplit(temp_str)
      read(substring(2), fmt='(F9.6)') dwf%NH4WetDep
      dwf%NH4WetDep_unit = substring(3)
    endif  
    if(temp_str2(1:9).eq.'NH4DryDep')then !NH4DryDep: 2.20000 kgN/ha/year
      call strsplit(temp_str)
      read(substring(2), fmt='(F9.6)') dwf%NH4DryDep
      dwf%NH4DryDep_unit = substring(3)
    endif
    if(temp_str2(1:9).eq.'NO3WetDep')then  !NO3WetDep: 0.300000 ppm
      call strsplit(temp_str)
      read(substring(2), fmt='(F9.6)') dwf%NO3WetDep
      dwf%NO3WetDep_unit = substring(3)
    endif
    if(temp_str2(1:9).eq.'NO3DryDep')then !NO3DryDep: 1.10000 kgN/ha/year
      call strsplit(temp_str)
      read(substring(2), fmt='(F9.6)') dwf%NO3DryDep
      dwf%NO3DryDep_unit = substring(3)
    endif  
    if(temp_str2(1:8).eq.'TAverage')then !TAverage: 7.97144 dgC
      call strsplit(temp_str)
      read(substring(2), fmt='(F8.5)') dwf%TAverage
    endif
    if(temp_str2(1:10).eq.'TAmplitude')then !TAmplitude: 36.7526 dgC
      call strsplit(temp_str)
      read(substring(2), fmt='(F8.4)') dwf%TAmplitude
    endif
    if(temp_str2(1:7).eq.'MaxTDay')then !MaxTDay: 200 yday
      call strsplit(temp_str)
      read(substring(2), fmt='(I3)') dwf%MaxTDay
    endif
    if(temp_str2(1:1).eq.'-')then  ! line string
      dwf%line_string = temp_str
      found_end_of_header = .true.
    endif
  endif
enddo
rewind(1)  
! get comments
if(allocated(dwf%comments)) deallocate(dwf%comments)
allocate(dwf%comments(1:dwf%n_comments))
rewind(1)
i_comments = 1
do i_header_lines = 1, n_header_lines
  read(1, fmt='(A100)') temp_str 
  temp_str2 = trim(adjustl(temp_str))
  if(temp_str2(1:1).eq.'#')then
    dwf%comments(i_comments) = temp_str(2:100)//' ' 
    i_comments = i_comments + 1
  endif  
enddo
! varnames
read(1,fmt='(A100)') temp_str !Year Month Day GlobRad AirTemp Precip
call strsplit(temp_str)
! yields 'substring' and 'n_substring'
search_string = 'Day'
i_day = find_string_in_array(n_substring, substring, search_string)
search_string = 'Month'
i_month = find_string_in_array(n_substring, substring, search_string)
search_string = 'Year'
i_year = find_string_in_array(n_substring, substring, search_string)
search_string = 'Hour'
i_hour = find_string_in_array(n_substring, substring, search_string)
if(i_hour.gt.0)then
  dwf%n_vars = n_substring-4
 else
  dwf%n_vars = n_substring-3
endif  
if(allocated(dwf%varnames)) deallocate(dwf%varnames)
allocate(dwf%varnames(1:dwf%n_vars))
if(i_hour.gt.0)then
  dwf%varnames = substring(5:dwf%n_vars)
 else 
  dwf%varnames = substring(4:dwf%n_vars)
endif  
! parameter units
read(1,fmt='(A100)') temp_str !year month mday W/m^2 dgC mm/d
call strsplit(temp_str)
if(allocated(dwf%varunits)) deallocate(dwf%varunits)
allocate(dwf%varunits(1:dwf%n_vars))
if(i_hour.gt.0)then
  dwf%varunits = substring(5:dwf%n_vars)
 else 
  dwf%varunits = substring(4:dwf%n_vars)
endif
! get number of timesteps
k = 1
dwf%n_times=0
do
  if(k.lt.0)exit
  read(1, iostat=k, fmt='(A100)') temp_str
!  print *, temp_str
  dwf%n_times = dwf%n_times + 1
enddo
dwf%n_times = dwf%n_times-1

if(allocated(dwf%year)) deallocate(dwf%year)
allocate(dwf%year(1:dwf%n_times))
if(allocated(dwf%month)) deallocate(dwf%month)
allocate(dwf%month(1:dwf%n_times))
if(allocated(dwf%day)) deallocate(dwf%day)
allocate(dwf%day(1:dwf%n_times))
if(i_hour.gt.0)then
  if(allocated(dwf%hour)) deallocate(dwf%hour)
  allocate(dwf%hour(1:dwf%n_times))
endif
if(allocated(dwf%var)) deallocate(dwf%var)
allocate(dwf%var(1:dwf%n_times,1:dwf%n_vars))
rewind(1)

do i_times = 1, n_header_lines+2
  read(1, fmt='(A100)') temp_str
enddo  

do i_times = 1, dwf%n_times
  read(1, fmt='(A100)') temp_str
  call strsplit(temp_str)
  read(substring(i_year), fmt='(I4)') dwf%year(i_times)
  read(substring(i_month), fmt='(I2)') dwf%month(i_times)
  read(substring(i_day), fmt='(I2)') dwf%day(i_times)
  if(i_hour.gt.0)then
    read(substring(i_hour), fmt='(I2)') dwf%hour(i_times)
    do i_vars = 1, dwf%n_vars
      if(trim(adjustl(substring(i_vars+4))).eq.trim(adjustl(nan_string)))then
        dwf%var(i_times,i_vars) = mv
       else 
        read(substring(i_vars+4), fmt='(F7.3)') dwf%var(i_times,i_vars)
      endif  
    enddo
   else  
    do i_vars = 1, dwf%n_vars
      if(trim(adjustl(substring(i_vars+3))).eq.trim(adjustl(nan_string)))then
        dwf%var(i_times,i_vars) = mv
       else 
        read(substring(i_vars+3), fmt='(F7.3)') dwf%var(i_times,i_vars)
      endif  
    enddo
  endif  
enddo  

close(1)     
 
end subroutine read_daisy_file

end module mod_read_daisy_file

