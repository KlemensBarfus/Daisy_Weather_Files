module mod_daisy_extract_period

! cuts a timespan from a Daisy weather file data structure 
! if start and stop hour are given, only hours in between are extracted

contains

subroutine daisy_extract_period(dwf, dwf_new, start_day, start_month, start_year, stop_day, stop_month, stop_year, start_hour, &
                                 stop_hour)
use mod_daisy_weather_type
use mod_get_daisy_temperatures
use mod_daisy_calc_daily_values
use mod_find_string_in_array

implicit none

type(daisy_weather_type), intent(in):: dwf
type(daisy_weather_type), intent(out):: dwf_new
integer, intent(in):: start_day
integer, intent(in):: start_month
integer, intent(in):: start_year
integer, intent(in):: stop_day
integer, intent(in):: stop_month
integer, intent(in):: stop_year
integer, intent(in), optional:: start_hour
integer, intent(in), optional:: stop_hour


integer, dimension(1:dwf%n_times):: index_valid
integer:: i_times, valid, n_times_new, i_vars, i_times_new
integer:: start_day_temp, start_month_temp, start_year_temp, start_hour_temp
integer:: stop_day_temp, stop_month_temp, stop_year_temp, stop_hour_temp
logical:: check_necessary
type(daisy_weather_type):: dwf_daily
real:: mv
character(100):: search_string
integer:: index_temperature
real:: TAverage, TAmplitude
integer:: doy_max, i_comments

mv = -99.0


start_day_temp = start_day
start_month_temp = start_month
start_year_temp = start_year
stop_day_temp = stop_day
stop_month_temp = stop_month
stop_year_temp = stop_year

! check if hour is in daisy structure
if(allocated(dwf%hour))then
  if(present(start_hour))then
    start_hour_temp = start_hour
   else
    start_hour_temp = 0
  endif
  if(present(stop_hour))then
    stop_hour_temp = stop_hour
   else
    stop_hour_temp = 23
  endif
endif

! get index array of valid cases
!if(allocated(index_valid)) deallocate(index_valid)

index_valid(1:dwf%n_times) = 0

! check for year
do i_times = 1, dwf%n_times
  if(dwf%year(i_times).ge.start_year_temp)then
    if(dwf%year(i_times).le.stop_year_temp)then
      index_valid(i_times) = 1
    endif
  endif
enddo
! check for month
if(start_month_temp.le.stop_month_temp)then
  do i_times = 1, dwf%n_times
    if(dwf%month(i_times).ge.start_month_temp)then
      if(dwf%month(i_times).le.stop_month_temp)then
        valid = 1
       else
        valid = 0
      endif
     else
      valid = 0 
    endif
    if(valid.eq.0)then
      if(index_valid(i_times).eq.1)then
        index_valid(i_times) = 0
      endif
    endif
  enddo  
 else ! start_month > stop_month -> over turn of the year
  do i_times = 1, dwf%n_times
    if(dwf%month(i_times).gt.stop_month_temp)then
      if(dwf%month(i_times).lt.start_month_temp)then
        valid = 0
       else
        valid = 1
      endif
     else
      valid = 1 
    endif
    if(valid.eq.0)then
      if(index_valid(i_times).eq.1)then
        index_valid(i_times) = 0
      endif
    endif
  enddo
endif 
! check for day
if(start_day_temp.le.stop_day_temp)then
  do i_times = 1, dwf%n_times
    if(dwf%day(i_times).ge.start_day_temp)then
      if(dwf%day(i_times).le.stop_day_temp)then
        valid = 1
       else
        valid = 0
      endif
     else
      valid = 0 
    endif
    if(valid.eq.0)then
      if(index_valid(i_times).eq.1)then
        index_valid(i_times) = 0
      endif
    endif
  enddo  
 else ! start_day > stop_day -> over turn of the year
  do i_times = 1, dwf%n_times
    if(dwf%day(i_times).gt.stop_day_temp)then
      if(dwf%day(i_times).lt.start_day_temp)then
        valid = 0
       else
        valid = 1
      endif
     else
      valid = 1 
    endif
    if(valid.eq.0)then
      if(index_valid(i_times).eq.1)then
        index_valid(i_times) = 0
      endif
    endif
  enddo
endif 
! check for hour
if(allocated(dwf%hour))then
  if(start_hour_temp.eq.0)then
    if(stop_hour_temp.eq.23)then
      check_necessary = .false.
     else
      check_necessary = .true.
    endif
   else
    check_necessary = .true.
  endif
  if(check_necessary.eq.(.true.))then 
    if(start_hour_temp.le.stop_hour_temp)then
      do i_times = 1, dwf%n_times
        if(dwf%hour(i_times).ge.start_hour_temp)then
          if(dwf%hour(i_times).le.stop_hour_temp)then
            valid = 1
           else
            valid = 0
          endif
         else
          valid = 0 
        endif
        if(valid.eq.0)then
          if(index_valid(i_times).eq.1)then
            index_valid(i_times) = 0
          endif
        endif
      enddo  
     else ! start_hour > stop_hour
      do i_times = 1, dwf%n_times
        if(dwf%hour(i_times).gt.stop_hour_temp)then
          if(dwf%hour(i_times).lt.start_hour_temp)then
            valid = 0
           else
            valid = 1
          endif
         else
          valid = 1 
        endif
        if(valid.eq.0)then
          if(index_valid(i_times).eq.1)then
            index_valid(i_times) = 0
          endif
        endif
      enddo
    endif 
  endif
endif

! count number of cases
n_times_new = 0
do i_times = 1, dwf%n_times
  if(index_valid(i_times).eq.1)then
    n_times_new = n_times_new+1
  endif
enddo  

! generate new dwf_weather_type
dwf_new%station = dwf%station
dwf_new%elevation = dwf%elevation
dwf_new%longitude = dwf%longitude
dwf_new%longitude_unit = dwf%longitude_unit
dwf_new%latitude = dwf%latitude
dwf_new%latitude_unit = dwf%latitude_unit
dwf_new%timezone = dwf%timezone
dwf_new%timezone_unit = dwf%timezone_unit 
dwf_new%surface = dwf%surface
dwf_new%screen_height = dwf%screen_height 
dwf_new%NH4WetDep = dwf%NH4WetDep
dwf_new%NH4WetDep_unit = dwf%NH4WetDep_unit !NH4WetDep: 0.900000 ppm
dwf_new%NH4DryDep = dwf%NH4DryDep
dwf_new%NH4DryDep_unit = dwf%NH4DryDep_unit !NH4DryDep: 2.20000 kgN/ha/year
dwf_new%NO3WetDep = dwf%NO3WetDep
dwf_new%NO3WetDep_unit = dwf%NO3WetDep_unit !NO3WetDep: 0.300000 ppm
dwf_new%NO3DryDep = dwf%NO3DryDep
dwf_new%NO3DryDep_unit = dwf%NO3DryDep_unit !NO3DryDep: 1.10000 kgN/ha/year
dwf_new%line_string = dwf%line_string !------------------------------------------------------------------------------
dwf_new%n_vars = dwf%n_vars
if(allocated(dwf_new%varnames)) deallocate(dwf_new%varnames)
allocate(dwf_new%varnames(1:dwf_new%n_vars))   
if(allocated(dwf_new%varunits)) deallocate(dwf_new%varunits)
allocate(dwf_new%varunits(1:dwf_new%n_vars))   
do i_vars = 1, dwf_new%n_vars
  dwf_new%varnames(i_vars) = dwf%varnames(i_vars)
  dwf_new%varunits(i_vars) = dwf%varunits(i_vars)
enddo
dwf_new%n_comments = dwf%n_comments
if(dwf_new%n_comments.ge.1)then
  if(allocated(dwf_new%comments)) deallocate(dwf_new%comments) 
  allocate(dwf_new%comments(1:dwf_new%n_comments))
  do i_comments = 1, dwf_new%n_comments
    dwf_new%comments(i_comments) = dwf%comments(i_comments)
  enddo
endif  
dwf_new%n_times = n_times_new
if(allocated(dwf_new%year)) deallocate(dwf_new%year)
allocate(dwf_new%year(1:dwf_new%n_times))   
if(allocated(dwf_new%month)) deallocate(dwf_new%month)
allocate(dwf_new%month(1:dwf_new%n_times))   
if(allocated(dwf_new%day)) deallocate(dwf_new%day)
allocate(dwf_new%day(1:dwf_new%n_times))   
if(allocated(dwf%hour))then
  if(allocated(dwf_new%hour)) deallocate(dwf_new%hour)
  allocate(dwf_new%hour(1:dwf_new%n_times))   
endif
if(allocated(dwf_new%var)) deallocate(dwf_new%var)
allocate(dwf_new%var(1:dwf_new%n_times,1:dwf_new%n_vars))   
i_times_new = 1
do i_times = 1, dwf%n_times
  if(index_valid(i_times).eq.1)then
    dwf_new%year(i_times_new) = dwf%year(i_times)
    dwf_new%month(i_times_new) = dwf%month(i_times)
    dwf_new%day(i_times_new) = dwf%day(i_times)
    if(allocated(dwf%hour))then
      dwf_new%hour(i_times_new) = dwf%hour(i_times)
    endif
    do i_vars = 1, dwf_new%n_vars
      dwf_new%var(i_times_new,i_vars) = dwf%var(i_times,i_vars)
    enddo
    i_times_new = i_times_new + 1
  endif
enddo

dwf_new%start_year = dwf_new%year(1)
dwf_new%start_month = dwf_new%month(1)
dwf_new%start_day = dwf_new%day(1)
dwf_new%stop_year = dwf_new%year(dwf_new%n_times)
dwf_new%stop_month = dwf_new%month(dwf_new%n_times)
dwf_new%stop_day = dwf_new%day(dwf_new%n_times)

! calc daily values for extracted period
if(allocated(dwf_new%hour))then
  call daisy_calc_daily_values(dwf_new, dwf_daily, mv)
  ! adjust temperature statistics
  search_string = 'AirTemp'
  index_temperature = find_string_in_array(dwf_daily%n_vars, dwf_daily%varnames, search_string)
  call get_daisy_temperatures(dwf_daily%n_times, dwf_daily%year, dwf_daily%month, dwf_daily%day, &
            dwf_daily%var(1:dwf_daily%n_times,index_temperature),mv, &
                        TAverage,doy_max,TAmplitude)
 else ! already daily data existing in structure
  search_string = 'AirTemp'
  index_temperature = find_string_in_array(dwf_new%n_vars, dwf_new%varnames, search_string)
  call get_daisy_temperatures(dwf_new%n_times, dwf_new%year, dwf_new%month, dwf_new%day, &
            dwf_new%var(1:dwf_new%n_times,index_temperature),mv, &
                        TAverage,doy_max,TAmplitude)
endif 
dwf_new%TAverage = TAverage
dwf_new%TAmplitude = TAmplitude
dwf_new%MaxTDay = doy_max


end subroutine daisy_extract_period

end module mod_daisy_extract_period 

