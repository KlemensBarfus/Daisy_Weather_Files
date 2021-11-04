module mod_daisy_calc_daily_values

contains 

subroutine daisy_calc_daily_values(dwf, dwf_new, mv)
use mod_daisy_weather_type
!use mod_julian_day
use mod_find_string_in_array
use mod_saturation_vapour_pressure
use mod_get_all_dates_inbetween
use mod_wind_conversion

implicit none

! calculates daily values from hourly values
! in and output are a daisy structure
! variables T_min and T_max are added
! Temperature statistic is not adjusted

type(daisy_weather_type), intent(in):: dwf
type(daisy_weather_type), intent(out):: dwf_new
real, intent(in), optional:: mv

! local variables
integer:: i_comments
real:: missing_value, eps
character(100):: search_string
integer:: index_AirTemp, index_RelHum, index_Tmin, index_Tmax
integer:: index_Winddir
logical:: only_water, vp_flag
integer:: i_times
real:: T_temp, e_temp, es_temp, rh_temp
integer:: index_Pressure, index_Precip, index_Wind, index_GlobRad
logical:: Tmin_new, Tmax_new
integer, dimension(:,:), allocatable:: n_vals
integer:: i_times_new, i_vars
logical:: found, uv_flag
real:: uwind_temp, vwind_temp, windspeed_temp, winddir_temp
real:: pi
real, dimension(:), allocatable:: uwind, vwind
!real:: w1_test,w2_test

eps = 0.000001
pi = 4.0*atan(1.0)
if(present(mv))then
  missing_value = mv
 else
  missing_value = -99.000000
endif 

! assign header values
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
dwf_new%NH4WetDep_unit = dwf%NH4WetDep_unit
dwf_new%NH4DryDep = dwf%NH4DryDep
dwf_new%NH4DryDep_unit = dwf%NH4DryDep_unit
dwf_new%NO3WetDep = dwf%NO3WetDep
dwf_new%NO3WetDep_unit = dwf%NO3WetDep_unit
dwf_new%NO3DryDep = dwf%NO3DryDep
dwf_new%NO3DryDep_unit = dwf%NO3DryDep_unit
dwf_new%TAverage = dwf%TAverage
dwf_new%TAmplitude = dwf%TAmplitude
dwf_new%MaxTDay = dwf%MaxTDay
dwf_new%line_string = dwf%line_string
dwf_new%n_comments = dwf%n_comments
if(dwf_new%n_comments.ge.1)then
  if(allocated(dwf_new%comments)) deallocate(dwf_new%comments)
  allocate(dwf_new%comments(1:dwf_new%n_comments))
  do i_comments = 1, dwf_new%n_comments
    dwf_new%comments(i_comments) = dwf%comments(i_comments)
  enddo  
endif

call get_all_dates_inbetween(dwf%year(1),dwf%month(1),dwf%day(1),dwf%year(dwf%n_times),dwf%month(dwf%n_times),dwf%day(dwf%n_times))

dwf_new%start_year = g%year(1)
dwf_new%start_month = g%month(1)
dwf_new%start_day = g%day(1)
!start_julday = julian_day(dwf_new%start_year,dwf_new%start_month,dwf_new%start_day)

dwf_new%stop_year = g%year(g%n_dates)
dwf_new%stop_month = g%month(g%n_dates)
dwf_new%stop_day = g%day(g%n_dates)
!stop_julday = julian_day(dwf_new%stop_year,dwf_new%stop_month,dwf_new%stop_day)
 

dwf_new%n_times = g%n_dates
if(allocated(dwf_new%day)) deallocate(dwf_new%day)
allocate(dwf_new%day(1:dwf_new%n_times))
dwf_new%day = g%day
if(allocated(dwf_new%month)) deallocate(dwf_new%month)
allocate(dwf_new%month(1:dwf_new%n_times))
dwf_new%month = g%month
if(allocated(dwf_new%year)) deallocate(dwf_new%year)
allocate(dwf_new%year(1:dwf_new%n_times))
dwf_new%year = g%year

! find index of parameters
search_string = "AirTemp"
index_AirTemp = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)
search_string = "RelHum"
index_RelHum = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)
if(index_RelHum.ge.1)then
  if(index_AirTemp.ge.1)then
    vp_flag = .true.   ! calculation via vapour pressure
   else
    vp_flag = .false.
  endif
endif 
search_string = " Winddir"
index_Winddir = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)
search_string = "GlobRad"
index_GlobRad = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)
search_string = "Precip"
index_Precip = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)
search_string = "Wind"
index_Wind = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)
if(index_Wind.ge.1)then
  if(index_Winddir.ge.1)then
    if(allocated(uwind)) deallocate(uwind)
    allocate(uwind(1:dwf%n_times))
    if(allocated(vwind)) deallocate(vwind)
    allocate(vwind(1:dwf%n_times))
    do i_times = 1, dwf%n_times
      uv_flag = .false.
      if(abs(dwf%var(i_times,index_Winddir)-missing_value).gt.eps)then
        if(abs(dwf%var(i_times,index_Wind)-missing_value).gt.eps)then
          call wind_to_uvwind(dwf%var(i_times,index_Wind),dwf%var(i_times,index_Winddir),uwind_temp,vwind_temp)
          uwind(i_times) = uwind_temp
          vwind(i_times) = vwind_temp
          !call uvwind_to_wind(uwind_temp,vwind_temp,w1_test,w2_test)
          !print *, dwf%var(i_times,index_Wind),dwf%var(i_times,index_Winddir), uwind_temp, vwind_temp, w1_test,w2_test
          uv_flag = .true.
        endif
      endif
      if(uv_flag.eq.(.false.))then
        uwind(i_times) = missing_value
        vwind(i_times) = missing_value
      endif
    enddo
  endif
endif  
search_string = "Pressure"
index_Pressure = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)
dwf_new%n_vars = dwf%n_vars
search_string = "Tmin"
index_Tmin = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)
Tmin_new = .false.
if(index_Tmin.lt.1)then
  index_Tmin = dwf%n_vars+1
  dwf_new%n_vars = dwf_new%n_vars + 1
  Tmin_new = .true.
endif
search_string = "Tmax"
index_Tmax = find_string_in_array(dwf%n_vars, dwf%varnames, search_string)
Tmax_new = .false.
if(index_Tmax.lt.1)then
  index_Tmax = dwf%n_vars+2
  dwf_new%n_vars = dwf_new%n_vars + 1
  Tmax_new = .true.
endif
! varnames
if(allocated(dwf_new%varnames)) deallocate(dwf_new%varnames)
allocate(dwf_new%varnames(1:dwf_new%n_vars))
if(allocated(dwf_new%varunits)) deallocate(dwf_new%varunits)
allocate(dwf_new%varunits(1:dwf_new%n_vars))
if(dwf_new%n_vars.eq.dwf%n_vars)then
  dwf_new%varnames = dwf%varnames
  dwf_new%varunits = dwf%varunits
 else
  dwf_new%varnames(1:dwf%n_vars) = dwf%varnames
  dwf_new%varnames(dwf%n_vars+1) = "T_min"
  dwf_new%varnames(dwf%n_vars+2) = "T_max"
  dwf_new%varunits(1:dwf%n_vars) = dwf%varunits
  dwf_new%varunits(dwf%n_vars+1) = "dgC"
  dwf_new%varunits(dwf%n_vars+2) = "dgC"
endif
if(trim(adjustl(dwf_new%varunits(index_Precip))).eq."mm/h")then
  dwf_new%varunits(index_Precip)="mm/d"
endif  
if(allocated(dwf_new%var)) deallocate(dwf_new%var)
allocate(dwf_new%var(1:dwf_new%n_times,dwf_new%n_vars)) 
dwf_new%var = 0.0
if(allocated(n_vals)) deallocate(n_vals)
allocate(n_vals(1:dwf_new%n_times,dwf_new%n_vars))
n_vals = 0



only_water = .true.
i_times_new = 1
do i_times = 1, dwf%n_times
  ! find temporal index of dwf_new structure
  found = .false.
  do
    if(found.eqv.(.true.))exit
    if(dwf%year(i_times).eq.dwf_new%year(i_times_new))then
      if(dwf%month(i_times).eq.dwf_new%month(i_times_new))then
        if(dwf%day(i_times).eq.dwf_new%day(i_times_new))then
          found = .true.
        endif
      endif
    endif
    if(found.eqv.(.false.))then
      i_times_new = i_times_new + 1
      if(i_times_new.gt.dwf_new%n_times)then
        i_times_new = 1
      endif
    endif  
  enddo
  ! winddir - averaging is done by uwind and vwind
  if(index_Wind.ge.1)then
    if(index_Winddir.ge.1)then
      if(abs(uwind(i_times) - missing_value).gt.eps)then
        if(abs(vwind(i_times) - missing_value).gt.eps)then
          dwf_new%var(i_times_new, index_Winddir) = dwf_new%var(i_times_new, index_Winddir) + &
              uwind(i_times)
          dwf_new%var(i_times_new, index_Wind) = dwf_new%var(i_times_new, index_Wind) + &
              vwind(i_times)
          n_vals(i_times_new,index_Winddir) = n_vals(i_times_new,index_Winddir) + 1
          n_vals(i_times_new,index_Wind) = n_vals(i_times_new,index_Wind) + 1
        endif
      endif
     else
      if(abs(dwf%var(i_times,index_Wind) - missing_value).gt.eps)then  ! temperature exists
        dwf_new%var(i_times_new,index_Wind) = dwf_new%var(i_times_new,index_Wind) + dwf%var(i_times,index_Wind)
        n_vals(i_times_new,index_Wind) = n_vals(i_times_new,index_Wind) + 1
      endif  
    endif
  endif
  ! relative humidity
  ! calculate e_temp
  if(index_RelHum.ge.1)then
    if(index_AirTemp.ge.1)then
      if(abs(dwf%var(i_times,index_RelHum) - missing_value).gt.eps)then
        if(abs(dwf%var(i_times,index_AirTemp) - missing_value).gt.eps)then  ! temperature exists
          T_temp  = dwf%var(i_times,index_AirTemp) + 273.15
          es_temp = saturation_vapour_pressure(T_temp,only_water)
          e_temp = (dwf%var(i_times,index_RelHum)/100.0) * es_temp
          dwf_new%var(i_times_new,index_RelHum) = dwf_new%var(i_times_new,index_RelHum) + e_temp
          n_vals(i_times_new,index_RelHum) = n_vals(i_times_new,index_RelHum) + 1
        endif
      endif
     else  ! without temperature
      if(abs(dwf%var(i_times,index_RelHum) - missing_value).gt.eps)then       
        dwf_new%var(i_times_new,index_RelHum) = dwf_new%var(i_times_new,index_RelHum) + dwf%var(i_times,index_RelHum)
        n_vals(i_times_new,index_RelHum) = n_vals(i_times_new,index_RelHum) + 1
      endif  
    endif
  endif
  ! temperature
  if(index_AirTemp.ge.1)then
    if(abs(dwf%var(i_times,index_AirTemp) - missing_value).gt.eps)then  ! temperature exists
      dwf_new%var(i_times_new,index_AirTemp) = dwf_new%var(i_times_new,index_AirTemp) + dwf%var(i_times,index_AirTemp)
      n_vals(i_times_new,index_AirTemp) = n_vals(i_times_new,index_AirTemp) + 1
    endif
  endif
  ! Glob Rad 
  if(index_GlobRad.ge.1)then
    if(abs(dwf%var(i_times,index_GlobRad) - missing_value).gt.eps)then  ! temperature exists
      dwf_new%var(i_times_new,index_GlobRad) = dwf_new%var(i_times_new,index_GlobRad) + dwf%var(i_times,index_GlobRad)
      n_vals(i_times_new,index_GlobRad) = n_vals(i_times_new,index_GlobRad) + 1
    endif
  endif
  ! Precip
  if(index_Precip.ge.1)then
    if(abs(dwf%var(i_times,index_Precip) - missing_value).gt.eps)then  ! temperature exists
      dwf_new%var(i_times_new,index_Precip) = dwf_new%var(i_times_new,index_Precip) + dwf%var(i_times,index_Precip)
      n_vals(i_times_new,index_Precip) = n_vals(i_times_new,index_Precip) + 1
    endif
  endif
  ! Pressure
  if(index_Pressure.ge.1)then
    if(abs(dwf%var(i_times,index_Pressure) - missing_value).gt.eps)then  ! temperature exists
      dwf_new%var(i_times_new,index_Pressure) = dwf_new%var(i_times_new,index_Pressure) + dwf%var(i_times,index_Pressure)
      n_vals(i_times_new,index_Pressure) = n_vals(i_times_new,index_Pressure) + 1
    endif
  endif
  ! TMin
  if(index_Tmin.ge.1)then  ! TMin exists
    if(Tmin_new.eqv.(.false.))then
      if(abs(dwf%var(i_times,index_Tmin) - missing_value).gt.eps)then  ! temperature exists
        if(n_vals(i_times_new,index_Tmin).eq.0)then ! no data in
          dwf_new%var(i_times_new,index_Tmin) = dwf%var(i_times,index_Tmin)
          n_vals(i_times_new,index_Tmin) = n_vals(i_times_new,index_Tmin) + 1
         else
          if(dwf%var(i_times,index_Tmin).lt.dwf_new%var(i_times_new,index_Tmin))then
            dwf_new%var(i_times_new,index_Tmin) = dwf%var(i_times,index_Tmin)
          endif
        endif  
      endif
     else ! derive from AirTemp
      if(index_AirTemp.ge.1)then
        if(abs(dwf%var(i_times,index_AirTemp) - missing_value).gt.eps)then  ! temperature exists
          if(n_vals(i_times_new,index_Tmin).eq.0)then ! no data in
            dwf_new%var(i_times_new,index_Tmin) = dwf%var(i_times,index_AirTemp)
            n_vals(i_times_new,index_Tmin) = n_vals(i_times_new,index_Tmin) + 1
           else
            if(dwf%var(i_times,index_AirTemp).lt.dwf_new%var(i_times_new,index_Tmin))then
              dwf_new%var(i_times_new,index_Tmin) = dwf%var(i_times,index_AirTemp)
            endif
          endif
        endif   
      endif
    endif
  endif
  ! TMax
  if(index_Tmax.ge.1)then  ! TMin exists
    if(Tmax_new.eqv.(.false.))then
      if(abs(dwf%var(i_times,index_Tmax) - missing_value).gt.eps)then  ! temperature exists
        if(n_vals(i_times_new,index_Tmax).eq.0)then ! no data in
          dwf_new%var(i_times_new,index_Tmax) = dwf%var(i_times,index_Tmax)
          n_vals(i_times_new,index_Tmax) = n_vals(i_times_new,index_Tmax) + 1
         else
          if(dwf%var(i_times,index_Tmax).gt.dwf_new%var(i_times_new,index_Tmax))then
            dwf_new%var(i_times_new,index_Tmax) = dwf%var(i_times,index_Tmax)
          endif
        endif  
      endif
     else ! derive from AirTemp
      if(index_AirTemp.ge.1)then
        if(abs(dwf%var(i_times,index_AirTemp) - missing_value).gt.eps)then  ! temperature exists
          if(n_vals(i_times_new,index_Tmax).eq.0)then ! no data in
            dwf_new%var(i_times_new,index_Tmax) = dwf%var(i_times,index_AirTemp)
            n_vals(i_times_new,index_Tmax) = n_vals(i_times_new,index_Tmax) + 1
           else
            if(dwf%var(i_times,index_AirTemp).gt.dwf_new%var(i_times_new,index_Tmax))then
              dwf_new%var(i_times_new,index_Tmax) = dwf%var(i_times,index_AirTemp)
            endif
          endif
        endif   
      endif
    endif
  endif
enddo  

! calculate aggregated values
do i_times = 1, dwf_new%n_times
  ! ensure that AirTemp is averaged first (for rh calculations)
  do i_vars = 1, dwf_new%n_vars
    if(n_vals(i_times,i_vars).eq.24)then
      if(i_vars.eq.index_AirTemp)then
        dwf_new%var(i_times,i_vars) = dwf_new%var(i_times,i_vars) / float(n_vals(i_times,i_vars))
      endif
     else
      dwf_new%var(i_times,i_vars) = missing_value
    endif  
  enddo  
  do i_vars = 1, dwf_new%n_vars
    if(n_vals(i_times,i_vars).eq.24)then
      if(i_vars.eq.index_RelHum)then
        if(vp_flag.eqv.(.true.))then
          T_temp  = dwf_new%var(i_times,index_AirTemp) + 273.15
          es_temp = saturation_vapour_pressure(T_temp,only_water)
          e_temp = dwf_new%var(i_times,i_vars) / float(n_vals(i_times,i_vars))
          rh_temp = e_temp / es_temp * 100.0
          if(rh_temp.gt.100.0)then
            rh_temp = 100.0
          endif  
          dwf_new%var(i_times,index_RelHum) = rh_temp
         else
          dwf_new%var(i_times,i_vars) = dwf_new%var(i_times,i_vars) / float(n_vals(i_times,i_vars))
        endif
      endif  
      if(i_vars.eq.index_GlobRad)then
        dwf_new%var(i_times,i_vars) = dwf_new%var(i_times,i_vars) / float(n_vals(i_times,i_vars))
      endif
      if(i_vars.eq.index_Precip)then
        dwf_new%var(i_times,i_vars) = dwf_new%var(i_times,i_vars)
      endif
      if(i_vars.eq.index_Wind)then
        dwf_new%var(i_times,i_vars) = dwf_new%var(i_times,i_vars) / float(n_vals(i_times,i_vars))
      endif
      if(i_vars.eq.index_Winddir)then
        dwf_new%var(i_times,i_vars) = dwf_new%var(i_times,i_vars) / float(n_vals(i_times,i_vars))
      endif
      if(i_vars.eq.index_Pressure)then
        dwf_new%var(i_times,i_vars) = dwf_new%var(i_times,i_vars) / float(n_vals(i_times,i_vars))
      endif
      if(i_vars.eq.index_Tmin)then
        dwf_new%var(i_times,i_vars) = dwf_new%var(i_times,i_vars)
      endif
      if(i_vars.eq.index_Tmax)then
        dwf_new%var(i_times,i_vars) = dwf_new%var(i_times,i_vars)
      endif
     else
      dwf_new%var(i_times,i_vars) = missing_value
    endif  
  enddo
enddo  
 
! if wind calculations are done via u and vwind, backward calculation has to be done here
if(allocated(uwind))then
  do i_times = 1, dwf_new%n_times
    if(abs(dwf_new%var(i_times,index_Wind)-missing_value).gt.eps)then
      if(abs(dwf_new%var(i_times,index_Winddir)-missing_value).gt.eps)then
        uwind_temp = dwf_new%var(i_times,index_Winddir)
        vwind_temp = dwf_new%var(i_times,index_Wind)
        call uvwind_to_wind(uwind_temp,vwind_temp,windspeed_temp,winddir_temp)
        dwf_new%var(i_times,index_Winddir) = winddir_temp
        dwf_new%var(i_times,index_Wind) = windspeed_temp
      endif
    endif
  enddo
endif  

end subroutine

end module mod_daisy_calc_daily_values
