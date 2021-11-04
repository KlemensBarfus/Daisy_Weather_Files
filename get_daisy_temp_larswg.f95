module mod_get_daisy_temp_larswg


contains

subroutine get_daisy_temp_larswg(n_times,year,month,day,t,missing_value,TAverage,doy_max,TAmplitude)
!use mod_leap_year
use mod_adjust_sine_wave
use mod_doy
use mod_get_index_max
implicit none

! assumes years with 365 days !

integer, intent(in):: n_times
integer, dimension(1:n_times), intent(in):: year
integer, dimension(1:n_times), intent(in):: month
integer, dimension(1:n_times), intent(in):: day
real, dimension(1:n_times), intent(in):: t
real, optional:: missing_value
real, intent(out):: TAverage
integer, intent(out):: doy_max
real, intent(out):: TAmplitude

integer:: i, j, n
real:: eps, mv
real:: sum_t
real:: doy_max_temp
integer:: min_year, max_year, n_years, n_years_valid, i_years, rec_year
integer:: n_valid, i_times
integer:: n_doy_max
integer:: start_index, stop_index, index_temp
integer:: year_temp, month_temp, day_temp, doy_temp
logical:: lp, year_valid
real:: phase, amplitude, offset
real, dimension(:), allocatable:: x, y

!real, dimension(1:365):: doy_max_t

eps = 0.00001

if(present(missing_value))then
  mv = missing_value
 else
  mv = -99.999
endif 

! get TAverage
n = 0
sum_t = 0.0
do i = 1, n_times
  if(abs(t(i) - mv).ge.eps)then
    sum_t = sum_t + t(i)
    n = n + 1
  endif
enddo
TAverage = sum_t / float(n)

! get doy of maximum temperature
! number of years
min_year = year(1)
max_year = year(n_times)
n_years = max_year - (min_year - 1)
doy_max_temp = 0.0
n_doy_max = 0
n_years_valid = 0
do i_years = 1, n_years
  rec_year = min_year + (i_years-1)
  ! get start and stop index
  start_index = -9
  stop_index = -9
  do i_times = 1, n_times
    if(year(i_times).eq.rec_year)then
      if(start_index.lt.0)then 
        start_index = i_times
        stop_index = i_times
       else
        stop_index = i_times
      endif
    endif
  enddo
  ! check if all days available
  if(start_index.ge.1)then
    n_valid = 0
    do i_times = start_index, stop_index
      if(abs(t(i_times)-mv).gt.eps)then
        n_valid = n_valid + 1
      endif
    enddo
    year_valid = .false.
    if(n_valid.eq.365)then
      year_valid = .true.
    endif
  endif
  ! get maximum value
  if(year_valid)then
    call get_index_max(stop_index-(start_index-1),t(start_index:stop_index),missing_value)
    doy_temp = 0
    do i_times = 1, n_max
      index_temp = start_index + index_max(i_times)
      year_temp = year(index_temp)
      month_temp = month(index_temp)
      day_temp = day(index_temp)
      doy_temp = doy_temp + doy(year_temp, month_temp, day_temp)
    enddo
    doy_max_temp = doy_max_temp + real(doy_temp)/real(n_max)
    n_years_valid  = n_years_valid + 1
 endif
enddo
doy_max_temp = real(doy_max_temp) / real(n_years_valid) 
doy_max = floor(doy_max_temp)

! get temperature amplitude
! reduce series by not complete starting year and 'leap days'
! get number of days
n = 0
do i_times = 1, n_times
  if(abs(t(i_times)-mv).gt.eps)then
    n = n + 1
  endif  
enddo
if(allocated(x)) deallocate(x)
allocate(x(1:n))
if(allocated(y)) deallocate(y)
allocate(y(1:n))
j = 1
do i_times = 1, n_times
  lp = .false.
  doy_temp = doy(year(i_times), month(i_times), day(i_times), lp)
  if(abs(t(i_times)-mv).gt.eps)then
    x(j) = (year(i_times) - min_year)*365 + doy_temp
    y(j) = t(i_times)
    j = j + 1
  endif  
enddo
phase = doy_max - 91.25
call adjust_sine_wave(n,x,y,phase,amplitude,offset)  

TAmplitude = amplitude


end subroutine get_daisy_temp_larswg

end module mod_get_daisy_temp_larswg
