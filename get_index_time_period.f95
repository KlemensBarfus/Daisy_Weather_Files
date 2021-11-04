module mod_get_index_time_period

integer, dimension(:), allocatable:: indexx_time_period
integer:: n_times_period

contains 

subroutine get_index_time_period(n_times,year,month,day,start_year,start_month,start_day,stop_year,stop_month,stop_day)

implicit none

! gets index and number of dates for a given timeperiod

integer, intent(in):: n_times
integer, dimension(1:n_times), intent(in):: year, month, day
integer, intent(in):: start_year, start_month, start_day
integer, intent(in):: stop_year, stop_month, stop_day

integer, dimension(:), allocatable:: index_temp
integer:: n_years_period, n_months_period, i, i_times, j
integer:: sum_index_temp

if(allocated(index_temp)) deallocate(index_temp)
allocate(index_temp(1:n_times))
index_temp = 0

n_years_period = (stop_year - start_year) + 1
if(n_years_period.eq.1)then
  n_months_period = (stop_month - start_month)+1
 else
  n_months_period = ((12-start_month)+1)+(n_years_period-2)*12+stop_month
endif  

do i_times = 1, n_times
  if(n_years_period.eq.1)then
    if(year(i_times).eq.start_year)then
      if(n_months_period.eq.1)then
        if(month(i_times).eq.start_month)then
          if((day(i_times).ge.start_day).and.(day(i_times).le.stop_day))then
            index_temp(i_times) = 1
          endif
        endif
       else  ! n_month > 1
        if(month(i_times).eq.start_month)then
          if(day(i_times).ge.start_day)then
            index_temp(i_times) = 1
          endif
        endif
        if(month(i_times).eq.stop_month)then
          if(day(i_times).le.stop_day)then
            index_temp(i_times) = 1
          endif
        endif
        if((month(i_times).gt.start_month).and.(month(i_times).le.stop_month))then
          index_temp(i_times) = 1
        endif  
      endif ! n_month > 1 
    endif
   else ! > 1 year
    if(year(i_times).eq.start_year)then
      if(month(i_times).ge.start_month)then
        if(month(i_times).eq.start_month)then
          if(day(i_times).ge.start_day)then
            index_temp(i_times) = 1
          endif 
         else
          index_temp(i_times) = 1 
        endif  
      endif
    endif
    if(year(i_times).eq.stop_year)then
      if(month(i_times).le.stop_month)then
        if(month(i_times).eq.stop_month)then
          if(day(i_times).le.stop_day)then
            index_temp(i_times) = 1
          endif 
         else
          index_temp(i_times) = 1 
        endif  
      endif
    endif
    if((year(i_times).gt.start_year).and.(year(i_times).le.stop_year))then
      index_temp(i_times) = 1  
    endif
  endif
enddo  
    
sum_index_temp = 0
do i_times = 1, n_times
  if(index_temp(i_times).eq.1)then
    sum_index_temp = sum_index_temp + 1
  endif
enddo

n_times_period = sum_index_temp
if(n_times_period.ge.1)then
  if(allocated(indexx_time_period)) deallocate(indexx_time_period)
  allocate(indexx_time_period(1:n_times_period))
  j = 1
  do i_times = 1, n_times
    if(index_temp(i_times).eq.1)then
      indexx_time_period(j) = i_times
      j = j + 1
    endif
  enddo
endif

end subroutine get_index_time_period

end module mod_get_index_time_period

  



  
  

