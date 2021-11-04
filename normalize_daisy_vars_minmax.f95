module mod_normalize_daisy_vars_minmax

contains

subroutine normalize_daisy_vars_minmax(n_times, n_vars, vars, min_val, max_val, missing_value)
use mod_find_string_in_array

implicit none

! makes a normalization of daisy vars
! due to Coppola et al. (2006)
! input is

integer, intent(in):: n_times 
integer, intent(in):: n_vars
real, dimension(1:n_times,1:n_vars), intent(inout):: vars
real, dimension(1:n_vars), intent(inout):: min_val
real, dimension(1:n_vars), intent(inout):: max_val
real, intent(in), optional:: missing_value  ! missing value

integer:: i_vars, i_times
real:: eps
logical:: first
real:: mv


eps = 0.0000001
if(present(missing_value))then
  mv = missing_value
 else
  mv = -99.0
endif  

! get min and max value
min_val = missing_value
max_val = missing_value 
do i_vars = 1, n_vars
  first = .true.
  do i_times = 1, n_times
    if(present(missing_value))then
      if(abs(vars(i_times,i_vars)-missing_value).gt.eps)then
        if(first.eqv.(.true.))then
          min_val(i_vars) = vars(i_times,i_vars)
          max_val(i_vars) = vars(i_times,i_vars)
          first = .false.
         else
          if(vars(i_times,i_vars).lt.min_val(i_vars))then
            min_val(i_vars) = vars(i_times,i_vars)
          endif  
          if(vars(i_times,i_vars).gt.max_val(i_vars))then
            max_val(i_vars) = vars(i_times,i_vars)
          endif  
        endif
      endif  
     else
      if(first.eqv.(.true.))then
        min_val(i_vars) = vars(i_times,i_vars)
        max_val(i_vars) = vars(i_times,i_vars)
        first = .false.
       else
        if(vars(i_times,i_vars).lt.min_val(i_vars))then
          min_val(i_vars) = vars(i_times,i_vars)
        endif  
        if(vars(i_times,i_vars).gt.max_val(i_vars))then
          max_val(i_vars) = vars(i_times,i_vars)
        endif  
      endif
    endif     
  enddo
enddo

! normalize
do i_vars = 1, n_vars
  do i_times = 1, n_times
    if(present(missing_value))then
      if(abs(vars(i_times,i_vars)-missing_value).gt.eps)then
        vars(i_times,i_vars) = (vars(i_times,i_vars)-min_val(i_vars))/(max_val(i_vars)-min_val(i_vars))
      endif
    endif
  enddo
enddo
  
end subroutine normalize_daisy_vars_minmax

end module mod_normalize_daisy_vars_minmax 
