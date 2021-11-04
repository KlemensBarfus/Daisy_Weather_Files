module mod_remove_variable_from_dwf

! removes a variable from dwf

contains

subroutine remove_variable_from_dwf(dwf,name)
use mod_daisy_weather_type
use mod_find_string_in_array

implicit none

type(daisy_weather_type), intent(inout):: dwf
!real, dimension(1:10), intent(in):: v
character(100), intent(in):: name

integer:: index_variable, i, j
character(100), dimension(:), allocatable:: varnames_temp, varunits_temp
real, dimension(:,:), allocatable:: var_temp

dwf%n_vars = dwf%n_vars+1


index_variable = find_string_in_array(dwf%n_vars, dwf%varnames, name)
if(index_variable.gt.0)then
  if(allocated(varnames_temp))deallocate(varnames_temp)
  allocate(varnames_temp(1:dwf%n_vars))
  if(allocated(varunits_temp)) deallocate(varunits_temp)
  allocate(varunits_temp(1:dwf%n_vars))
  if(allocated(var_temp)) deallocate(var_temp)
  allocate(var_temp(1:dwf%n_times,1:dwf%n_vars))
  j = 1
  do i = 1, dwf%n_vars+1
    if(i.ne.index_variable)then
      varnames_temp(j) = dwf%varnames(i)
      varunits_temp(j) = dwf%varunits(i)
      var_temp(1:dwf%n_times,j) = dwf%var(1:dwf%n_times,i)
      j = j + 1
    endif
  enddo
endif  
if(allocated(dwf%varnames)) deallocate(dwf%varnames)
allocate(dwf%varnames(1:dwf%n_vars))
dwf%varnames = varnames_temp
if(allocated(dwf%varunits)) deallocate(dwf%varunits)
allocate(dwf%varunits(1:dwf%n_vars))
dwf%varunits = varunits_temp
if(allocated(dwf%var)) deallocate(dwf%var)
allocate(dwf%var(1:dwf%n_times,1:dwf%n_vars))
dwf%var = var_temp

end subroutine remove_variable_from_dwf

end module mod_remove_variable_from_dwf
