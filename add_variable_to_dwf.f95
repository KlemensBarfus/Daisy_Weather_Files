module mod_add_variable_to_dwf

! adds a variable to dwf which does not exist before in structure

contains

subroutine add_variable_to_dwf(dwf,v,name,vunit)
use mod_daisy_weather_type

implicit none

type(daisy_weather_type), intent(inout):: dwf
real, dimension(1:dwf%n_times), intent(in):: v
!real, dimension(1:10), intent(in):: v
character(100), intent(in):: name
character(100), intent(in):: vunit

real, dimension(1:dwf%n_times,1:dwf%n_vars):: var_temp
character(100), dimension(1:dwf%n_vars+1):: varnames_temp, varunits_temp


dwf%n_vars = dwf%n_vars+1
varnames_temp(1:dwf%n_vars) = dwf%varnames
varnames_temp(dwf%n_vars+1) = name
var_temp = dwf%var
if(allocated(dwf%varnames)) deallocate(dwf%varnames)
allocate(dwf%varnames(1:dwf%n_vars))
dwf%varnames = varnames_temp
varunits_temp(1:dwf%n_vars) = dwf%varunits
varunits_temp(dwf%n_vars+1) = vunit
if(allocated(dwf%varunits)) deallocate(dwf%varunits)
allocate(dwf%varunits(1:dwf%n_vars))
dwf%varunits = vunit
if(allocated(dwf%var)) deallocate(dwf%var)
allocate(dwf%var(1:dwf%n_times,1:dwf%n_vars))
dwf%var(1:dwf%n_times,1:dwf%n_vars-1) = var_temp
dwf%var(1:dwf%n_times,dwf%n_vars) = v !(1:dwf%n_times)

end subroutine add_variable_to_dwf

end module mod_add_variable_to_dwf
