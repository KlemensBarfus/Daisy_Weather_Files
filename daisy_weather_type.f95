module mod_daisy_weather_type

type daisy_weather_type
   integer:: n_comments
   character(150), dimension(:), allocatable:: comments
   character(100):: station
   real:: elevation !Elevation: 50 m
   real:: longitude
   character(100):: longitude_unit !Longitude: 12 dgEast
   real:: latitude
   character(100):: latitude_unit !Latitude: 51 dgNorth
   integer:: timezone
   character(100):: timezone_unit !TimeZone: 15 dgEast
   character(100):: surface !Surface: ref
   integer:: screen_height !ScreenHeight: 2 m
   integer:: start_year
   integer:: start_month
   integer:: start_day
   !character(100):: begin_string !Begin: 1953-01-01
   integer:: stop_year
   integer:: stop_month
   integer:: stop_day
   !character(100):: end_string !End: 1990-12-31
   real:: NH4WetDep
   character(100):: NH4WetDep_unit !NH4WetDep: 0.900000 ppm
   real:: NH4DryDep
   character(100):: NH4DryDep_unit !NH4DryDep: 2.20000 kgN/ha/year
   real:: NO3WetDep
   character(100):: NO3WetDep_unit !NO3WetDep: 0.300000 ppm
   real:: NO3DryDep
   character(100):: NO3DryDep_unit !NO3DryDep: 1.10000 kgN/ha/year
   real:: TAverage
   !character(100):: TAverage_string !TAverage: 7.97144 dgC
   real:: TAmplitude
   !character(100):: TAmplitude_string !TAmplitude: 36.7526 dgC
   integer:: MaxTDay
   !character(100):: MaxTDay_string !MaxTDay: 200 yday
   character(100):: line_string !------------------------------------------------------------------------------
   integer:: n_vars   
   character(100), dimension(:), allocatable:: varnames
   character(100), dimension(:), allocatable:: varunits
   integer:: n_times
   integer, dimension(:), allocatable:: year
  integer, dimension(:), allocatable:: month
  integer, dimension(:), allocatable:: day
  integer, dimension(:), allocatable:: hour
  real, dimension(:,:), allocatable:: var
endtype daisy_weather_type

end module mod_daisy_weather_type
