module mod_analog_approach

integer:: n_sequence


contains 

subroutine analog_approach(n_predictors, predictor_names, predictand, ref, model,missing_value)
use mod_daisy_weather_type
use mod_find_string_in_array
use mod_get_index_min

implicit none

integer, intent(in):: n_predictors
character(100), dimension(1:n_predictors), intent(in):: predictor_names
character(100), intent(in):: predictand
type(daisy_weather_type), intent(inout):: ref, model
real, intent(inout), optional:: missing_value

real, dimension(1:ref%n_vars):: mean_ref, std_ref
real, dimension(1:ref%n_times):: diff_all
integer, dimension(1:ref%n_times):: index_chosen

real, dimension(1:n_sequence):: diff_seq
integer, dimension(1:n_sequence):: n_diff_seq

logical, dimension(model%n_times):: gap,  predictors_model_avail
logical, dimension(model%n_times,n_predictors):: predictor_model_avail
logical, dimension(ref%n_times, n_predictors):: predictor_ref_avail
logical, dimension(ref%n_times):: predictand_ref_avail
logical, dimension(ref%n_times):: predictors_ref_avail
real, dimension(1:1):: weights_sequence
real:: r_nan
logical:: isnan



integer:: i1_seq, i2_seq
integer:: i_vars_model, index_search_model, i_times_model, i_seq
real:: eps, diff_temp
integer::  i_times_ref, index_search_ref, i_vars_ref, n_times_temp
integer:: i_times_temp
integer, dimension(1:n_predictors):: index_predictors_model, index_predictors_ref
integer:: n_vars_avail
integer:: n_of_sequence
integer:: n_predictors_needed
integer:: n_simul
integer:: i_predictors
real:: rn
integer:: index_min_final, index_min_temp    
integer:: i_seq_weights

            

! makes the analog appraoch
!function analog_approach_with_ref_seq, model, ref, search_parameter, weights_sequence, n_predictor_needed


!; uses the analog approach to fill up gaps
!; weights_sequence length defines weights of the sequence and with it the sequence length
!; n_predictor_needed defines the number of predictors needed by model as well as ref to fill the gap
!; data are not from the station itself but from the ref station
!; search parameter defines the parameter which shall be filled up
!; input (model and ref) are structures = {name, lat, lon, missing_value, year, month, day, varnames, parameter_units, n_vars, n_times, var[n_times,n_vars]}


n_predictors_needed = 3 !4

if(.not.present(missing_value))then
  missing_value = -99.0
endif  
eps = 0.0000001

n_sequence = 1 !3 !n_elements(weights_sequence)  ! lenghth of the sequence
weights_sequence = (/1.0/) !(/1.0,1.0,1.0/)
i1_seq = ((n_sequence-1) / 2) * (-1)
i2_seq = ((n_sequence-1) / 2) * 1

!gap = replicate(0, model%n_times)         ! indicator if gap exists
!predictor_model = replicate(0, model%n_times) ! indicator if predictor 
!predictor_ref = replicate(0, ref%n_times)  
!predictand_ref = replicate(0, ref%n_times)





! relate_indexes
index_search_model = find_string_in_array(model%n_vars,model%varnames,predictand) ! index index for predictand model
index_search_ref = find_string_in_array(ref%n_vars,ref%varnames,predictand)     ! index for predictand ref
do i_predictors = 1, n_predictors
  index_predictors_model(i_predictors) = find_string_in_array(model%n_vars,model%varnames,predictor_names(i_predictors))    
  index_predictors_ref(i_predictors) = find_string_in_array(ref%n_vars,ref%varnames,predictor_names(i_predictors))    
enddo       

! check the gaps due to predictand model
do i_times_model = 1, model%n_times
  if(abs(model%var(i_times_model,index_search_model)-missing_value).lt.eps)then
    gap(i_times_model) = .true.
   else
    gap(i_times_model) = .false. 
  endif
enddo
    
! check available predictors for the model
! check for minimum number of needed predictors as well as sequence length
do i_times_model = 1, model%n_times
  if((i_times_model + i1_seq.ge.1).and.(i_times_model + i2_seq.le.model%n_times))then ! skip the edges
    n_vars_avail = 0
    do i_predictors = 1, n_predictors         ! run through models
      n_of_sequence = 0
      do i_seq = i1_seq, i2_seq  ! run through sequence
        i_times_temp = i_times_model + i_seq
        if(abs(model%var(i_times_temp,index_predictors_model(i_predictors)) - missing_value).gt.eps)then
          n_of_sequence = n_of_sequence + 1   
        endif
      enddo
      if(n_of_sequence.eq.n_sequence)then 
        predictor_model_avail(i_times_model,i_predictors) = .true.
        n_vars_avail = n_vars_avail + 1
       else
        predictor_model_avail(i_times_model,i_predictors) = .false.
      endif   
    enddo
    if(n_vars_avail.ge.n_predictors_needed)then
      predictors_model_avail(i_times_model) = (.true.)
     else
      predictors_model_avail(i_times_model) = (.false.)
    endif  
   else
    predictors_model_avail(i_times_model) = (.false.)
  endif  
enddo      

              
! check available predictand for the reference
do i_times_ref = 1, ref%n_times
  if((i_times_ref + i1_seq.ge.1).and.(i_times_ref + i2_seq.le.ref%n_times))then ! skip the edges
    if(abs(ref%var(i_times_ref,index_search_ref) - missing_value).gt.eps)then
      predictand_ref_avail(i_times_ref) = (.true.)
     else
      predictand_ref_avail(i_times_ref) = (.false.) 
    endif
  endif
enddo

! check available predictors for the ref
do i_times_ref = 1, ref%n_times
  if((i_times_ref + i1_seq.ge.1).and.(i_times_ref + i2_seq.le.ref%n_times))then ! skip the edges
    n_vars_avail = 0
    do i_predictors = 1, n_predictors         ! run through models
      n_of_sequence = 0
      do i_seq = i1_seq, i2_seq  ! run through sequence
        i_times_temp = i_times_ref + i_seq
        if(abs(ref%var(i_times_temp,index_predictors_ref(i_predictors)) - missing_value).gt.eps)then
          n_of_sequence = n_of_sequence + 1   
        endif
      enddo
      if(n_of_sequence.eq.n_sequence)then 
        predictor_ref_avail(i_times_ref,i_predictors) = .true.
        n_vars_avail = n_vars_avail + 1
       else
        predictor_ref_avail(i_times_ref,i_predictors) = .false.
      endif   
    enddo
    if(n_vars_avail.ge.n_predictors_needed)then
      predictors_ref_avail(i_times_ref) = .true.
     else
      predictors_ref_avail(i_times_ref) = .false.
    endif
   else
    predictors_ref_avail(i_times_ref) = .false.
  endif  
enddo      




! normalize the ref data
! calculate mean and standard deviation 
do i_vars_ref = 1, ref%n_vars
  mean_ref(i_vars_ref) = 0.00
  std_ref(i_vars_ref) = 0.00
enddo
do i_vars_ref = 1, ref%n_vars
   n_times_temp = 0
   do i_times_ref = 1, ref%n_times
      if(abs(ref%var(i_times_ref,i_vars_ref)  - missing_value).gt.eps)then       ! check for missing values
        mean_ref(i_vars_ref) = mean_ref(i_vars_ref) + ref%var(i_times_ref,i_vars_ref)
        n_times_temp = n_times_temp + 1
      endif
   enddo
   mean_ref(i_vars_ref) = mean_ref(i_vars_ref) / float(n_times_temp)
   n_times_temp = 0
   do i_times_ref = 1, ref%n_times
      if(abs(ref%var(i_times_ref,i_vars_ref)  - missing_value).gt.eps)then       ! check for missing values
        std_ref(i_vars_ref) = std_ref(i_vars_ref) + (ref%var(i_times_ref,i_vars_ref) - mean_ref(i_vars_ref))**2
        n_times_temp = n_times_temp + 1
      endif
   enddo
   std_ref(i_vars_ref)  =  sqrt(std_ref(i_vars_ref)  / float(n_times_temp))
enddo

! normalize ref data   
do i_vars_ref = 1, ref%n_vars
  if(i_vars_ref.ne.index_search_ref)then
    do i_times_ref = 1, ref%n_times
      if(abs(ref%var(i_times_ref,i_vars_ref) - missing_value).gt.eps)then
        ref%var(i_times_ref,i_vars_ref) = (ref%var(i_times_ref,i_vars_ref) - mean_ref(i_vars_ref)) / std_ref(i_vars_ref)
      endif
    enddo  
  endif
enddo


! normalize model data
do i_times_model = 1, model%n_times
  do i_vars_model = 1, model%n_vars
    if(i_vars_model.ne.index_search_model)then  ! skip the search variable
      if(abs(model%var(i_times_model,i_vars_model) - missing_value).gt.eps)then  ! valid value
         model%var(i_times_model,i_vars_model) = (model%var(i_times_model,i_vars_model) - &
             mean_ref(i_vars_model))/std_ref(i_vars_model)
      endif
    endif
  enddo
enddo


! run through the model data
do i_times_model = 1, model%n_times
  index_chosen(i_times_model) = -9
enddo  
do i_times_model = 1, model%n_times     ! run through model timesteps
  if(mod(i_times_model, 100).eq.0)then
    print *, i_times_model, ' ', model%n_times
  endif 
  if(gap(i_times_model))then                                                      ! gap exists
    if(predictors_model_avail(i_times_model))then                           ! predictors from model are available
      do i_times_ref = 1, ref%n_times
        diff_all(i_times_ref) = r_nan()
      enddo  
      do i_times_ref = 1, ref%n_times
        if(predictand_ref_avail(i_times_ref))then                              ! predictand from reference available
          if(predictors_ref_avail(i_times_ref))then                              ! predictor from reference available
            ! #########################################################################
            ! check number of simultaneous predictors
            n_simul = 0
            do i_predictors = 1, n_predictors
              if(predictor_ref_avail(i_times_ref,i_predictors))then
                if(predictor_model_avail(i_times_model,i_predictors))then
                  n_simul = n_simul + 1
                endif
              endif
            enddo
            ! #############################################################################
            if(n_simul.ge.n_predictors_needed)then   
              diff_seq = 0.0000
              n_diff_seq = 0
              do i_predictors = 1, n_predictors
                if(predictor_model_avail(i_times_model,i_predictors))then
                  if(predictor_ref_avail(i_times_ref,i_predictors))then
                    do i_seq = i1_seq, i2_seq
                      i_seq_weights = i_seq + abs(i1_seq) + 1                      
                      diff_temp = (abs(model%var(i_times_model+i_seq,index_predictors_model(i_predictors)) - ref%var(i_times_ref+ &
                                       i_seq,index_predictors_ref(i_predictors)))) * weights_sequence(i_seq_weights)
                      if(isnan(diff_all(i_times_ref)))then
                        diff_all(i_times_ref) = diff_temp
                       else                 
                        diff_all(i_times_ref) = diff_all(i_times_ref) + diff_temp
                      endif  
                      n_diff_seq = n_diff_seq + 1
                    enddo
                  endif
                endif
              enddo
              !diff_all(i_times_ref) = diff_all(i_times_ref) / float(n_diff_seq)
            endif
          endif
        endif
      enddo
      ! analyse this circle
      ! find minimum
      !real:: min_val
      !integer:: n_min  ! number of minimum values
      !integer, dimension(:), allocatable:: index_min
      call get_index_min(ref%n_times,diff_all)
      !print *, index_min 
      if(n_min.gt.1)then
        call random_seed()
        call random_number(rn)
        index_min_temp = ceiling(rn*float(n_min))
        index_min_final = index_min(index_min_temp)
       else
        index_min_final = index_min(1)
      endif  
      model%var(i_times_model,index_search_model) = ref%var(index_min_final,index_search_ref)
    endif
  endif                 
enddo

! renormalize ref data   
do i_vars_ref = 1, ref%n_vars
  if(i_vars_ref.ne.index_search_ref)then
    do i_times_ref = 1, ref%n_times
      if(abs(ref%var(i_times_ref,i_vars_ref) - missing_value).gt.eps)then
        ref%var(i_times_ref,i_vars_ref) = ref%var(i_times_ref,i_vars_ref) * std_ref(i_vars_ref) + mean_ref(i_vars_ref)
      endif
    enddo  
  endif
enddo


! renormalize model data
do i_times_model = 1, model%n_times
  do i_vars_model = 1, model%n_vars
    if(i_vars_model.ne.index_search_model)then  ! skip the search variable
      if(abs(model%var(i_times_model,i_vars_model) - missing_value).gt.eps)then  ! valid value
         model%var(i_times_model,i_vars_model) = model%var(i_times_model,i_vars_model) * &
             std_ref(i_vars_model) + mean_ref(i_vars_model)
      endif
    endif
  enddo
enddo

!return, model                      
  
end subroutine analog_approach

end module mod_analog_approach
