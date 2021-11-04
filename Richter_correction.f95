module mod_Richter_correction

contains 

real function Richter_correction(precip, month, region, lev_prot, snow_index)

implicit none

real, intent(inout)::  precip
integer, intent(in):: month
integer, intent(in):: region
integer, intent(in), optional:: lev_prot
real, intent(in), optional:: snow_index ! between 0 and 1 with 1=snow and 0=rain
real, dimension(4,12,9):: mean_monthly_corr
real, dimension(1:12):: m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16
real, dimension(1:3,1:10)::exp_fun_data
real, dimension(1:10):: exp_fun_data1, exp_fun_data2, exp_fun_data3
real:: corr_fact, prec_corr, corr_BVS, corr_BVW, corr_BV, a_WR
real:: b_WR, a_WS, b_WS, corr_WR, corr_W, corr_WS


! Correction of daily precipitation after RICHTER.
! Source: D. Richter (1995): Ergebnisse methodischer Untersuchungen zur
! Korrektur des systematischen MeÃ?fehlers des Hellmann-Niederschlagsmessers.
! Berichte des Deutschen Wetterdienstes 194.
! LEV_PROT:    level of protection of the station, has the same length as the
!              number of columns in PREC, 1>free, 2>slight protection,
!              3>moderate protection, 4>strong protection
!              most often 1
! REGION:      indicates the region after Richter, where the same correction
!              factors are applied, has the same length as the number of
!              columns in PREC, for Saxony choose 3 (north) or 5 (south)
! SNOW_IDX:    index of the snow partition - 0>rain - 1>snow
!              0.3>70 % rain and 30 % snow; has the same dimensions as PREC; if
!              not given, mean monthly correction will be performed

! mean monthly correction factors
! region	levprot	jan	feb	mar	apr	may	jun	jul	aug	sep	oct	nov	dec
! region 1,6	1	22.8	23.6	20	16	12	10.3	10.5	10.3	11.5	13.6	16.2	18.9
! <700 mNN	2	17.3	17.9	15.5	13.6	10.8	9.2	9.4	9.3	10.2	11.2	12.9	14.6
!		3	13.4	13.7	12.6	11.6	9.8	8.4	8.5	8.4	9.1	9.7	10.6	11.6
!		4	9.5	9.6	9.4	9.4	8.5	7.3	7.5	7.3	7.8	7.8	8	8.4
! region 2,4,7	1	27.5	29	23.6	18.2	12.3	10.3	10.5	10.5	12.1	14.2	19.1	22.7
! <700 mNN	2	20.5	21.5	17.8	15	10.9	9.3	9.4	9.5	10.9	11.6	15	17.3
!		3	15.2	15.8	14	12.4	9.8	8.3	8.6	8.6	9.6	10.2	12	13.2
!		4	10.3	10.7	10	9.6	8.5	7.3	7.5	7.5	8.2	8.2	8.7	9.2
! region 3,5	1	31.6	33.5	26.9	18.3	12.5	10.4	10.8	10.5	12.6	15.5	21.8	26.5
! <700 mNN	2	23.3	24.5	20.3	15.1	11.1	9.8	10	9.5	11.5	12.7	16.8	19.8
! *Saxony	3	17.3	17.9	15.5	12.7	10.1	8.8	9.1	8.5	10.2	11	13.3	15
!		4	11.5	11.8	10.7	10	8.6	7.7	8	7.5	8.7	8.8	9.5	10.3
! region 8,9	1	31.7	30.5	25.6	18.8	10.4	8.1	7.9	8.2	9.6	13.4	21.3	26.9
! <1000 mNN	2	23	22.2	19.4	15	9	7.2	7.1	7.3	8.6	10.6	16	19.7
!		3	16.2	15.7	14.3	11.9	8	6.5	6.3	6.6	7.7	8.8	12.1	14.4
!		4	10.6	10.2	9.6	8.7	6.7	5.7	5.6	5.8	6.5	6.8	8.3	9.5



! mean monthly correction factors
! region	levprot	jan	feb	mar	apr	may	jun	jul	aug	sep	oct	nov	dec
m1 =  (/ 22.8,23.6,20.0,16.0,12.0,10.3,10.5,10.3,11.5,13.6,16.2,18.9/)
m2 =  (/ 17.3,17.9,15.5,13.6,10.8, 9.2, 9.4, 9.3,10.2,11.2,12.9,14.6/)
m3 =  (/ 13.4,13.7,12.6,11.6, 9.8, 8.4, 8.5, 8.4, 9.1, 9.7,10.6,11.6/)
m4 =  (/  9.5, 9.6, 9.4, 9.4, 8.5, 7.3,	7.5, 7.3, 7.8, 7.8, 8.0, 8.4/)
m5 =  (/ 27.5,29.0,23.6,18.2,12.3,10.3,10.5,10.5,12.1,14.2,19.1,22.7/)
m6 =  (/ 20.5,21.5,17.8,15.0,10.9, 9.3,	9.4, 9.5,10.9,11.6,15.0,17.3/)
m7 =  (/ 15.2,15.8,14.0,12.4, 9.8, 8.3,	8.6, 8.6, 9.6,10.2,12.0,13.2/)
m8 =  (/ 10.3,10.7,10.0, 9.6, 8.5, 7.3,	7.5, 7.5, 8.2, 8.2, 8.7, 9.2/)
m9 =  (/ 31.6,33.5,26.9,18.3,12.5,10.4,10.8,10.5,12.6,15.5,21.8,26.5/)
m10 = (/ 23.3,24.5,20.3,15.1,11.1, 9.8,10.0, 9.5,11.5,12.7,16.8,19.8/)
m11 = (/ 17.3,17.9,15.5,12.7,10.1, 8.8,	9.1, 8.5,10.2,11.0,13.3,15.0/)
m12 = (/ 11.5,11.8,10.7,10.0, 8.6, 7.7,	8.0, 7.5, 8.7, 8.8, 9.5,10.3/)
m13 = (/ 31.7,30.5,25.6,18.8,10.4, 8.1,	7.9, 8.2, 9.6,13.4,21.3,26.9/)
m14 = (/ 23.0,22.2,19.4,15.0, 9.0, 7.2,	7.1, 7.3, 8.6,10.6,16.0,19.7/)
m15 = (/ 16.2,15.7,14.3,11.9, 8.0, 6.5,	6.3, 6.6, 7.7, 8.8,12.1,14.4/)
m16 = (/ 10.6,10.2, 9.6, 8.7, 6.7, 5.7,	5.6, 5.8, 6.5, 6.8, 8.3, 9.5/)

mean_monthly_corr(1,1:12,1) = m1
mean_monthly_corr(1,1:12,6) = m1
mean_monthly_corr(2,1:12,1) = m2
mean_monthly_corr(2,1:12,6) = m2
mean_monthly_corr(3,1:12,1) = m3
mean_monthly_corr(3,1:12,6) = m3
mean_monthly_corr(4,1:12,1) = m4
mean_monthly_corr(4,1:12,6) = m4

mean_monthly_corr(1,1:12,2) = m5
mean_monthly_corr(1,1:12,4) = m5
mean_monthly_corr(1,1:12,7) = m5
mean_monthly_corr(2,1:12,2) = m6
mean_monthly_corr(2,1:12,4) = m6
mean_monthly_corr(2,1:12,7) = m6
mean_monthly_corr(3,1:12,2) = m7
mean_monthly_corr(3,1:12,4) = m7
mean_monthly_corr(3,1:12,7) = m7
mean_monthly_corr(4,1:12,2) = m8
mean_monthly_corr(4,1:12,4) = m8
mean_monthly_corr(4,1:12,7) = m8

mean_monthly_corr(1,1:12,3) = m9
mean_monthly_corr(1,1:12,5) = m9
mean_monthly_corr(2,1:12,3) = m10
mean_monthly_corr(2,1:12,5) = m10
mean_monthly_corr(3,1:12,3) = m11
mean_monthly_corr(3,1:12,5) = m11
mean_monthly_corr(4,1:12,3) = m12
mean_monthly_corr(4,1:12,5) = m12

mean_monthly_corr(1,1:12,8) = m13
mean_monthly_corr(1,1:12,9) = m13
mean_monthly_corr(2,1:12,8) = m14
mean_monthly_corr(2,1:12,9) = m14
mean_monthly_corr(3,1:12,8) = m15
mean_monthly_corr(3,1:12,9) = m15
mean_monthly_corr(4,1:12,8) = m16
mean_monthly_corr(4,1:12,9) = m16

exp_fun_data1=(/0.20215,0.11552,0.18248,0.14027,0.11096,0.05752,0.99861,0.78758,0.53155,0.20719/)
exp_fun_data2=(/0.37001,0.41481,0.42142,0.45721,0.40954,0.38400,0.61991,0.50959,0.46612,0.69029/)
exp_fun_data3=(/0.470,0.300,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/)
exp_fun_data(1,1:10) = exp_fun_data1
exp_fun_data(2,1:10) = exp_fun_data2
exp_fun_data(3,1:10) = exp_fun_data3


if(present(snow_index).eq.(.false.))then
  ! mean monthly correction
  corr_fact = mean_monthly_corr(lev_prot,month,region)
  prec_corr = precip+precip*corr_fact/100.0
 else
  ! correction of wetting and evaporation loss
  corr_BVS=exp_fun_data(1,1)*precip**exp_fun_data(2,1)
  if(corr_BVS.gt.exp_fun_data(3,1))then
    corr_BVS=exp_fun_data(3,1)
  endif
  corr_BVW=exp_fun_data(1,2)*precip**exp_fun_data(2,2)
  if(corr_BVW.gt.exp_fun_data(3,2))then  
    corr_BVW=exp_fun_data(3,2)
  endif
  corr_BV=corr_BVW
  if((month.ge.5).and.(month.le.10))then
    corr_BV=corr_BVS
  endif  
  a_WR=exp_fun_data(1,2+lev_prot)
  b_WR=exp_fun_data(2,2+lev_prot)
  a_WS=exp_fun_data(1,6+lev_prot)
  b_WS=exp_fun_data(2,6+lev_prot)
  corr_WR= a_WR *precip**b_WR
  corr_WS = a_WS *precip**b_WS
  corr_W = (1.0 - snow_index) *corr_WR + snow_index * corr_WS !weighted mean of WR and WS
  prec_corr=precip+corr_BV+corr_W
endif

Richter_correction = prec_corr

end function

end module

