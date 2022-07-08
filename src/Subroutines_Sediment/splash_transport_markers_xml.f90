!****************************************************************
!  subroutine to transport the markers by rainsplash
!
!  JC May 2011
!  JW May 2014 modified to account for xml input changes
!
!****************************************************************

subroutine splash_transport_markers_xml

use shared_data
use mt95
use parameters_from_xml

implicit none

double precision :: splash_across_m, splash_down_m, splash_up_m
double precision :: p_across, p_down, p_up, randn_dir
double precision :: splash_mv_m (4), p_dir (4), splash_dist	
double precision :: ZBQLEXP

integer :: l

!	 Determine probablity marker will be deposited in one of the four cardinal directions
!	 First convert mass of splashed soil (kg) to kg/m2
detach_soil_m (int (MXY (mi, 3)), mycell, mxcell) = detach_soil_m (int (MXY (mi, 3)), mycell, mxcell) / dx_m ** 2    
splash_across_m = 0.25d0 * detach_soil_m (int (MXY (mi, 3)), mycell, mxcell)        
splash_down_m = (0.5d0 * detach_soil_m (int (MXY (mi, 3)), mycell, mxcell)) * &
                (1.0d0 - 0.5d0 * exp (-2.42d0 * (atan (slope (mycell, mxcell) / 1.0d2)) * 57.29577951))
splash_up_m = (0.5d0 * detach_soil_m (int (MXY (mi, 3)), mycell, mxcell)) - splash_down_m

!	 Next convert to proportion of splashed mass to give probability of deposition
p_across = 2.5d-1
p_down = splash_down_m / detach_soil_m (int (MXY (mi, 3)), mycell, mxcell)
p_up = splash_up_m / detach_soil_m (int (MXY (mi, 3)), mycell, mxcell)

!	 Now determine splashed directions
!	 no slope -- distribute in all directions equally
if (slope (mycell, mxcell).eq.0.0d0) then
   do l = 1, 4
      splash_mv_m (l) = p_across
   enddo                       
!	 slope to E
elseif (aspect (mycell, mxcell).eq.2) then
   splash_mv_m (1) = p_down
   splash_mv_m (3) = p_up
   splash_mv_m (2) = p_across
   splash_mv_m (4) = p_across
!      slope to S
elseif (aspect (mycell, mxcell).eq.3) then 
   splash_mv_m (2) = p_down
   splash_mv_m (4) = p_up
   splash_mv_m (1) = p_across
   splash_mv_m (3) = p_across
!	 slope to W
elseif (aspect (mycell, mxcell).eq.4) then
   splash_mv_m (3) = p_down
   splash_mv_m (1) = p_up
   splash_mv_m (2) = p_across
   splash_mv_m (4) = p_across
!	 slope to N
else
   splash_mv_m (4) = p_down
   splash_mv_m (2) = p_up
   splash_mv_m (1) = p_across
   splash_mv_m (3) = p_across
endif         
			
!	 Determine probability of deposition in each cardinal direction, based on the slope direction                    
!	 left
p_dir (1) = splash_mv_m (3)                        
!	 right
p_dir (2) = splash_mv_m (1)                      
!	 down
p_dir (3) = splash_mv_m (2)                     
!	 up
p_dir (4) = splash_mv_m (4)
			                   
!	 Determine splash direction and distance, and move markers

!	 First determine the direction		
call genrand_real1 (randn_dir)

!	 Next determine the splash distance (cm) (see Savat and Poesen, 1981)
!	 Based on an exponential distribution in which the mean distance = 1 / smb
!	 NB. this is not the weighted mean so it not biased by difference in particle mass
splash_dist = ZBQLEXP (1.0d0 / smb (INT (MXY (mi, 3))))
!	 Convert distance in to m
splash_dist = splash_dist / 1.0d2

!	 Now move markers
!	 For movement of markers +ve y is downslope and x is across the slope. In MAHLERAN(0,0) is at top
!	 right of plot - same convention is used here
!	 left
if (randn_dir.le.p_dir (1)) then	
   MXY (mi, 2) = MXY (mi, 2) + splash_dist
!	 right
elseif (randn_dir.gt.p_dir (1).and.randn_dir.le.p_dir (1) + p_dir (2)) then			
   MXY (mi, 2) = MXY (mi, 2) - splash_dist
!	 down
elseif (randn_dir.gt.p_dir (1) + p_dir (2).and.randn_dir.le.p_dir (1) + p_dir (2) + p_dir (3)) then		
   MXY (mi, 1) = MXY (mi, 1) + splash_dist
!	 up
else
   MXY (mi, 1) = MXY (mi, 1) -  splash_dist   
endif

return
end