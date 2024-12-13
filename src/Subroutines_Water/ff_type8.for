       subroutine ff_type8( i, k, friction_fact, depth )
       use shared_data
       implicit double precision (a - h, o - z)
       implicit integer (i - n)
       double precision friction_fact, depth
c      ff equations for sandy soils Based on Lawrence (1997) ESPL
c      Author: Caspar Hewett - adapted from James Cooper's MIC code
c      Created on 18 July 2013
c     
c      Calculate d50_hc	
c       write(6,*) 'ff type = 8'
       dsum = 0.0d0      
       dsumlast = 0.0d0
       do phi = 1, 6			
          dsum = dsum + sed_propn (phi, i, k)   
          if (dsum.ge.0.5d0.and.dsumlast.lt.0.5d0) then  
              if (phi.eq.1) then  
                  d50_hc = (diameter (1) / dsum) * 0.5d0   
              else    
                  d50_hc = diameter (phi - 1) + (0.5d0 - dsumlast) *
     &            (diameter (phi) - diameter (phi - 1)) 
     &              / (dsum - dsumlast)        
              endif        
              exit     
          endif           
          dsumlast = dsum        
       enddo	    
cJCJun11 Calculate the characteristic roughness scale
			
       hc = d50_hc / 2.0d0
      
cJC    Calculate the proportion of the surface covered by the characteristic 
cJC    roughness scale. Lawrence uses the proportion of the surface covered 
cJC    by the largest grains but this is a small value in the Tsukuba 
cJC    experiments. In Lawrence hc = d50/2 so this makes more sense.		
cJC    The variable d50_hc rather than d50 is used because there were issues 
cJC    in sharing d50 in route_water and route_sediment - reason is not known
	
       do phi = 1, 5								
          if (d50_hc.gt.diameter (phi).and.d50_hc.le.
     &			diameter (phi + 1)) then			
              hc_propn = sed_propn (phi + 1, i, k)			
              exit    			
          elseif (d50_hc.gt.diameter (phi + 1)) then			
              hc_propn = sed_propn (6, i, k)			
          else				
              hc_propn = sed_propn (1, i, k)			
          endif			
       enddo
cJC    Calculate the degree of inundation of the surface in dimensionless form			
       delta_inund = (depth / 1.0d3) / hc
cJC    Define the flow condition		
       if (depth.gt.0.0d0.and.delta_inund.le.1.0d0) then
cJC    ... there is partial inundation (Cd = 1)
          friction_fact = (8.0d0 / pi) * hc_propn * 
     &                 min ((pi / 4.0d0), delta_inund)		
       elseif (delta_inund.ge.1.0d0.and.delta_inund.lt.10.0d0) then
cJC   ... there is marginal inundation		
           friction_fact = min ((8.0d0 / pi) * hc_propn * 
     &                 min ((pi / 4.0d0), delta_inund), 
     &                 10.0d0 / (delta_inund ** 2.0d0))	
       elseif (delta_inund.ge.10.0d0) then
cJC   ...there is full inundation		
          friction_fact = max (10.0d0 / (delta_inund ** 2.0d0), 
     &	( (1.64d0 + (0.803d0 * log (delta_inund))) ** (-2.0d0) ) )		
       elseif (depth.le.0.0d0) then
cJC       Constrain maximum value of ff
cJC       A value is needed else calculation of v (i, k) reports an error			
          friction_fact = 14.0d0				
       endif
cJC    Constrain minimum value of ff according to expected Reynolds number
cJC    (see Nearing et al., 1997 WRR and Mugler et al., 2011 JofH)
cJC    NB. a lower number is likely to cause numerical instabilities		
       if (friction_fact.lt.0.2d0) then			
          friction_fact = 0.2d0		
       elseif (friction_fact.gt.14.0d0) then
          friction_fact = 14.0d0
       endif
c       write(6,*) 'friction_fact =',friction_fact
       
       return
       end


