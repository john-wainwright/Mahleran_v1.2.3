c ************************************************************
c      subroutine to define topographic attributes using 
c      surface topography and water-surface so that sinks  
c      can be overtopped automatically (introduced by JW, 
c      Feb05). Adapted by CJMH Jul13
c
c      Only called if iroute=6.
c ************************************************************
       subroutine dynamic_topog_attribute

       use shared_data
       use parameters_from_xml
c       implicit double precision (a - h, o - z)
c       implicit integer (i - n)
       
       implicit none
       
       double precision zmin, zmin1, z_temp

       integer *4 sdir (4, 2)
       integer *4 asp_temp, i, i1, j, k, k1, nedge, newaspect
       integer *4 lowest_aspect 
       
       data sdir /-1, 0, 1, 0, 0, 1, 0, -1 /
c      sdir() contains four vectors in the N[-1,0], E[0,1],
c      S[1,0] and W[0,-1] directions
c       
       write (6, *) 'In dynamic topographic attribute routine ...'
c
       reorder = .false.
       do i = 2, nr
          do k = 2, nc
c             write (6, *) i, k
             newaspect = 0
             lowest_aspect = 0
             if (z (i, k).le.-9999.d0) then	  
                 slope (i, k) = 0.0d0
             endif

cJWFeb05     Algorithm only defined for ndirn = 4
cJWFeb05
cJWFeb05     flow routed along four cardinal directions 
cJWFeb05     (1='N', 2='E', 3='S', 4='W').
cJWFeb05     Routing based on water-surface elevation
cJWFeb05
cJWDec24             zmin = z (i, k) + d (1, i, k)
cJWDec24
             zmin = z (i, k)
	     zmin1 = 1.d36
c             write (6, *) 'D_T_A_1', i, k, z (i, k), d (1, i, k)
             do j = 1, 4
                 
cJWFeb05        the following if ... then block determines the cell out of the
cJWFeb05        four neighbouring cardinal cells that has the lowest elevation
cJWFeb05        and assigns the processing cell a flow direction of j (1 to 4)
cJWFeb05
cJWDec24                if (z (i + sdir (j, 1), k + sdir (j, 2)) +
cJWDec24     &	         d (1, i + sdir (j, 1), k + sdir (j, 2)).lt.zmin)
cJWDec24     &             then
                z_temp = z (i + sdir (j, 1), k + sdir (j, 2))
                if (z_temp.ne.nodata_value_from_topog.and.
     &              z_temp.lt.zmin) then
                   newaspect = j
                   zmin = z_temp
                endif
                if (z_temp.ne.nodata_value_from_topog.and.
     &              z_temp.lt.zmin1) then
                   zmin1 = z_temp
                   lowest_aspect = j
                endif
                
             
             enddo            
c             write (6, *) 'D_T_A_2', i, k, newaspect, zmin
             if (newaspect.ne.aspect (i, k)) then
                 reorder = .true.
c                 write (6, *) 'D_T_A_2_2', newaspect,
c     &                        i + sdir (newaspect, 1), 
c     &                        k + sdir (newaspect, 2)
                 write (6, *) ' In dynamic_topog_attribute, ',
     &                        'i = ', i, ', k = ', k, ' ',                 
     &                        'newaspect = ', newaspect, ', ',
     &                        'aspect = ', aspect (i, k), ', ',
     &                        'z (i, k) = ', z (i, k), ', ',
     &                        'd (1, i, k) = ', d (1, i, k),
     &                        'z_asp = ', 
     &                        z (i + sdir (newaspect, 1), 
     &                           k + sdir (newaspect, 2)),
     &                        ', d_asp = ', 
     &	                      d (1, i + sdir (newaspect, 1), 
     &                              k + sdir (newaspect, 2)) 
                 aspect (i, k) = newaspect
             endif
c             write (6, *) 'D_T_A_2_3', i, k, aspect (i, k), newaspect
             asp_temp = aspect (i, k)
	     slope (i, k) = (z (i, k) - zmin) / dx
cJWFeb05
cJWFeb05     calculate threshold depth value if cell is a sink
cJWFeb05
             if (aspect (i, k).eq.0) then
                 d_thresh (i, k) = zmin1 - z (i, k)
                 if (d (1, i, k).gt.d_thresh (i, k)) then
                     slope (i, k) = (d (1, i, k) - d_thresh (i, k)) / 
     &                              dx
                     aspect (i, k) = lowest_aspect
                     reorder = .true.
                 endif
             else
                 d_thresh (i, k) = 0.0d0
             endif
c             write (6, *) 'D_T_A_2a', i, k, asp_temp, slope (i, k)
          enddo
       enddo
c
c       write (6, *) 'D_T_A_3'

       do i = 2, nr
          do k = 2, nc 
              if ( ( rmask (i, k).lt.0 ) .or.
     &            ( slope (i, k).gt.1000.d0 ) ) then  
                  slope (i, k) = 0.0d0
              endif
          enddo
       enddo
c       
c       write (6, *) 'D_T_A_4'
       nedge = 0
       do i = 2, nr
          do k = 2, nc
c            prevent oversteepening of edge cells by setting their slope
c            equal to that of the adjacent cell
c
             if ( rmask (i, k).ge.0.0d0 ) then 
                 j = aspect (i, k)
                 i1 = sdir(j,1)
                 k1 = sdir(j,2)
                 if (rmask( i + i1, k + k1 ).lt.0.0d0) then  
                     if (slope (i, k).gt.slope (i - i1, k - k1)
     &                 .or. slope (i, k).eq.0.d0) then
                          slope (i, k) = slope (i - i1, k - k1)
	                  nedge = nedge + 1
	              endif 
                 endif  
             endif
          enddo
       enddo
c
c       write (6, *) 'D_T_A_5'
       if (reorder) then
       write(6, *) '... reordering cells ... '
c           
cCJMHJul13 calculation of contributing areas moved to 
cCJMHJul13 subroutine contrib_area (sdir)
c
c          Calculate number of cells contributing flow to each cell (contrib)
           call contrib_area (sdir)
c      
           ncell1 = 0
           do i = 2, nr
               do k = 2, nc
c
cJWFeb05           ignore areas outside plot
c
                   if (rmask (i, k).ge.0.0d0) then
                       ncell1 = ncell1 + 1
                       order (ncell1, 1) = i
                       order (ncell1 , 2) = k
                       order (ncell1, 3) = contrib (i, k)
                   endif
               enddo
           enddo
c
cCJMH      call subroutine to sort cells in order from up- to down-slope
           call sort (ncell1, order)
           write(6, *) '... completed dynamic topographic attribute ',
     &                 'routine.'
       else
           write(6, *) '... completed dynamic topographic attribute ',
     &                 'routine: no reordering required'
c           
       endif
c       
9999   format ('DSAA')
9990   format ( '[',I2,',',I2,']' )
       
       return
       end