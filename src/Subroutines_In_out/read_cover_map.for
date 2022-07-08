c****************************************************************
c  subroutine to read cover type
c
c  cover type 1: vegetated (shrub or grass cover)
c  cover type 2: bare
c
c****************************************************************
       subroutine read_cover_map
c
cJWMay 2005 now uses .asc format which uses single input file with following header lines:
c
c       ncols [x] 
c       nrows [y]
c       xllcorner [xmin]
c       yllcorner [ymin]
c       cellsize [dx]
c       nodata_value [nodata] 
c
c  followed by row 1 etc. of the data
cJWMay 2005

	 use shared_data
  	 implicit double precision (a - h, o - z)
       implicit integer (i - n)
       character *14 a_col, a_row, a_dx, a_miny, a_minx
     
c
c opens the *.asc file of the cover map
cEva	cover=1 vegetated surface cover, cover=2 bare surface cover	
c
       open (3, FILE=pfadin(1:pfadi)// cover_file, status = 'unknown')
       rewind (3)
		
c
c   read header
c
       read (3, 9999) a_col, ncol1
	 read (3, 9999) a_row, nrw1
	 read (3, 9998) a_minx, xmin1
       read (3, 9998) a_miny, ymin1
	 read (3, 9997) a_dx, dx1
	 read (3, 9996)
	 dx1 = dx1 * 1000.0
	 if (ncol1.ne.nc1.or.nrw1.ne.nr1.or.xmin1.ne.xmin.or.
     &     ymin1.ne.ymin.or.dx1.ne.dx) then
	    write (6, *) ' Warning -- cover file header in ', 
     &                 cover_file, ' does not match topography file ',
     &                 'header in ', topo_file
	 endif

       do i = 1, nr1
          read (3, *) (cover (i, k), k = 1, nc1)
       enddo

       close (3)

       write (6, *) 
       write (6, *) ' Cover data read in '

9999   format (a6, i10)
9998   format (a10, f15.8)
9997   format (a9, f15.8)
9996   format (a13)

       return
       end
