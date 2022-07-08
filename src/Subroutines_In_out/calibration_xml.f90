!****************************************************************
!  subroutine to read in calibration file if present and set calibration factors to 1 otherwise
!  and/or check if spatial hydrographs are required
!
!****************************************************************


subroutine calibration_xml
	 
use shared_data
use parameters_from_xml
!  	 implicit double precision (a - h, o - z)
!       implicit integer (i - n)
implicit none

character *80 filename, hydro_x_file

logical fexist

integer i
integer x_temp
integer y_temp
integer ioerror

hydro_out = .FALSE.
fexist = .FALSE.
filename = input_folder (1:input_folder_length) // 'calib.dat'
hydro_x_file = input_folder (1:input_folder_length) // 'hypoints.dat'

inquire (file = filename, exist = fexist, err = 3)
   go to 4
   3   continue
   fexist = .FALSE.
   4   continue
   if (fexist) then
      open (56, file = filename, status = 'old')
      rewind (56)
      read (56, *) ksat_mod
      read (56, *) psi_mod
      close (56)
   else
      ksat_mod = 1.0d0
      psi_mod  = 1.0d0
   endif
!JWAug05
!JWAug05   check if spatial hydrographs are required
!JWAug05
inquire (file = hydro_x_file, exist = hydro_out, err = 5)
   go to 6
   5   continue
   hydro_out = .FALSE.
   6   continue
   if (hydro_out) then
      open (56, file = hydro_x_file, status = 'old')
      rewind (56)
      n_hypoints = 0
      do 
         read (56, *, IOSTAT = ioerror) x_temp, y_temp
         if (ioerror.ne.0) then
            exit
         endif
	 n_hypoints = n_hypoints + 1
      enddo
      rewind (56)
      if (n_hypoints.gt.0) then
	 allocate (x_hypoints (n_hypoints))
	 allocate (y_hypoints (n_hypoints))
	 do i = 1, n_hypoints
	    read (56, *) x_hypoints (i), y_hypoints (i)
	    if (x_hypoints (i).lt.2.or.x_hypoints (i).gt.nr1.or. y_hypoints (i).lt.2.or.y_hypoints (i).gt.nc1) then
               write (6, *) ' Warning -- hydrograph output point ', i, ' is out of range and has been ', 'reset to [2, 2] '
	       x_hypoints (i) = 2
	       y_hypoints (i) = 2
	    endif
	 enddo
      else
	 hydro_out = .FALSE.
      endif
      close (56)
      do i = 1, n_hypoints
	 write (6, *) ' Hydrograph output points set as follows: '
	 write (6, '(2 (i4, 1x))') x_hypoints (i), y_hypoints (i)
      enddo
   endif

return
end
