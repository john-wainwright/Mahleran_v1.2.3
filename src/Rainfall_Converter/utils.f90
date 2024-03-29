module utils_h

contains
INTEGER FUNCTION GetNumberOfSubstrings(string)
!@ This funtion returns the number of substrings within a string. @
!@ Valid substring separators are one or more blanks " " or tabs  @
!@ characters (char(9)).                                          @

implicit none
!INTENT(IN)
character(len=*),intent(in):: string
!LOCAL
integer:: i,n
!CODE
!If string length is zero
if (len(string) .eq. 0) then
  n= 0
!If string has only one character
else if (len(string) .eq. 1) then
  if ((string .eq. " ").or.(string .eq. char(9))) then
    n= 0
  else
    n= 1
  end if
!If string has more than one character
else
  n= 0
  !Account for valid character at first position
  if ((string(1:1) .ne. " ") .and. (string(1:1) .ne. char(9))) then
    n= n + 1
  end if
  !Account for subsequent substrings
  do i=1,(len(string)-1)
    if (((string(i:i) .eq. " ") .or. (string(i:i) .eq. char(9))) .and. &
       ((string(i+1:i+1) .ne. " ") .and. (string(i+1:i+1) .ne. char(9)))) then
      n= n + 1
    end if
  end do
end if
GetNumberOfSubstrings= n
END FUNCTION GetNumberOfSubstrings



INTEGER FUNCTION id_ext2int(ext_id, id_array)

	! converts external IDs (used in the input files) to internal numbering scheme
	! 2005-06-29, Till Francke
	!ii pointer statt array �bergeben (loc)


	IMPLICIT NONE

	INTEGER, INTENT(IN):: ext_id		!external ID that is to be converted into internal numbering scheme
	INTEGER, DIMENSION(:), INTENT(IN):: id_array		!lookup array containing the external IDs in the order corresponding to their internal numbering scheme

	INTEGER	:: i		!auxiliary variables


	DO i=1,size(id_array)					!search entire array for occurence of ext_id
		if (id_array(i)==ext_id) then
			id_ext2int=i					!external ID found, return internal position
			return
		end if
	END DO

	id_ext2int=-1					!no matchin external ID found
	return

END FUNCTION id_ext2int


function which1(boolarray) result(indexarray1)
!returns index of the first element that is TRUE in the input array
!if there are more, stop
implicit none
logical,dimension(:),intent(in):: boolarray
integer,dimension(max(1,count(boolarray))):: indexarray
integer:: indexarray1
integer:: i
	if (count(boolarray) .eq. 0) then
	  indexarray(:)= 0
	else
	  indexarray(:)= pack((/(i,i=1,size(boolarray))/),boolarray)
	end if
	if (size(indexarray)>1) then
		write(*,*)'Ambiguous search result in which1.Quitting.'
!		stop 
	else
		indexarray1=indexarray(1)
	end if
end function which1



END MODULE utils_h