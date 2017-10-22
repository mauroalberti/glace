! fortran 90/95 program
! for icesat gla12 analysis
!
! Mauro Alberti,

! version: August, 12, 2008
        
!------------------------------------------------
!------------------------------------------------
   
module kinds_mod

	integer, parameter :: i1b = selected_int_kind(1)                    
	integer, parameter :: i2b = selected_int_kind(4)                    
	integer, parameter :: i4b = selected_int_kind(9)  
	integer, parameter :: r4b = selected_real_kind( 6, 37)              
	integer, parameter :: r8b = selected_real_kind(15,307) 


	! GLA input invalid_values
	integer (kind=i1b),parameter :: gi_invalid_i1b=127
	integer (kind=i2b),parameter :: gi_invalid_i2b=32767
	integer (kind=i4b),parameter :: gi_invalid_i4b=2147483647

	real (kind=r4b),parameter :: gi_invalid_r4b = huge(1.0_r4b)
	real (kind=r8b),parameter :: gi_invalid_r8b = huge(1.0_r8b)

   
   
end module kinds_mod


!------------------------------------------------
!------------------------------------------------

module analysis_parameters

use kinds_mod

! algorithmic parameters
integer (kind=i2b), parameter :: record_length = 133 !  = record length in input files
integer (kind=i2b), parameter  :: maxNumOffs = 500 ! maximum allowed number of position offsets
										! for each track (used for virtual indices calculation)
integer (kind=i2b), parameter :: maxNumTracks = 500 ! maximum allowed number of tracks in
													! input binary file, due to memory size
                                       
! natural geometric/spatial parameters
real (kind=r8b), parameter :: pi = 3.1415926535897932384626433
real (kind=r8b), parameter :: EarthRadius_m = 6358000.0 !m, near Pole

! analysis parameters
real (kind=r8b), parameter :: mintracklength = 5000 !m, minimum length of a track for calculating its great circle 
							! and the intersection with other tracks
real (kind=r8b), parameter :: angincr_step = .0000269581 ! mean angular increment (in radians) for each ICESat spot
   								! experimentally and theoretically constrained
real (kind=r8b) :: searchradius_m ! search radius between records, in meters
real (kind=r8b)	:: searchradius_rad ! search radius between records, in radians

integer (kind=i2b):: ToleranceFactorsDefault_paralleltracks = 10 &
					,ToleranceFactorsDefault_intersectingtracks = 50 &
                    ,ToleranceFactors(2)

real (kind=r8b)	:: CorrectedDistanceThr_par_rad, CorrectedDistanceThr_int_rad

real (kind=r4b) :: deltaelev_min, deltaelev_max
                     

contains

character (len=20) function parse_analysiscase(analysistype_id) result (analysistype_text)

	integer (kind=i2b), intent(in) :: analysistype_id
	
	select case(analysistype_id)
  	case (0)
		analysistype_text = '      Short-complete'
  	case (1)
		analysistype_text = '   Parallel-complete'
  	case (2)
		analysistype_text = '  Parallel-optimized'
  	case (3)
		analysistype_text = 'Nonparallel-internal'
  	case (4)
		analysistype_text = 'Nonparallel-external'
    
end select


end function parse_analysiscase 



end module analysis_parameters


!------------------------------------------------
!------------------------------------------------

module structures_defs

use analysis_parameters


type :: vector               
	real (kind=r8b)	:: x,y,z
end type vector


type :: pole
 sequence
    real (kind=r8b) :: lat_degr, lon_degr
end type pole


type :: point 
	type(vector) :: vect_coord
	type(pole) :: pole_coord      
end type point 


type :: range 
	integer (kind=i4b)  :: lower_limit, upper_limit     
end type range 


type :: date
sequence
	integer (kind=i2b) :: year,month,day,hour,minutes
    real (kind=r4b) :: seconds 
end type date


type     :: GLA12shot
  sequence

  integer (kind=i4b) :: id_shot				! id of shot
  character (len=45) :: GLA12sourcefile     ! GLA12 file name
  integer (kind=i4b) :: id_track  			! id of track	 	
  integer (kind=i4b) :: rec_ndx				! GLA12 record index
  real    (kind=r8b) :: UTCTime_julsec		! shot time in Julian seconds
  type    (date)     :: time_shot			! shot time in year-month-day-hr-min-sec
  type    (pole)     :: shot_loc			! location (latitude-longitude)
  integer (kind=i1b) :: ElvuseFlg			! elevation use flag (0: valid elevation; 1: invalid e.)
  real    (kind=r4b) :: elev 				! elevation (meters) 
  real    (kind=r4b) :: satElevCorr			! elevation correction for saturation (meters) 
  real    (kind=r4b) :: gdHt				! geoid (meters)
  real    (kind=r4b) :: erElv				! solid earth tide elevation (meters, linearly interpolated between first and last shot)
  integer (kind=i2b) :: gval_rcv			! gain value used for received pulse  
  integer (kind=i2b) :: sigmaatt			! attitude quality indicator  
  real    (kind=r4b) :: RecNrgAll			! received energy signal begin to signal end
  integer (kind=i1b) :: numPk				! number of peaks found in the return
  real    (kind=r4b) :: kurt2				! kurtosis of the received echo (standard)
  real    (kind=r4b) :: skew2				! skewness
  integer (kind=i2b) :: IceSVar				! standard deviation of the ice sheet Gaussian fit
  integer (kind=i1b) :: satNdx				! saturation index
  integer (kind=i1b) :: pctSAT				! percent saturation
    
end type GLA12shot


type :: rec_offset
	integer (kind=i2b)	:: i_p, i_p_offset
end type rec_offset


type :: track
	integer (kind=i2b) :: id, offsets_number, max_ivirt 
	integer (kind=i4b) :: numrec,firstrecnum, lastrecnum, begintime    					  
    type(point) :: firstrecpoint,lastrecpoint, profGCpoint
    real (kind=r8b) :: ang_length
    type (rec_offset) :: offsets(maxNumOffs)  
end type track


type :: track_couple
	integer (kind=i2b) :: tracka_id, trackb_id, caseid    
	integer (kind=i4b) :: totalrecords, foundrecords
    real (kind=r8b) :: trackcouple_exectime
end type track_couple



end module structures_defs

!------------------------------------------------
!------------------------------------------------



!------------------------------------------------
!------------------------------------------------

module geometric_manipulations

use structures_defs

implicit none


contains

!------------------------
! angle (in radians) between two vectors (radiants, 0-pi)

real (kind=r8b) function vector_angle_rad(vector1,vector2)

	implicit none
    
	type(vector), intent(in) :: vector1, vector2    
    real (kind=r8b):: scaledscalarproduct, vector1_magn, vector2_magn 

    
	vector1_magn = vector_magn(vector1)
	vector2_magn = vector_magn(vector2)
	if ((vector1_magn < 1.0e-05).or.(vector2_magn < 1.0e-05)) then
		write(*,*) 'error in vector magnitude (subr. vector_angle_rad)'
		read(*,*)
    	stop	! STOP PROGRAM FOR MAJOR ERROR IN DATA INPUT
	end if
	! scalar product between two vectors
	scaledscalarproduct = vector_scalprod(vector1, vector2)/(vector1_magn*vector2_magn)  
     
	! angle between vectors (in radians)
    
	if (scaledscalarproduct < -1.) then 
		vector_angle_rad = pi
    else if (scaledscalarproduct > 1.) then 
		vector_angle_rad = 0
    else
    	vector_angle_rad = acos(scaledscalarproduct)
    end if

end  function vector_angle_rad 
        
!------------------------


!------------------------
! angle (in radians) between two axes (radiants, 0-pi/2)

real (kind=r8b) function axes_angle_rad(vector1,vector2)

	implicit none
	type(vector), intent(in) :: vector1, vector2    

	! angle between vectors (in radians)
	axes_angle_rad = vector_angle_rad(vector1,vector2)
    axes_angle_rad = min(axes_angle_rad, pi-axes_angle_rad)

end function axes_angle_rad

!------------------------


!------------------------
! angle (in radians) between two points (radiants, 0-pi)  
  	
real (kind=r8b) function latlon_angle_rad(lat_degr1,lon_degr1,lat_degr2,lon_degr2)
    
	real (kind=r8b), intent(in) :: lat_degr1,lon_degr1,lat_degr2,lon_degr2

    
	type(pole):: pole1, pole2
	type(vector):: vector1, vector2
    
	pole1%lat_degr = lat_degr1
	pole1%lon_degr = lon_degr1

	pole2%lat_degr = lat_degr2
	pole2%lon_degr = lon_degr2  

	vector1 = pole2vector(pole1)
    call vector_normalization(vector1)

	vector2 = pole2vector(pole2)
    call vector_normalization(vector2)
    
	latlon_angle_rad = vector_angle_rad(vector1,vector2) 

end function latlon_angle_rad

!------------------------

                            
!------------------------
! calculates the antipole from a given pole

type(pole) function antipole(pole1)

	type(pole), intent(in) :: pole1

	antipole%lat_degr = - pole1%lat_degr
    if (pole1%lon_degr>=0.0) then
    	antipole%lon_degr = pole1%lon_degr-180.0
    else
    	antipole%lon_degr = pole1%lon_degr+180.0
    end if
    
end function antipole

!------------------------


!------------------------
! vector from latitude/longitude 
! (modified and simplified from Etter, 1997, p. 393)

type(vector) function pole2vector(poleA)

	type(pole), intent(in) :: poleA
    
	real (kind=r8b) :: colat_radA,lon_radA   


	colat_radA = (90.0 - poleA%lat_degr)*pi/180.0

	lon_radA = poleA%lon_degr*pi/180.0
    
	pole2vector%x = sin(colat_radA)*cos(lon_radA) 
	pole2vector%y = sin(colat_radA)*sin(lon_radA)
	pole2vector%z = cos(colat_radA)


end function pole2vector

!------------------------


!------------------------
! vector normalization 

subroutine vector_normalization(vectorA)

	type(vector), intent(inout) :: vectorA

	real (kind=r8b):: vectorA_magn

	vectorA_magn = vector_magn(vectorA)

    if (vectorA_magn < 1.0e-07) then
      write(*,*) 'DEBUG Message: error in vector magnitude processing. Hit any key to stop'
      read (*,*)
      stop ! STOP PROGRAM FOR MAJOR ERROR IN DATA INPUT
    end if

	vectorA%x = vectorA%x/vectorA_magn
	vectorA%y = vectorA%y/vectorA_magn
    vectorA%z = vectorA%z/vectorA_magn  
    

end subroutine vector_normalization

!------------------------


!------------------------
! latitude/longitude from unit vector 

type(pole) function vector2pole(vectorA)

	type(vector), intent(in) :: vectorA

	vector2pole%lat_degr = 90.0 - acos(vectorA%z)*180.0/pi   

	vector2pole%lon_degr = atan2(vectorA%y,vectorA%x)*180.0/pi


end function vector2pole

!------------------------


!------------------------
! vector magnitude 

real (kind=r8b) function vector_magn(vectorA)

	type(vector), intent(in) :: vectorA


   	vector_magn = sqrt((vectorA%x)**2+(vectorA%y)**2+(vectorA%z)**2)


end function vector_magn

!------------------------


!------------------------
! scalar product of two vectors (given as their cartesian coordinates)
      
real (kind=r8b) function vector_scalprod(vector1, vector2)


	type(vector), intent(in) :: vector1, vector2

 
	vector_scalprod = vector1%x*vector2%x+vector1%y*vector2%y+vector1%z*vector2%z  


end function vector_scalprod

!------------------------


!------------------------
! vector product of two vectors (given as their cartesian coordinates) 
      
type(vector) function vector_vectprod(vector1,vector2)


	type(vector), intent(in) :: vector1, vector2
   
	vector_vectprod%x=(vector1%y*vector2%z)-(vector1%z*vector2%y)
	vector_vectprod%y=(vector1%z*vector2%x)-(vector1%x*vector2%z)
	vector_vectprod%z=(vector1%x*vector2%y)-(vector1%y*vector2%x)

end function vector_vectprod

!------------------------


end module geometric_manipulations


!------------------------------------------------
!------------------------------------------------



!------------------------------------------------
!------------------------------------------------

module GLArecords_management


use geometric_manipulations

             
contains        


!------------------------
! calculate total number of records in input file    
    
integer (kind=i4b) function FileRecordCount(inputfileunit) result(NumRecordFile)

    integer (kind=i2b), intent(in) :: inputfileunit
    integer (kind=i2b) :: ir_cond
    integer (kind=i4b) :: j
    character (len=record_length) :: recordA    

       
	j=0
	do
		j=j+1
		read(unit=inputfileunit,rec=j,iostat=ir_cond) recordA

    	if (ir_cond/=0) then
			exit
		end if
    
	end do 
        
	NumRecordFile = j-1

end function FileRecordCount

!------------------------
        

!------------------------
! read GLA12 shot data
     
type(GLA12shot) function readrec(inputfileunit,j) result(GLA12shot_data)
      
	integer (kind=i2b), intent(in) :: inputfileunit
    integer (kind=i4b), intent(in) :: j		     

    integer (kind=i2b) :: ir_cond         


	read(inputfileunit,rec=j,iostat=ir_cond) GLA12shot_data
    
	if (ir_cond/=0) then
    	write(*,*) 'record not readable'
    	write(*,*) 'program will stop after you hit any key'
        read(*,*)
        stop ! STOP PROGRAM FOR MAJOR ERROR IN DATA INPUT
    end if

  
end function readrec


!------------------------


!------------------------
! analyze records within given ranges for both tracks  
      
subroutine analyze_recinterval(inputunit_a,inputunit_b,range_pa,range_pb,trackcouple)
					
	implicit none

	integer (kind=i2b), intent(in) :: inputunit_a, inputunit_b
	type(range), intent(in) ::  range_pa,range_pb
	type(track_couple), intent(inout) :: trackcouple
    
	integer (kind=i4b) :: i,j

	type(GLA12shot) :: recdataA, recdataB      
       

	do i = range_pa%lower_limit, range_pa%upper_limit    	
	
		do j = range_pb%lower_limit, range_pb%upper_limit
			
			call analyze_reccouple(inputunit_a, inputunit_b,i,j,trackcouple)

		end do 
                	
	end do

end subroutine analyze_recinterval


!------------------------


!------------------------
! analyse distance in a shot pair
! and, if less than the threshold distance, write data in output file 
subroutine analyze_reccouple(inputunit_a, inputunit_b,i,j, trackcouple)

	integer (kind=i2b), intent(in) :: inputunit_a, inputunit_b
	integer (kind=i4b), intent(in) :: i,j
	type(track_couple), intent(inout) :: trackcouple
        
	type(GLA12shot) :: recdataA, recdataB
	real (kind=r8b) :: angdist_AB
            
	recdataA = readrec(inputunit_a,i)
	recdataB = readrec(inputunit_b,j)   
        
	! calculate spherical distance and print out pair parameters if separation less than threshold                     
	angdist_AB = latlon_angle_rad(recdataA%shot_loc%lat_degr,recdataA%shot_loc%lon_degr  &
    					,recdataB%shot_loc%lat_degr,recdataB%shot_loc%lon_degr)
				
	if (angdist_AB <= searchradius_rad) then
		call printout(recdataA, recdataB, angdist_AB, trackcouple)
	end if

end subroutine analyze_reccouple


!------------------------



!------------------------
! write results for shot pair in output file 
        
subroutine printout(recdat1,recdat2,minangdist_12,trackcouple)
    	
	type(GLA12shot), intent(in) :: recdat1, recdat2
	real (kind=r8b), intent(in)  :: minangdist_12
	type(track_couple), intent(inout) :: trackcouple

	character (len=199) :: AnalysisOutputString
	character, parameter  :: SeparatorString = ","   
    real (kind=r8b) :: deltaelev_AB

    
	! skip if delta_elev out of allowed range 
    deltaelev_AB = abs(recdat2%elev - recdat1%elev)
	if ((deltaelev_AB>deltaelev_max).or.   &
        (deltaelev_AB<deltaelev_min)) then
			 return
	end if

	trackcouple%foundrecords = trackcouple%foundrecords + 1

    
	! OUTPUT for ANALYSIS FILE
    ! shot 1 id
    write(AnalysisOutputString,"(I10,A)") recdat1%id_shot,SeparatorString
    ! track 1 id
    write(AnalysisOutputString,"(A,I6,A)") trim(AnalysisOutputString),recdat1%id_track,SeparatorString    
    ! shot 2 id
    write(AnalysisOutputString,"(A,I10,A)") trim(AnalysisOutputString),recdat2%id_shot,SeparatorString
    ! track 2 id
    write(AnalysisOutputString,"(A,I6,A)") trim(AnalysisOutputString),recdat2%id_track,SeparatorString    
	! time difference (in days) between the two shots
    write(AnalysisOutputString,"(A,F7.2,A)") trim(AnalysisOutputString),   &
    		(recdat2%UTCTime_julsec-recdat1%UTCTime_julsec)/86400.0,SeparatorString
    ! year shot 1
    write(AnalysisOutputString,"(A,I4,A)") trim(AnalysisOutputString),recdat1%time_shot%year,SeparatorString 
    ! month shot 1
    write(AnalysisOutputString,"(A,I2,A)") trim(AnalysisOutputString),recdat1%time_shot%month,SeparatorString 
    ! day shot 1
    write(AnalysisOutputString,"(A,I2,A)") trim(AnalysisOutputString),recdat1%time_shot%day,SeparatorString            
    ! hour shot 1
    write(AnalysisOutputString,"(A,I2,A)") trim(AnalysisOutputString),recdat1%time_shot%hour,SeparatorString
    ! minutes shot 1
    write(AnalysisOutputString,"(A,I2,A)") trim(AnalysisOutputString),recdat1%time_shot%minutes,SeparatorString
    ! seconds shot 1
    write(AnalysisOutputString,"(A,F5.2,A)") trim(AnalysisOutputString),recdat1%time_shot%seconds,SeparatorString
    ! year shot 2
    write(AnalysisOutputString,"(A,I4,A)") trim(AnalysisOutputString),recdat2%time_shot%year,SeparatorString 
    ! month shot 2
    write(AnalysisOutputString,"(A,I2,A)") trim(AnalysisOutputString),recdat2%time_shot%month,SeparatorString 
    ! day shot 2
    write(AnalysisOutputString,"(A,I2,A)") trim(AnalysisOutputString),recdat2%time_shot%day,SeparatorString            
    ! hour shot 2
    write(AnalysisOutputString,"(A,I2,A)") trim(AnalysisOutputString),recdat2%time_shot%hour,SeparatorString
    ! minutes shot 2
    write(AnalysisOutputString,"(A,I2,A)") trim(AnalysisOutputString),recdat2%time_shot%minutes,SeparatorString
    ! seconds shot 2
    write(AnalysisOutputString,"(A,F5.2,A)") trim(AnalysisOutputString),recdat2%time_shot%seconds,SeparatorString
    ! latitude shot 1
    write(AnalysisOutputString,"(A,F10.6,A)") trim(AnalysisOutputString),recdat1%shot_loc%lat_degr,SeparatorString
    ! longitude shot 1
    write(AnalysisOutputString,"(A,F11.6,A)") trim(AnalysisOutputString),recdat1%shot_loc%lon_degr,SeparatorString
    ! latitude shot 2
    write(AnalysisOutputString,"(A,F10.6,A)") trim(AnalysisOutputString),recdat2%shot_loc%lat_degr,SeparatorString
    ! longitude shot 2
    write(AnalysisOutputString,"(A,F11.6,A)") trim(AnalysisOutputString),recdat2%shot_loc%lon_degr,SeparatorString
	! distance (m) between shot 1 and shot 2
    write(AnalysisOutputString,"(A,F8.2,A)") trim(AnalysisOutputString),minangdist_12*EarthRadius_m,SeparatorString
	! elevation (uncorrected) shot 1
    write(AnalysisOutputString,"(A,F8.2,A)") trim(AnalysisOutputString),recdat1%elev,SeparatorString
	! elevation (uncorrected) shot 2
    write(AnalysisOutputString,"(A,F8.2,A)") trim(AnalysisOutputString),recdat2%elev,SeparatorString
    ! delta elevation (uncorrected) shot2 - shot1
    write(AnalysisOutputString,"(A,F8.2,A)") trim(AnalysisOutputString),recdat2%elev-recdat1%elev,SeparatorString
	! elevation (corrected) shot 1
    if (recdat1%satElevCorr < gi_invalid_r4b) then
    	write(AnalysisOutputString,"(A,F8.2,A)") trim(AnalysisOutputString)  &
        		,recdat1%elev + recdat1%satElevCorr,SeparatorString
    else
    	write(AnalysisOutputString,"(A,A)") trim(AnalysisOutputString),SeparatorString
    end if
	! elevation (corrected) shot 2
    if (recdat2%satElevCorr < gi_invalid_r4b) then
    	write(AnalysisOutputString,"(A,F8.2,A)") trim(AnalysisOutputString)  &
        		,recdat2%elev + recdat2%satElevCorr,SeparatorString
    else
    	write(AnalysisOutputString,"(A,A)") trim(AnalysisOutputString),SeparatorString
    end if
    ! delta elevation (corrected) shot2 - shot1
    if ((recdat1%satElevCorr < gi_invalid_r4b).and.  &
        (recdat2%satElevCorr < gi_invalid_r4b)) then
    	write(AnalysisOutputString,"(A,F8.2)") trim(AnalysisOutputString)  &
        	,recdat2%elev + recdat2%satElevCorr - recdat1%elev - recdat1%satElevCorr
    end if

	write(18,"(A)") AnalysisOutputString
            

end subroutine printout
        
!------------------------


end module GLArecords_management


!------------------------------------------------
!------------------------------------------------

    


!------------------------------------------------
!------------------------------------------------


module trackcouple_basicmanag


use GLArecords_management

    
implicit none


integer (kind=i4b) :: p


type(GLA12shot) :: recdataA
      
contains

!------------------------

! defines first and last records for each track
subroutine prof_initfinrecs(inputfileunit,NumRecordFile, maxNumTracks, track_i, TotalTrackNumber)

    integer (kind=i2b), intent(in) :: inputfileunit, maxNumTracks    
    integer (kind=i4b), intent(in) :: NumRecordFile
	type(track), intent(out) :: track_i(maxNumTracks)
    integer (kind=i4b), intent(out) :: TotalTrackNumber

    integer (kind=i4b) :: n, j, i_upperbound, p, curr_trackid   ! temporary variables
    logical :: done


	n=1
	p=1

	tracks_definition: do  
    
		recdataA = readrec(inputfileunit,n)

		curr_trackid = recdataA%id_track
  	
		track_i(p)%id = curr_trackid	! set track code
		track_i(p)%firstrecnum = n		! set track first record number
    	if (n==NumRecordFile) then		! if last record in file
			track_i(p)%lastrecnum = n	! set track last record number
			exit tracks_definition		! exit loop, job done 
		end if

		done = .false.    
    	j = n
            
		do while (.not.done)
    		j = j+100
        	if (j>=NumRecordFile) then
        		i_upperbound = NumRecordFile
          		done = .true.
        	else
				recdataA = readrec(inputfileunit,j)          
				if (recdataA%id_track /= curr_trackid) then
        			i_upperbound = j
          			done = .true.
            	end if    
			end if
		end do    

		maxlim_search: do j = i_upperbound,n, -1
			recdataA = readrec(inputfileunit,j) 
			if (recdataA%id_track == curr_trackid) then
				track_i(p)%lastrecnum = j
				exit maxlim_search
			end if   

		end do maxlim_search

		if (j < 1) then
		write(*,*) 'error in code: ref. 01'
        stop	! STOP PROGRAM FOR MAJOR ERROR IN DATA INPUT
        end if

        
		n = j+1
    	if (n>NumRecordFile) exit tracks_definition

		p = p+1  ! progressive id of track

	end do tracks_definition

	TotalTrackNumber = p



end subroutine prof_initfinrecs
 
! ---------------------
   

! ---------------------
! determine start time of track (in sec)

subroutine track_begintime(inputfileunit, track_p)

	integer (kind=i2b), intent(in) :: inputfileunit            
	type(track), intent(inout) :: track_p


    integer (kind=i4b) :: j   
    type(GLA12shot) :: recdataA    

	j = track_p%firstrecnum
	recdataA = readrec(inputfileunit,j)
        
	track_p%begintime = recdataA%UTCTime_julsec


end subroutine track_begintime

! ---------------------


! ---------------------
! define coordinates of boundary records for track

subroutine track_boundcoord(inputfileunit, track_p)

    integer (kind=i2b), intent(in) :: inputfileunit
	type(track), intent(inout) :: track_p

    integer (kind=i4b) :: j   
    

	j = track_p%firstrecnum
	recdataA = readrec(inputfileunit,j)

	track_p%firstrecpoint%pole_coord%lat_degr = recdataA%shot_loc%lat_degr   
	track_p%firstrecpoint%pole_coord%lon_degr = recdataA%shot_loc%lon_degr  


	j = track_p%lastrecnum         
	recdataA = readrec(inputfileunit,j)

	track_p%lastrecpoint%pole_coord%lat_degr = recdataA%shot_loc%lat_degr   
	track_p%lastrecpoint%pole_coord%lon_degr = recdataA%shot_loc%lon_degr     

	track_p%firstrecpoint%vect_coord = pole2vector(track_p%firstrecpoint%pole_coord)
	track_p%lastrecpoint%vect_coord = pole2vector(track_p%lastrecpoint%pole_coord)      
  

end subroutine track_boundcoord

!------------------------


!------------------------
! calculate track angular length (in radians) 

subroutine track_length_rad(track_p)

	type(track), intent(inout) :: track_p


	if (track_p%numrec == 1) then
		track_p%ang_length = 0.0
    else      
    	track_p%ang_length = axes_angle_rad(track_p%firstrecpoint%vect_coord   &
        	,track_p%lastrecpoint%vect_coord)   
    end if
    

end subroutine track_length_rad

!------------------------


!------------------------
! calculate great-circle pole approximating a track

subroutine track_greatcirclepole(track_p)

	type(track), intent(inout) :: track_p
   
  	type(vector) :: profGCvector
    
	if (track_p%ang_length*EarthRadius_m > mintracklength) then

		profGCvector = vector_vectprod(track_p%firstrecpoint%vect_coord  &
        	,track_p%lastrecpoint%vect_coord)

    	call vector_normalization(profGCvector)
		track_p%profGCpoint%vect_coord = profGCvector
        track_p%profGCpoint%pole_coord = vector2pole(track_p%profGCpoint%vect_coord)
    
    end if
    

end subroutine track_greatcirclepole

!------------------------


!------------------------
! increments of virtual indices in a track
            
subroutine track_increments(inputfileunit, track_p)

	integer (kind=i2b), intent(in) :: inputfileunit
	type(track), intent(inout) :: track_p

	type(GLA12shot) :: recdata_i
   	integer (kind=i4b) :: j, delta_index, cum_deltaind, changed_indexnumber 
	real (kind=r8b) :: lat_prev, lon_prev, angdist_12
 

    cum_deltaind = 0
    changed_indexnumber = 0
        
    recdata_i = readrec(inputfileunit,track_p%firstrecnum)
        
    lat_prev = recdata_i%shot_loc%lat_degr
    lon_prev = recdata_i%shot_loc%lon_degr
                
	do j = track_p%firstrecnum+1,track_p%lastrecnum
			
		recdata_i = readrec(inputfileunit,j)   	   

		angdist_12 = latlon_angle_rad(lat_prev, lon_prev  &
          ,recdata_i%shot_loc%lat_degr, recdata_i%shot_loc%lon_degr) 

		delta_index = idnint(angdist_12/angincr_step)

		if (delta_index == 0) then
			write(*,*) 'found two records with apparently the same position'
			write(*,*) 'program will stop (after you hit any key) to prevent errors.  '
            read(*,*)
            stop	! STOP PROGRAM FOR MAJOR ERROR IN DATA INPUT
       	end if        
            
		if (delta_index > 1) then
			cum_deltaind = cum_deltaind + delta_index - 1
            changed_indexnumber = changed_indexnumber+1
            if (changed_indexnumber>maxNumOffs) then
				write(*,*) 'reached maximum array size (maxNumOffs) for track%offsets'
				write(*,*) 'program will stop (after you hit any key) to prevent errors.'
				write(*,*) 'To solve the problem, reduce analyzed data number or'                    
 				write(*,*) 'change array size in source code and recompile'
                read(*,*)
                stop	! STOP PROGRAM FOR MAJOR ERROR IN DATA INPUT                              
            end if
            track_p%offsets(changed_indexnumber)%i_p = j-track_p%firstrecnum+1
            track_p%offsets(changed_indexnumber)%i_p_offset = cum_deltaind                
 		end if
              
		lat_prev = recdata_i%shot_loc%lat_degr
		lon_prev = recdata_i%shot_loc%lon_degr

	end do 

	track_p%offsets_number = changed_indexnumber
	track_p%max_ivirt = j-track_p%firstrecnum+1+ cum_deltaind


end subroutine track_increments

!------------------------



!------------------------
! determine virtual index of a track from the real one
        
integer (kind=i2b) function ip_virt(ip_num, offset_arr,offset_number)

	implicit none
	integer (kind=i2b), intent(in) :: offset_number
	integer (kind=i2b), intent(in) :: ip_num 
	type (rec_offset), intent(in) :: offset_arr(offset_number)
        
	integer (kind=i2b) :: k
       
	if (offset_number == 0) then
    	ip_virt = ip_num
		return
    end if    

   
  	! cases for ip_num greater than the first offset
	do k = offset_number, 1, -1 

		if (offset_arr(k)%i_p <= ip_num) then

			ip_virt = ip_num + offset_arr(k)%i_p_offset
        	return
            
        end if

	end do

	! case for ip_num lower than the first offset
    ip_virt = ip_num
    

end function ip_virt

!------------------------



!------------------------
! determine real index (when existing) of a track from the virtual one
        
integer (kind=i2b) function ip(ip_virt, max_ip, offset_arr,offset_number)

	implicit none
	integer (kind=i2b), intent(in) :: offset_number
	integer (kind=i2b), intent(in) :: ip_virt

	type (rec_offset), intent(in) :: offset_arr(offset_number)
        
	integer (kind=i2b) :: k, ll_cr, ul_cr,ip_virt_upranmax, ip_virt_upranmin
	integer (kind=i4b)  :: max_ip        
	integer (kind=i1b) :: analysis_case

	!----------------------
	! selection of analysis case
    if (ip_virt < 0) then
		analysis_case = 1	! case of negative (invalid) ip_virt number
    else if (offset_number == 0) then
		analysis_case = 2	! case for tracks without missing measures
    else if (ip_virt <= (offset_arr(1)%i_p-1)) then
		analysis_case = 3	! case for ip_virt lower than the first offset ip
    else if (ip_virt < offset_arr(1)%i_p+offset_arr(1)%i_p_offset) then
		analysis_case = 4	! case for ip_virt lower than the first offset ip_virt
    else 
		analysis_case = 5	! general case for tracks with 1 or more missing measures      
    end if

    
	!----------------------
    ! analysis cases
    ip=-444
    select case(analysis_case)
    case(1)	! case of negative (invalid) ip_virt number		
  		ip = -666
    case(2)	! case for tracks without missing measures		  
		ip = ip_virt        
        if (ip > max_ip ) then
  			ip = -999
		end if
    case(3)	! case for ip_virt lower than the first offset ip		
		ip = ip_virt  
    case(4)	! case for ip_virt lower than the first offset ip_virt		
		ip = -1               
    case(5)	! general case for tracks with 1 or more missing measures       
		! uppermost interval for ip_virt 
		!   upper boundary
		ip_virt_upranmax = max_ip + offset_arr(offset_number)%i_p_offset
		!   lower boundary 
		ip_virt_upranmin = offset_arr(offset_number)%i_p + offset_arr(offset_number)%i_p_offset
 
		if (ip_virt > ip_virt_upranmax) then
			! case for ip* value exceeding corresponding maximum value of ip
  			ip = -999
		else if (ip_virt >= ip_virt_upranmin) then
			! case for ip* value greater than the corresponding uppermost value of ip offset variation
            ip = ip_virt-offset_arr(offset_number)%i_p_offset
		else
  			!cases for ip* between corresponding uppermost and lowermost values of ip offset variation
			do k = offset_number, 2, -1 
  				! considered range
                 ! upper limit
  				ul_cr = offset_arr(k)%i_p + offset_arr(k)%i_p_offset
  				 ! lower limit
  				ll_cr = offset_arr(k-1)%i_p + offset_arr(k-1)%i_p_offset
  
  				if (ip_virt<ul_cr .and. ip_virt>=ll_cr) then
    				if (ip_virt >= (offset_arr(k)%i_p + offset_arr(k-1)%i_p_offset)) then
					! non-existing ip values
      					ip =  -1
      				else 
					! existing ip values                       
						ip = ip_virt - offset_arr(k-1)%i_p_offset
    				end if            
  				end if
			end do
        end if
	end select    



	if (ip == -444 .or. ip > max_ip) then
        write(*,*) 'error in function ip'
        write(*,*) 'program will stop, hit any key'
        read(*,*) 
        stop	! STOP PROGRAM FOR MAJOR ERROR IN DATA INPUT
    end if       

    
end function ip

!------------------------



!------------------------
! determine angular relationships between an intersection pole 
! and the track that could contain it
                
subroutine pole_in_track(pointA, trackA, ispolewithintrack, min_angdist, isnearest_firstrec)

	type(point), intent(in) :: pointA
	type(track), intent(in) :: trackA
	logical, intent(out) ::  ispolewithintrack, isnearest_firstrec
    real (kind= r8b), intent(out) :: min_angdist
    
    real (kind= r8b) :: vectangle_rad_firstpoint, vectangle_rad_lastpoint

	vectangle_rad_firstpoint = vector_angle_rad(trackA%firstrecpoint%vect_coord,pointA%vect_coord)
	vectangle_rad_lastpoint = vector_angle_rad(trackA%lastrecpoint%vect_coord,pointA%vect_coord)

	if (vectangle_rad_firstpoint<vectangle_rad_lastpoint) then
		isnearest_firstrec = .true.
	else
		isnearest_firstrec = .false.
    end if

    
    min_angdist = min(vectangle_rad_firstpoint, vectangle_rad_lastpoint)

    ispolewithintrack = .true.
	if ((vectangle_rad_firstpoint > trackA%ang_length).or.   &
        (vectangle_rad_lastpoint > trackA%ang_length)) then
		ispolewithintrack = .false.
    end if    


 end subroutine pole_in_track

!------------------------



!------------------------
! calculate projection of first record of track -a- on track -b-
! 	using great circle perpendicular to track -a-

type(point) function pointprojection_onGC(pa_GCpoint, pb_GCpoint, pa_r1point)

	type(point), intent(in) :: pa_GCpoint, pb_GCpoint, pa_r1point

	type(point) :: paP1_orthGCpoint
    real (kind=r8b) :: dist_P1Pp
    
	! determine great circle pole that is perpendicular to track -a-
    ! and that includes first record of track -a-
	paP1_orthGCpoint%vect_coord = vector_vectprod(pa_GCpoint%vect_coord,pa_r1point%vect_coord)
    call vector_normalization(paP1_orthGCpoint%vect_coord)

	! determine intersection point between previously defined great circle and track -b-
	pointprojection_onGC%vect_coord = vector_vectprod(paP1_orthGCpoint%vect_coord,pb_GCpoint%vect_coord)
    call vector_normalization(pointprojection_onGC%vect_coord)

	! check if angle between calculated intersection point and first record of track -a- is more than 90°      
	! and in positive case recalculate intersection point
    dist_P1Pp = vector_angle_rad(pa_r1point%vect_coord,pointprojection_onGC%vect_coord)
    if (dist_P1Pp > pi/2) then
      pointprojection_onGC%vect_coord%x=-pointprojection_onGC%vect_coord%x
      pointprojection_onGC%vect_coord%y=-pointprojection_onGC%vect_coord%y      
      pointprojection_onGC%vect_coord%z=-pointprojection_onGC%vect_coord%z
    end if

	! calculate polar coordinates of intersection point 
    pointprojection_onGC%pole_coord = vector2pole(pointprojection_onGC%vect_coord)


end function pointprojection_onGC

!------------------------



!------------------------
! calculates the distance between two (subparallel) tracks
! along the great circle that includes a given point of one track

real (kind=r8b) function angdistance_between_tracks(pa_GCpoint, pb_GCpoint, pa_r1point)

	type(point), intent(in) :: pa_GCpoint, pb_GCpoint, pa_r1point	  

	type(point) :: paP1_orthGCpoint, pbP0_mindist_paP1

	! determination of point on track -b-
    ! at the minimum distance from point P1 of track -a-
    ! (implicit assumption: the two tracks are subparallel) 
    pbP0_mindist_paP1 = pointprojection_onGC(pa_GCpoint, pb_GCpoint, pa_r1point)
  

	! calculation of distance between the two tracks
    !  at the first record of track -a-
    angdistance_between_tracks = vector_angle_rad(pa_r1point%vect_coord  &
    						,pbP0_mindist_paP1%vect_coord)

    angdistance_between_tracks = min(angdistance_between_tracks,pi-angdistance_between_tracks)
    

end  function angdistance_between_tracks


!------------------------
! definition and initialization of parameters for analysed track pair

subroutine trackcouplepar_initial(trackcouple, pa_id, pb_id, numrec_pa, numrec_pb)

	type(track_couple), intent(inout) :: trackcouple
	integer (kind=i2b) :: pa_id, pb_id 
	integer (kind=i4b) :: numrec_pa, numrec_pb    

	trackcouple%tracka_id = pa_id
	trackcouple%trackb_id = pb_id
	trackcouple%caseid = 0
    trackcouple%totalrecords = numrec_pa*numrec_pb
	trackcouple%foundrecords = 0
	trackcouple%trackcouple_exectime = -1.0

end subroutine trackcouplepar_initial



end module trackcouple_basicmanag

!------------------------------------------------
!------------------------------------------------





!------------------------------------------------
!------------------------------------------------

! processing of comparison between two tracks

module trackcouple_analysis

use trackcouple_basicmanag

implicit none

integer (kind=i4b) :: j


contains



!------------------------
! case of whole record comparison between two tracks

subroutine integraltracks(inputunit_a,inputunit_b,track_a,track_b,trackcouple)

	integer (kind=i2b), intent(in) :: inputunit_a, inputunit_b
    type(track), intent(in) :: track_a, track_b
	type(track_couple), intent(inout) :: trackcouple
    
	type(range) :: range_pa, range_pb    


	!------------------------

	range_pa%lower_limit = track_a%firstrecnum
	range_pa%upper_limit = track_a%lastrecnum

 	range_pb%lower_limit = track_b%firstrecnum
	range_pb%upper_limit = track_b%lastrecnum

	call analyze_recinterval(inputunit_a, inputunit_b,range_pa,range_pb,trackcouple)

end subroutine integraltracks

!------------------------


!------------------------
! case of subparallel tracks

subroutine paralleltracks(inputunit_a, inputunit_b,track_a,track_b,trackcouple)

	integer (kind=i2b), intent(in) :: inputunit_a, inputunit_b
	type(track), intent(in) :: track_a, track_b
	type(track_couple), intent(inout) :: trackcouple
    
    type(GLA12shot) :: rec2_A, recdataA, recdataB0, recdataB 

	integer (kind=i2b) :: i, j, n, step_sign, shift_case, i_virt_a, i_virt_b   &
    				, shift_ivirt, delta_bestpairing, i_virt_b_bestpaired  &
                    , j_virt, j_paired, j_virt_bestpaired, step

    real (kind=r4b) ::  initial_angular_offset
    real(kind=r8b) :: sphertrian_a_side, sphertrian_b_side, sphertrian_c_side   &
				   , sphertrian_A, sphertrian_A_cos

	real (kind=r8b) :: dist_pab_firstrecpa, dist_pab_lastrecpa,   &
    					angdist_AB0, angdist_AB, previous_distance 

	type(point) :: projection_paP1_onpb
	logical :: found_i_b_bestpaired


	! Determine distance between tracks -a- and -b-
    !  and skip analysis if distance > maximum threshold

	! determine distance between tracks -a- and -b- in correspondence to the first record of track -a- 
    dist_pab_firstrecpa = angdistance_between_tracks(track_a%profGCpoint, track_b%profGCpoint   &
    							,track_a%firstrecpoint)

	! determine distance between tracks -a- and -b- in correspondence to the last record of track -a- 
    dist_pab_lastrecpa = angdistance_between_tracks(track_a%profGCpoint, track_b%profGCpoint   &
    							,track_a%lastrecpoint)
                                                    
    ! skip analysis if distance > maximum threshold in correspondence to both first and last record of track -a-
	if ((dist_pab_firstrecpa > CorrectedDistanceThr_par_rad).and.  &
    	(dist_pab_lastrecpa > CorrectedDistanceThr_par_rad)) then
    	return
    end if
    

	!
	! if we are here, subparallel tracks are at a distance lower than maximum threshold 
    
 
	!------------------------
    ! Calculation of initial virtual offset 
    ! between track -a- and -b- 

    
	! calculate angle A between track -a- and great circle joining first records of tracks -a- and -b-
    
	 ! determination of second shot in track -a-
	 rec2_A = readrec(inputunit_a,track_a%firstrecnum+1)
                
	 ! side a
     sphertrian_a_side = latlon_angle_rad(track_b%firstrecpoint%pole_coord%lat_degr  &
       ,track_b%firstrecpoint%pole_coord%lon_degr,rec2_A%shot_loc%lat_degr,rec2_A%shot_loc%lon_degr) 			

	 ! side b
     sphertrian_b_side = latlon_angle_rad(track_a%firstrecpoint%pole_coord%lat_degr  &
       ,track_a%firstrecpoint%pole_coord%lon_degr,rec2_A%shot_loc%lat_degr,rec2_A%shot_loc%lon_degr)

	 ! side c                                
	 sphertrian_c_side = latlon_angle_rad(track_a%firstrecpoint%pole_coord%lat_degr  &
       ,track_a%firstrecpoint%pole_coord%lon_degr, track_b%firstrecpoint%pole_coord%lat_degr  &
       ,track_b%firstrecpoint%pole_coord%lon_degr)

     ! determination of angle A
     ! general case formula: A = arccos{(cosa-cosb*cosc)/(sinb*sinc)}
     ! with some NULL and OUT-OF-RANGE cases
     if (sphertrian_c_side<1.0e-06) then
       shift_case = 1 ! distance between two first records of the two tracks less than about 10 m
     else
       shift_case = 2 ! distance between two first records of the two tracks more than about 10 m
       sphertrian_A_cos = (cos(sphertrian_a_side)-cos(sphertrian_b_side)  &
         *cos(sphertrian_c_side))/(sin(sphertrian_b_side)*sin(sphertrian_c_side))
       if (sphertrian_A_cos>1.0) then
         sphertrian_A_cos = 1.0
       elseif (sphertrian_A_cos<-1.0) then
         sphertrian_A_cos = -1.0
       end if
	   sphertrian_A = acos(sphertrian_A_cos)
       shift_case = 2
     end if


    ! completion of initial virtual offset calculation
	select case (shift_case)
    	case(1)
        	shift_ivirt = 0
        case(2)
			projection_paP1_onpb = pointprojection_onGC(track_a%profGCpoint, track_b%profGCpoint   &
    							,track_a%firstrecpoint)

    		initial_angular_offset = vector_angle_rad(projection_paP1_onpb%vect_coord  &
    								,track_b%firstrecpoint%vect_coord)     

    		! initial_angular_offset is divided by the mean angular increment (in radians) for each ICESat spot
            ! and its sign is determined                  
			shift_ivirt =  nint(initial_angular_offset/angincr_step)

			! if spherical angle A < 90° , shift_ivirt is negative 				
			if (sphertrian_A < pi/2) then
				shift_ivirt = - shift_ivirt
    		end if
	end select

         
	!------------------------                    
    ! cycle of comparison between each shot of track -a-
    !	and the nearest in track -b-

    ! initialise variable storing difference between the virtual index of shot in track -a- 
    ! and the corrected virtual index of matched near shot in track -b-
    delta_bestpairing = 0
    

	! for each shot in track -a-
    
	do i = 1, track_a%numrec
    	

		! read parameters of shot i in track -a-
		recdataA = readrec(inputunit_a,i+track_a%firstrecnum-1)   
                
		! convert i_a in i*_a
        i_virt_a = ip_virt(i, track_a%offsets, track_a%offsets_number)

        ! add to i*_A the shift value and the bestpairing correction,
        ! in order to determine the matched i*_B value
		i_virt_b =  i_virt_a + shift_ivirt
        i_virt_b_bestpaired =  i_virt_b + delta_bestpairing

		!------------------------
        ! search for the real index i_b that is nearest to the virtual index i*_b_bestpaired
		j_virt = 0
		if (i_virt_b_bestpaired<=0) then
        	j_paired = 1
        	found_i_b_bestpaired = .true.            
        elseif (i_virt_b_bestpaired>=track_b%max_ivirt) then
        	j_paired = track_b%numrec
        	found_i_b_bestpaired = .true.
        else
        	n=-1
        	step=-1
        	found_i_b_bestpaired = .false.
        	do           	            
          		n=n+1
            	step_sign = (-1)**n
            	if (step_sign>=0) step = step+1
            	j_virt = i_virt_b_bestpaired + step_sign*step            
				j_paired = ip(j_virt, track_b%numrec,track_b%offsets, track_b%offsets_number)
        		if (j_paired > 0) then
        			found_i_b_bestpaired = .true.
                	exit
            	end if                
			end do

        end if
    
 		
		if (.not.found_i_b_bestpaired) cycle  

        
		!------------------------      
        ! reads the paired record of track b       
		recdataB0 = readrec(inputunit_b,j_paired+track_b%firstrecnum-1)
   
		! calculates the spherical distance and prints out couple parameters
        ! when the inter-distance is lower than or equal to the distance threshold      
        angdist_AB0 = latlon_angle_rad(recdataA%shot_loc%lat_degr,recdataA%shot_loc%lon_degr  &
        	,recdataB0%shot_loc%lat_degr,recdataB0%shot_loc%lon_degr)

        if (angdist_AB0 <= searchradius_rad) then
        	call printout(recdataA, recdataB0, angdist_AB0, trackcouple)
        end if

		!------------------------
		! upper domain search for record coupling
        if (j_paired<track_b%numrec) then
        	previous_distance = angdist_AB0
			do j=j_paired+1,track_b%numrec
				recdataB = readrec(inputunit_b,j+track_b%firstrecnum-1)
        		angdist_AB = latlon_angle_rad(recdataA%shot_loc%lat_degr,recdataA%shot_loc%lon_degr  &
                			,recdataB%shot_loc%lat_degr,recdataB%shot_loc%lon_degr)
        		if (angdist_AB <= searchradius_rad) then
            		! determines the best correction for the virtual index
            		j_virt_bestpaired = ip_virt(j, track_b%offsets, track_b%offsets_number)
					delta_bestpairing = j_virt_bestpaired-i_virt_b
                	! print out of the results
        			call printout(recdataA, recdataB, angdist_AB, trackcouple)
            	elseif (angdist_AB > previous_distance) then            	 
 					exit
        		end if
            	previous_distance = angdist_AB
        	end do
         end if
          
        
		!------------------------
		! lower domain search for record coupling
        if (j_paired>1) then
        	previous_distance = angdist_AB0
			do j=j_paired-1,1,-1
				recdataB = readrec(inputunit_b,j+track_b%firstrecnum-1)
        		angdist_AB = latlon_angle_rad(recdataA%shot_loc%lat_degr,recdataA%shot_loc%lon_degr  &
                			,recdataB%shot_loc%lat_degr,recdataB%shot_loc%lon_degr)
        		if (angdist_AB <= searchradius_rad) then
            		! determine the best correction for the virtual index
            		j_virt_bestpaired = ip_virt(j, track_b%offsets, track_b%offsets_number)
					delta_bestpairing = j_virt_bestpaired-i_virt_b
                	! print out of the results            
        			call printout(recdataA, recdataB, angdist_AB, trackcouple)
            	elseif (angdist_AB > previous_distance) then            	 
 					exit
        		end if
            	previous_distance = angdist_AB
        	end do
		end if
         
	end do !i = track_a%firstrecnum, track_a%lastrecnum


end subroutine paralleltracks

!------------------------


!------------------------
! case of non-parallel tracks  

subroutine nonparalleltracks(inputunit_a, inputunit_b,track_a,track_b  &
				, searchradius_rad, min_angle_btwn_tracks, trackcouple)

	integer (kind=i2b), intent(in) :: inputunit_a, inputunit_b
    type(track), intent(in) :: track_a, track_b   
    real (kind=r8b), intent(in) ::  searchradius_rad, min_angle_btwn_tracks
	type(track_couple), intent(inout) :: trackcouple

	type(point) :: intersectpointA, intersectpointB
	logical :: iswithinAa, iswithinAb, iswithinBa, iswithinBb
	real (kind=r8b) :: min_angdistAa, min_angdistAb, min_angdistBa, min_angdistBb
	logical :: isnearest_firstrecAa, isnearest_firstrecAb, isnearest_firstrecBa, isnearest_firstrecBb
	logical :: internal_pole
   
	real (kind=r8b) :: minangdist_Rp(4)    
	logical :: isnearest_firstrec_Rp(4)
    
	type(point) :: inregion_pole,internalintersectpoint, nearestexternalpoint


    ! input data: 
    ! - A (minimum angle between the 2 great circles): min_angle_btwn_tracks
    ! - a (search radius converted in radians): searchradius_rad
    ! to be determined: b (maximum distance from intersection pole,
    !						above which the distance between two shots will be larger than the search radius)
    ! 					b = asin(sin(a/2)/sin(A/2))
    !			b is then multiplied by a tolerance factor, 
    !			to obtain the -corrected distance range- (in radians)
	CorrectedDistanceThr_int_rad = asin(sin(searchradius_rad/2)/   &
                    		sin(min_angle_btwn_tracks/2)) * ToleranceFactors(2)

    
	! determination of the two intersection axes between the two tracks                             
	intersectpointA%vect_coord = vector_vectprod(track_a%profGCpoint%vect_coord,track_b%profGCpoint%vect_coord)
                    
	call vector_normalization(intersectpointA%vect_coord)
	intersectpointA%pole_coord = vector2pole(intersectpointA%vect_coord)
    intersectpointB%pole_coord = antipole(intersectpointA%pole_coord)
	intersectpointB%vect_coord = pole2vector(intersectpointB%pole_coord)
                                    
 
	! does one of the two intersection axes fall inside of the track couple?              
	call pole_in_track(intersectpointA, track_a, iswithinAa  &
           , min_angdistAa, isnearest_firstrecAa)
	call pole_in_track(intersectpointA, track_b, iswithinAb  &
           , min_angdistAb, isnearest_firstrecAb)
	call pole_in_track(intersectpointB, track_a, iswithinBa  &
           , min_angdistBa, isnearest_firstrecBa)
	call pole_in_track(intersectpointB, track_b, iswithinBb  &
           , min_angdistBb, isnearest_firstrecBb)

	internal_pole = .false.
	if (iswithinAa.or.iswithinAb) then
       internal_pole = .true.
       internalintersectpoint = intersectpointA
    else if (iswithinBa.or.iswithinBb) then
       internal_pole = .true.
       internalintersectpoint = intersectpointB
	end if
	

	minangdist_Rp(1) = min_angdistAa
	minangdist_Rp(2) = min_angdistAb
    minangdist_Rp(3) = min_angdistBa
    minangdist_Rp(4) = min_angdistBb

	isnearest_firstrec_Rp(1) = isnearest_firstrecAa
	isnearest_firstrec_Rp(2) = isnearest_firstrecAb
	isnearest_firstrec_Rp(3) = isnearest_firstrecBa
	isnearest_firstrec_Rp(4) = isnearest_firstrecBb 


    ! runs one of two different functions,
	! depending whether inside or outside intersection axis

    if (internal_pole) then 
       call internalintersection(inputunit_a, inputunit_b,track_a,track_b   &
            , internalintersectpoint, CorrectedDistanceThr_int_rad, trackcouple)
	else
       call externalintersection(inputunit_a, inputunit_b,track_a,track_b, CorrectedDistanceThr_int_rad  &
                    		, minangdist_Rp, isnearest_firstrec_Rp, trackcouple)
    end if


end subroutine nonparalleltracks

!------------------------



!------------------------
! determine index range of track points internal to the -corrected distance range- 

type(range) function range_inters(inputunit, track_i, internalintersectpoint, CorrectedDistanceThr_int_rad)

	implicit none
	
	integer (kind=i2b), intent(in) :: inputunit
    real (kind=r8b), intent(in) :: CorrectedDistanceThr_int_rad
	type(track),intent(in) :: track_i
    type(point),intent(in) :: internalintersectpoint  
   
	logical :: firstinternindexfound, trackindexrangedefined    
	integer (kind=i4b) :: i,search_pa_min, search_pa_max
    real (kind=r8b) :: ang_rec_interspt 
    type(GLA12shot) :: recdataA
    
    firstinternindexfound = .false.
    trackindexrangedefined = .false.
    search_pa_min= 0
    search_pa_max= 0
                                      
	do i = track_i%firstrecnum,track_i%lastrecnum 						

		recdataA = readrec(inputunit,i)
				
		ang_rec_interspt = latlon_angle_rad(recdataA%shot_loc%lat_degr,recdataA%shot_loc%lon_degr   &
           ,internalintersectpoint%pole_coord%lat_degr  &
           ,internalintersectpoint%pole_coord%lon_degr)

		if (.not.firstinternindexfound) then                     
           if (ang_rec_interspt <= CorrectedDistanceThr_int_rad) then
              firstinternindexfound = .true.
              search_pa_min = i
              if (i == track_i%lastrecnum) then
                 search_pa_max = i 
                 trackindexrangedefined = .true.
                 exit
              else
                 cycle
              end if  
           end if
        end if

       if (firstinternindexfound) then
           if ((ang_rec_interspt > CorrectedDistanceThr_int_rad).or.  &
               (i == track_i%lastrecnum)) then
                search_pa_max = i 
                trackindexrangedefined = .true.
                exit
           end if 
       end if                              

	end do       

	if (trackindexrangedefined) then
    	range_inters%lower_limit = search_pa_min
    	range_inters%upper_limit = search_pa_max
    else
    	range_inters%lower_limit = 0
    	range_inters%upper_limit = 0
    end if        

end function


!------------------------
! case of non-parallel tracks with internal intersection pole

subroutine internalintersection(inputunit_a, inputunit_b, track_a, track_b  &
			, internalintersectpoint, CorrectedDistanceThr_int_rad,trackcouple)

	integer (kind=i2b), intent(in) :: inputunit_a, inputunit_b
	type(track),intent(in) :: track_a, track_b
    real (kind=r8b) :: CorrectedDistanceThr_int_rad
    type(point) :: internalintersectpoint

    type(track_couple), intent(inout) :: trackcouple
    
	type(range) :: track_a_range, track_b_range



    !------------------------
    ! setting of analysis case id
	trackcouple%caseid = 3	!non-parallel internal
    
	! determination of range for track -a-
	track_a_range = range_inters(inputunit_a,track_a,internalintersectpoint, CorrectedDistanceThr_int_rad)

	! determination of range for track -b-
	track_b_range = range_inters(inputunit_b,track_b,internalintersectpoint, CorrectedDistanceThr_int_rad) 

	if((track_a_range%lower_limit==0).or.  &
		(track_a_range%upper_limit==0).or.  &  
		(track_b_range%lower_limit==0).or.  &
		(track_b_range%upper_limit==0)) then
        return
    end if

	! elaboration of shots of tracks -a- and -b- internal to the related ranges
	call analyze_recinterval(inputunit_a,inputunit_b,track_a_range,track_b_range,trackcouple)

                          
end subroutine internalintersection

!------------------------


!------------------------
! case of non-parallel tracks with external intersection pole

subroutine externalintersection(inputunit_a, inputunit_b,track_a, track_b  &
			, CorrectedDistanceThr_int_rad, minangdist_Rp, isnearest_firstrec_Rp, trackcouple)

	integer (kind=i2b), intent(in) :: inputunit_a, inputunit_b
	type(track), intent(in) :: track_a, track_b
    real (kind=r8b), intent(in) :: CorrectedDistanceThr_int_rad
	real (kind=r8b), intent(in) :: minangdist_Rp(4)
	logical, intent(in) :: isnearest_firstrec_Rp(4)

    type(track_couple), intent(inout) :: trackcouple
   
	integer (kind=i4b) :: i, external_pa_lastindex, external_pb_lastindex


	logical :: isnearestfirstrec_pa, isnearestfirstrec_pb

    
	real (kind=r8b) :: ang_rec_externspt

	integer (kind=i4b) :: init_rec_pa, final_rec_pa, step_pa &
					 ,init_rec_pb, final_rec_pb, step_pb
                     
	real (kind=r8b) :: minangdist_pa, minangdist_pb

	type(point) :: intersectpointA, intersectpointB, inregion_pole   &
              , nearestexternalpoint

    type(GLA12shot) :: recdataA, recdataB
    type(range) :: range_pa, range_pb



    !------------------------
    ! setting of analysis case id
	trackcouple%caseid = 4   !non-parallel external
    
	! check on derived angular distances:
    ! angular distances between an intersection pole and the track extremes
    ! must be always greater or smaller than those of the other intersection pole 
    ! (ie. the pole symmetrical with respect to Earth centre)  
	if ((minangdist_Rp(1)-minangdist_Rp(3))*(minangdist_Rp(2)-minangdist_Rp(4))<=0) then
       write(*,*) 'error in algorithm structure vs. specific data range'
       write(*,*) 'PROGRAM NOW STOPS'    
       read(*,*)
       stop	! STOP PROGRAM FOR MAJOR ERROR IN DATA INPUT
    end if                    
      
	! definition of intersection pole nearest 
    ! to the investigated area, and of the related distances             

    if ((minangdist_Rp(1)-minangdist_Rp(3))>0) then
		nearestexternalpoint = intersectpointB
        minangdist_pa = minangdist_Rp(3)
        minangdist_pb = minangdist_Rp(4)
        isnearestfirstrec_pa = isnearest_firstrec_Rp(3)
        isnearestfirstrec_pb = isnearest_firstrec_Rp(4)                                             
    else 
		nearestexternalpoint = intersectpointA
        minangdist_pa = minangdist_Rp(1)
        minangdist_pb = minangdist_Rp(2)  
        isnearestfirstrec_pa = isnearest_firstrec_Rp(1)
        isnearestfirstrec_pb = isnearest_firstrec_Rp(2)                          
    end if


    ! comparison between the two tracks skipped
    ! if the minimum distance for at least one of the tracks is larger than the maximum allowed 
	if ((minangdist_pa > CorrectedDistanceThr_int_rad) .or.  &
		(minangdist_pb > CorrectedDistanceThr_int_rad)) then
		return
    end if              
       

    ! define search and increment parameters for external analysis
    if (isnearestfirstrec_pa) then
       init_rec_pa = track_a%firstrecnum 
       final_rec_pa = track_a%lastrecnum
       step_pa = 1
    else
       init_rec_pa = track_a%lastrecnum  
       final_rec_pa = track_a%firstrecnum
       step_pa = -1  
    end if                    


    if (isnearestfirstrec_pb) then
       init_rec_pb = track_b%firstrecnum 
       final_rec_pb = track_b%lastrecnum
       step_pb = 1
    else
       init_rec_pb = track_b%lastrecnum  
       final_rec_pb = track_b%firstrecnum
       step_pb = -1  
    end if   

	! determination of index ranges for shots of the two tracks inside the -corrected distance range-  

	! 1 - track -a-

    external_pa_lastindex= 0
                                                          
	external_search_in_trackA: do i = init_rec_pa,final_rec_pa,step_pa 						

		recdataA = readrec(inputunit_a,i)
				
		ang_rec_externspt = latlon_angle_rad(recdataA%shot_loc%lat_degr,recdataA%shot_loc%lon_degr   &
           ,nearestexternalpoint%pole_coord%lat_degr  &
           ,nearestexternalpoint%pole_coord%lon_degr)                                                                
                    
        if ((ang_rec_externspt > CorrectedDistanceThr_int_rad).or.  &
           (i == final_rec_pa)) then
            external_pa_lastindex = i
            exit
        end if
                                              
        end do external_search_in_trackA           

		! 2 - track -b-

        external_pb_lastindex= 0
                                                          
		external_search_in_trackB: do i = init_rec_pb,final_rec_pb,step_pb 						

			recdataB = readrec(inputunit_b,i)
				
			ang_rec_externspt = latlon_angle_rad(recdataB%shot_loc%lat_degr,recdataB%shot_loc%lon_degr   &
               ,nearestexternalpoint%pole_coord%lat_degr  &
               ,nearestexternalpoint%pole_coord%lon_degr)                                                                
                    
            if ((ang_rec_externspt > CorrectedDistanceThr_int_rad).or.  &
               (i == final_rec_pb)) then
               external_pb_lastindex = i
               exit
            end if
                                              
            end do external_search_in_trackB

            if (isnearestfirstrec_pa) then
               range_pa%lower_limit = init_rec_pa
               range_pa%upper_limit = external_pa_lastindex
            else
               range_pa%lower_limit = external_pa_lastindex
               range_pa%upper_limit = init_rec_pa
            end if
					

            if (isnearestfirstrec_pb) then
               range_pb%lower_limit = init_rec_pb
               range_pb%upper_limit = external_pb_lastindex
            else
               range_pb%lower_limit = external_pb_lastindex
               range_pb%upper_limit = init_rec_pb
            end if                

            ! analysis of data falling into the -corrected distance range- 
			call analyze_recinterval(inputunit_a, inputunit_b,range_pa,range_pb, trackcouple)                    


end subroutine externalintersection

!------------------------


!------------------------
! analysis of a track pair

subroutine track_couple_analysis(inputunit_a, inputunit_b,track_par_a,track_par_b, trackcouple)

	integer (kind=i2b), intent(in) :: inputunit_a, inputunit_b
    type (track), intent(in)  :: track_par_a,track_par_b
	type(track_couple), intent(inout) :: trackcouple
    
	real (kind=r8b) :: angle_btwn_tracks, min_angle_btwn_tracks    


	write(*,*) '... now analysing track pair: ',track_par_a%id,', ',track_par_b%id


	! definition of total number of couples between the two tracks
	trackcouple%totalrecords = track_par_a%numrec*track_par_b%numrec


	! definition of analysis case

	! short track(s) - one or both tracks with length less than 5,000 m
    if (track_par_a%ang_length*EarthRadius_m < mintracklength .or. &
        	track_par_b%ang_length*EarthRadius_m < mintracklength) then            

    	! setting of analysis case id
		trackcouple%caseid = 0	!short-complete
		call integraltracks(inputunit_a, inputunit_b,track_par_a,track_par_b, trackcouple)
                            
	else ! both tracks with length >= 5,000 m
    
        ! determination of the minimum angle between the great circles representing the two tracks
		angle_btwn_tracks = vector_angle_rad(track_par_a%profGCpoint%vect_coord    &
            ,track_par_b%profGCpoint%vect_coord)
        min_angle_btwn_tracks = min(angle_btwn_tracks,pi-angle_btwn_tracks)
		
        ! subparallel tracks   
        if (min_angle_btwn_tracks < pi/360.0) then 
           
			! subparallel tracks with opposite sense of movement 
			if (angle_btwn_tracks >= pi/2.0) then
                ! setting of analysis case id
				trackcouple%caseid = 1	!parallel-complete
				call integraltracks(inputunit_a, inputunit_b,track_par_a,track_par_b,trackcouple)
			! subparallel tracks with same sense of movement                 
            else
                ! setting of analysis case id
				trackcouple%caseid = 2	!parallel-optimised
        		call paralleltracks(inputunit_a, inputunit_b,track_par_a,track_par_b,trackcouple)
            end if 	
            		
		! nonparallel tracks 
        else
        	call nonparalleltracks(inputunit_a, inputunit_b,track_par_a,track_par_b  &
         		, searchradius_rad, min_angle_btwn_tracks, trackcouple)          

        end if                           

	end if 


end subroutine track_couple_analysis


end module trackcouple_analysis


!------------------------------------------------
!------------------------------------------------


module inputoutput_management

use trackcouple_analysis


character (len=37) :: InputFile, OutputFile
integer (kind=i2b) :: i_cond, o_cond
logical :: exists


contains


!------------------------
! choose input case for analysis, one or two input file

integer (kind=i1b) function DefineInputNumber () result(NumberInputFile)

          
	do
		write(*,"(A)",ADVANCE="no") 'Enter the number of input file (1, 2): '
		read(*,*) NumberInputFile

        if ((NumberInputFile == 1) .or.  &
			(NumberInputFile == 2)) then
            exit
        else
        	write(*,*) 'Input error.'
        end if
    end do
    
	write (*,*)
    
    
end function DefineInputNumber


!------------------------
! define input file(s)

subroutine InputFileDefinition(NumberInputFile,i,inputfilename,inputfileunit)

integer (kind=i1b), intent(in)  :: NumberInputFile, i

integer (kind=i2b), intent(out) :: inputfileunit   
character (len=*), intent(out) :: inputfilename

character (len=*), parameter  :: inputfileorder(3) = (/'       ',' first ',' second'/)


do
	! processing of input file

	! 	name definition 
	write(*,"(A)", ADVANCE="no") 'Enter the name of'//trim(inputfileorder(i+NumberInputFile-1))// &
    		' input file (binary file with .dat extension): '
	read(*,*) inputfilename

	inquire(file=inputfilename,exist=exists)
	if (.NOT.exists) then
		write(*,*) 'Input file not found'
		cycle        
	end if

	! 	open input file with direct access
	inputfileunit = 14+i      
	open(unit=inputfileunit,file=inputfilename,status='old',access='direct'  &
        ,form='unformatted',recl=record_length, iostat=i_cond)
	if (i_cond /= 0) then
		write(*,*) 'Input file not opened'
		cycle
	end if

	exit
    
end do
        
write(*,*)

end subroutine InputFileDefinition

!------------------------



!-------------------------
! defines the output files containing the results and the metadata

subroutine OutputFilesDef() 

character (len= 37) :: OutputFileName
integer (i2b) :: ios

do
  	
	!definition of output file name
	write(*,"(A)", ADVANCE="no") 'Enter output filename (without extensions): '
	read(*,*) OutputFileName

	! creates sequential-access output file (for GIS import)
	open (unit=18,file=trim(OutputFileName)//'_gis.txt',status='NEW' &
  		, access='sequential', form='formatted', iostat=ios)

	if (ios /= 0) then
  		write (*,"(A,/,A)") 'Error with output file creation.','Change name'
  		cycle
	end if


  	! creates metadata output file 
	open (unit=19,file=trim(OutputFileName)//'_md.txt',status='NEW' &
  		, access='sequential', form='formatted', iostat=ios)

	if (ios /= 0) then
  		write (*,"(A,/,A)") 'Error with output file creation.','Change name'
  		cycle
	end if

    exit
  
end do  

write (*,*)


end subroutine OutputFilesDef

!------------------------



!------------------------
! input of search radius
subroutine SearchRadiusDefinition()

write (*,"(A)",ADVANCE="no") 'Enter search radius between records (in meters, suggested 100 to 500-1000): '
read (*,*) searchradius_m

if (searchradius_m <= 0) then
	write (*,*) 'Error in search radius input. Program will stop (hit any key)'
	stop	! STOP PROGRAM FOR MAJOR ERROR IN DATA INPUT
else
	write (*,*)
end if
    
searchradius_rad = searchradius_m/EarthRadius_m


end subroutine SearchRadiusDefinition

!------------------------



! ---------------------
! define the tolerance factors for sub-parallel and intersecting tracks

subroutine ToleranceFactorDefinition ()

	do
		write(*,"(A)") 'Enter the tolerance factors for sub-parallel and intersecting tracks (2 positive integers)'
		write(*,"(A)") '- to use one or both default values, i.e. 10 and 50, enter 0 for each default value (e.g. 0 0) -'
		read(*,*) ToleranceFactors

        if ((ToleranceFactors(1) < 0) .or. &
            (ToleranceFactors(2) < 0)) then
            cycle
        end if
        
        if (ToleranceFactors(1) == 0) then
            ToleranceFactors(1) = ToleranceFactorsDefault_paralleltracks
        end if

        if (ToleranceFactors(2) == 0) then
            ToleranceFactors(2) = ToleranceFactorsDefault_intersectingtracks
        end if

        exit
        
    end do
    
	write (*,*)
    
    
end subroutine ToleranceFactorDefinition

!------------------------



!------------------------
! input of delta elevation range
subroutine  DeltaElevRange()

do
	write (*,"(A)",ADVANCE="no") 'Enter absolute values of min/max uncorrected elevation difference (in m, e.g. 0 5): '
	read (*,*) deltaelev_min, deltaelev_max

	if (.not.((deltaelev_min>=0) .and.deltaelev_max > deltaelev_min)) then
      write (*,*) 'Error in min and max allowed uncorrected elevation difference'
      cycle
    end if

    exit

end do

write (*,*)


end subroutine DeltaElevRange

!------------------------


!------------------------
! write header of output file

subroutine ResultOutput_Header()
	   
	write(18,*)  'shotid_1',',','trackid_1',',','shotid_2',',','trackid_2',',','time_diff'   &
   		,',','year_1',',','month_1',',','day_1',',','hr_1',',','min_1',',','sec_1'	&
   		,',','year_2',',','month_2',',','day_2',',','hr_2',',','min_2',',','sec_2'	& 
   		,',','lat_1',',','lon_1',',','lat_2',',','lon_2',',','dist_12'              &
        ,',','elev1_uncor',',','elev2_uncor',',','D_elev_uncor'   &
        ,',','elev1_cor',',','elev2_cor',',','D_elev_cor'  


end subroutine ResultOutput_Header

!------------------------


!------------------------
! header for metadata file
subroutine MetaData_Header(time_values_initial)

integer, dimension(8) :: time_values_initial

! begin time of analysis
write (19,"(A)") 'Metadata for height variations GLA12 analysis'
write (19,"(2X,A,2X,I4,A,I2,A,I2)") 'analysis of:',time_values_initial(1),'/',time_values_initial(2),'/',time_values_initial(3)
write (19,*)


end subroutine MetaData_Header

!------------------------


!------------------------
! write input settings information in metadata file
subroutine MetaData_InputSettings(NumberInputFile, inputfilename)


integer (kind=i1b), intent(in)  :: NumberInputFile
character (len=50), intent(in) :: inputfilename(2)

integer (kind=i1b) :: i

write (19,"(1X,A)") 'Input settings for analysis'
write (19,*)

! number of used input file
write (19,"(1X,A,2X,I4,A,I2,A,I2)") 'Number of input file: ',NumberInputFile

! name(s) of input files
write (19,"(1X,A)") 'Input file name(s): '
do i=1,NumberInputFile
	write (19,"(3X,A)") inputfilename(i) 
end do
write (19,*)

! search radius
write (19,"(1X,A,F7.1,1X,A)") 'Search radius: ',searchradius_m,'meters' 
write (19,*)

! tolerance factors
write (19,"(1X,A)") 'Tolerance factors'
write (19,"(3X,A,1X,i5)") 'Sub-parallel tracks: ',ToleranceFactors(1) 
write (19,"(3X,A,1X,i5)") 'Intersecting tracks: ',ToleranceFactors(2) 
write (19,*)


! delta elevation boundaries
write (19,"(1X,A)") 'Allowed range of uncorrected elevation differences (absolute values, meters)'
write (19,"(3X,A,1X,F6.1,4X,A,1X,F6.1)") 'min: ',deltaelev_min,'max: ',deltaelev_max 
write (19,*)

end subroutine MetaData_InputSettings

!------------------------


!------------------------
! write tracks summary data in metadata file
subroutine MetaData_Track(NumberInputFile, inputfilename, NumRecordFile, TotalTrackNumber, track_par)

integer (kind=i1b), intent(in) :: NumberInputFile
character (len=50), intent(in) :: inputfilename(2)
integer (kind=i4b) :: NumRecordFile(2)
integer (kind=i4b), intent(in) :: TotalTrackNumber(2)
type(track), intent(in):: track_par(2,maxNumTracks)


integer (kind=i1b) :: i

! header
write (19,"(10X,A)") 'Summary of data analysis'
write (19,*)

! total number of data in input file(s)
write (19,"(1X,A)") 'Total number of data in input file(s)'
do i=1,NumberInputFile
	write (19,"(4X,A,A,3X,I7)") trim(inputfilename(i)),': ',NumRecordFile(i)
end do

write (19,*)

! summary characteristics for tracks
do i=1,NumberInputFile
  
	write(19,"(3A,I0)") 'Total number of tracks in input file ',trim(inputfilename(i)),': ',TotalTrackNumber(i)
	write(19,"(2X,A,2X,I0,2X,A,2X,I0)") 'Code numbers from',track_par(i,1)%id,'to',track_par(i,TotalTrackNumber(i))%id 

end do

write (19,*)

! detailed characteristics for tracks
write(19,"(A)") 'Detailed characteristics of tracks'
do i=1,NumberInputFile

	write(19,"(A,I0)") 'Input file # ',i
	write(19,"(A,5X,A)") 'Track #','Num. rec.'    

    do j= 1,TotalTrackNumber(i)
  
		write(19,"(I6,7X,I6)") track_par(i,j)%id,track_par(i,j)%numrec 

	end do


end do

write (19,*)

end subroutine MetaData_Track

!------------------------


!------------------------
! write track pair header in metadata file
subroutine MetaData_TrackCoupleHeader()

	write(19,"(A)") 'Track pair summary'
    write(19,"(3X,3(A,5X),12X,A,8X,A)") 'Track1','Track2','Case','N tot','N pair'

end subroutine MetaData_TrackCoupleHeader

!------------------------


!------------------------
! write track couple information in metadata file
subroutine MetaData_TrackCouple(trackcouple)
	
	type(track_couple), intent(in) :: trackcouple
    

	write (19,"(2(I8,3X),A20,2X,I10,2X,I8)") trackcouple%tracka_id,trackcouple%trackb_id   &
    	,parse_analysiscase(trackcouple%caseid),trackcouple%totalrecords,trackcouple%foundrecords


end subroutine MetaData_TrackCouple

!------------------------


!------------------------
! write final information in metadata file
subroutine MetaData_Final(time_values_initial,time_values_final)

integer, dimension(8) :: time_values_initial,time_values_final


write (19,*) 
write (19,*) '------------------------------------------------'
write (19,*) 
write (19,"(A,2X,I4,2(A,I0))") 'analysis of:',time_values_initial(1)  &
				,'-',time_values_initial(2),'-',time_values_initial(3)
write (19,"(A,4X,I2,A,I2,A,I2)") 'starts at:',time_values_initial(5)    &
				,':',time_values_initial(6),':',time_values_initial(7)


write (19,"(A,6X,I2,A,I2,A,I2)") 'ends at:'   &
    ,time_values_final(5),':',time_values_final(6),':',time_values_final(7)
write (19,*) 
write (19,*) '------------------------------------------------'


end subroutine MetaData_Final

!------------------------

  
!------------------------
! writes program header on screen of the program

subroutine OutputScreen_01()

write (*,*)
write (*,*)
write (*,*) '*****************************************'
write (*,*)
write (*,*) '               HeightVarGLA'
write (*,*)
write (*,*)	'   	         vers. 1.0'
write (*,*) '                 2008-07'
write (*,*)
write (*,*) ' program for analysing height variations'
write (*,*) '            from GLA12 files'
write (*,*) 
write (*,*) 
write (*,*) '*****************************************'
write (*,*)
write (*,*)
write (*,*) 'Input parameters definitions:'
write (*,*)

end subroutine OutputScreen_01

!-----------------------------


!-----------------------------
! write summary information on the screen

subroutine OutputScreen_02(NumberInputFile, inputfilename, TotalTrackNumber, track_par)

integer (kind=i1b), intent(in) :: NumberInputFile
character (len=50), intent(in) :: inputfilename(2)
integer (kind=i4b), intent(in) :: TotalTrackNumber(2)
type(track), intent(in):: track_par(2,maxNumTracks)

integer (kind=i1b) :: i


do i=1,NumberInputFile
  
	write(*,"(3A,I0)") 'Total number of tracks in input file ',inputfilename(i),': ',TotalTrackNumber(i)
	write(*,"(2X,2(A,I0))") 'Code numbers from : ',track_par(i,1)%id,' to: ',track_par(i,TotalTrackNumber(i))%id 

end do


end subroutine OutputScreen_02

!-----------------------------



end module inputoutput_management



!------------------------------------------------
!------------------------------------------------
!------------------------------------------------
!------------------------------------------------


program HeightVarGLA

use inputoutput_management

implicit none

integer, dimension(8) :: time_values_initial, time_values_final ! analysis date/time information
    
integer (kind=i1b) :: NumberInputFile, i
integer (kind=i4b) :: NumRecordFile(2)
integer (kind=i4b) :: TotalTrackNumber(2), p_a, p_b    
type(vector) :: profGCvector

type(track):: track_par(2,maxNumTracks)
character (len=50) :: inputfilename(2)
integer (kind=i2b) :: inputfileunit(2)

type(track_couple) :: trackcouple



!-------------------------------
! gets the initial time_date of analysis 
call DATE_AND_TIME(VALUES=time_values_initial)


! ---------------------
! writes program header on screen
call OutputScreen_01()

  
! ---------------------
! define input case for analysis, one or two input file
NumberInputFile = DefineInputNumber()
  

! ---------------------
! define and open input file
do i=1,NumberInputFile
	call InputFileDefinition(NumberInputFile,i,inputfilename(i),inputfileunit(i))
end do
  

! ---------------------
! define and open output file 
call OutputFilesDef()


! ---------------------
! writes metadata file header
call MetaData_Header(time_values_initial)

  
! ---------------------
! define the search radius (in meters)
call SearchRadiusDefinition()


! ---------------------
! define the tolerance factors for sub-parallel and intersecting tracks
call ToleranceFactorDefinition()

! ---------------------
! calculates corrected distances for sub-parallel tracks case
CorrectedDistanceThr_par_rad = searchradius_rad*ToleranceFactors(1)


! ---------------------
! optionally define a delta_elevation range (in meters)
call DeltaElevRange()


! ---------------------
! write relevant previously defined  information into metadata file
call MetaData_InputSettings(NumberInputFile, inputfilename)


! ---------------------
! determine total number of data in input file(s)

do i=1,NumberInputFile
	NumRecordFile(i) = FileRecordCount(inputfileunit(i)) 
end do
    
        
! ---------------------
! determine initial and final records for each track

do i=1,NumberInputFile
	call prof_initfinrecs(inputfileunit(i),NumRecordFile(i), maxNumTracks, track_par(i,:), TotalTrackNumber(i))
end do


! ---------------------
! number of records for track

do i=1,NumberInputFile
	do p = 1,TotalTrackNumber(i)  
		track_par(i,p)%numrec = track_par(i,p)%lastrecnum - track_par(i,p)%firstrecnum +1
	end do
end do 


! ---------------------
! beginning time of track

do i=1,NumberInputFile
	do p = 1,TotalTrackNumber(i)  
		call track_begintime(inputfileunit(i),track_par(i,p)) 
	end do
end do


! ---------------------
! coordinates of boundary records

do i=1,NumberInputFile
	do p = 1,TotalTrackNumber(i)
		call track_boundcoord(inputfileunit(i),track_par(i,p))
	end do
end do


! ---------------------
! ang_length of track

do i=1,NumberInputFile
	do p = 1,TotalTrackNumber(i)
	call track_length_rad(track_par(i,p))
	end do
end do


! ---------------------
! great circle pole to track

do i=1,NumberInputFile      
	do p = 1,TotalTrackNumber(i)
		call track_greatcirclepole(track_par(i,p))
	end do
end do


! ---------------------
! increments in tracks

do i=1,NumberInputFile 
	do p = 1,TotalTrackNumber(i)
		call track_increments(inputfileunit(i),track_par(i,p))
	end do
end do


! ---------------------
! write summary information in the screen
call OutputScreen_02(NumberInputFile, inputfilename, TotalTrackNumber, track_par)


!------------------------
! write tracks data in metadata file
call MetaData_Track(NumberInputFile, inputfilename, NumRecordFile, TotalTrackNumber, track_par)


! ---------------------
! write header of output file
call ResultOutput_Header()


! ---------------------
! analysis of data by comparison of track couples

write(*,*)
write(*,"(3X,A)") 'Ongoing analysis. Please wait ....'


! write track couple header in metadata file
call MetaData_TrackCoupleHeader()

! case of single input file
if (NumberInputFile==1) then

	! 
    ! cycle on tracks from first to penultimate
	do p_a = 1,(TotalTrackNumber(1)-1)
    
		! cycle on tracks from track following the current, to the last 
		do p_b = p_a+1,TotalTrackNumber(1)

        	call trackcouplepar_initial(trackcouple, track_par(1,p_a)%id, track_par(1,p_b)%id   &
            	,track_par(1,p_a)%numrec, track_par(1,p_b)%numrec)
			call track_couple_analysis(inputfileunit(1),inputfileunit(1)  &
            		,track_par(1,p_a),track_par(1,p_b),trackcouple)
            call MetaData_TrackCouple(trackcouple)

		end do 

	end do 
    
! case of two input file
else 

	! cycle on tracks from first to penultimate
	do p_a = 1,TotalTrackNumber(1)
    
		! cycle on tracks from track following the current, to the last 
		do p_b = 1,TotalTrackNumber(2)

               
        	call trackcouplepar_initial(trackcouple, track_par(1,p_a)%id, track_par(2,p_b)%id   &
            	,track_par(1,p_a)%numrec, track_par(2,p_b)%numrec)
			call track_couple_analysis(inputfileunit(1),inputfileunit(2)  &
            		,track_par(1,p_a),track_par(2,p_b),trackcouple)
            call MetaData_TrackCouple(trackcouple)
            
		end do 

	end do        

end if
    

!-------------------------------
! gets final time_date of analysis 
call DATE_AND_TIME(VALUES=time_values_final)


! ---------------------
! write final time information into metadata file
call MetaData_Final(time_values_initial,time_values_final)


! ---------------------
! write previously defined relevant information into metadata file
write (*,*)
write (*,*)
write (*,*) '***************************************************'
write (*,*)
write (*,*) 'Analysis completed. Press any key to close program.'
read (*,*)
 
end program HeightVarGLA
