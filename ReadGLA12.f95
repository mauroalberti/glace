! fortran 90/95 program
! for icesat gla12 reading
!
! Mauro Alberti,
! July-November, 2007; April 2016
! 

! part of code inspired to Fortran programs in NSIDC web site

! modified:
! 2008-09-23: changed InputFile to GLA12FileName, and its width to 45 (string then trimmed);
! 2010-06-26: output GIS data to multiple files; read analysis parameters for a text file
! 2016-04-30: adapted to release 34

!------------------------------------------------    

   
module kinds_mod

	integer, parameter :: i1b = selected_int_kind(1)                    
	integer, parameter :: i2b = selected_int_kind(4)                    
	integer, parameter :: i4b = selected_int_kind(9)  
	integer, parameter :: r4b = selected_real_kind( 6, 37)              
	integer, parameter :: r8b = selected_real_kind(15,307) 

	real (kind=r8b), parameter :: div_D06=1.0D06, div_D03=1.0D03, div_D02=1.0D02 

	! GLA input invalid_values
	integer (kind=i1b),parameter :: gi_invalid_i1b=127
	integer (kind=i2b),parameter :: gi_invalid_i2b=32767
	integer (kind=i4b),parameter :: gi_invalid_i4b=2147483647

	real (kind=r4b),parameter :: gi_invalid_r4b = huge(1.0_r4b)
	real (kind=r8b),parameter :: gi_invalid_r8b = huge(1.0_r8b)


 
end module kinds_mod


!------------------------------------------------


!------------------------------------------------

module time_processing

use kinds_mod

type :: date
sequence
    integer (kind=i2b) :: year, month, day, hour, minutes
    real (kind=r4b) :: seconds 
end type date

contains

!-----------------------------
!  '..... to provide the calculation of Julian dates,
!  after the C routines found in _ACM SIGNUM Newsletter_
!  v 30, n 4, October 1995, beginning on p 14,
!  "Algorithms for Julian Dates" by Richard L. Branham, Jr.'
!  'Translated to Fortran 90 by Dan Nagle, January, 1996.'
!  Modified for inclusion into present program by Mauro Alberti, November, 2007
!  Origin time_date is 2000-01-01 12:00:00. 

real(kind=r8b) function date_to_j2000_sec(time_date) result(julian_time_sec)

type(date), intent(in) :: time_date

real(kind=r8b) :: julian_time_days 
!  days prior to beginning of each month (ly: leap year)
integer, parameter, dimension(13) :: &
   bgn_day = (/0,31,59,90,120,151,181,212,243,273,304,334,365/), &
   bgn_day_ly = (/0,31,60,91,121,152,182,213,244,274,305,335,366 /)   


! calculate julian days at year start (1461 days in 4 years starting 4713 BC)
julian_time_days = real(((1461*(4712 + time_date%year)-1)/4), kind=r8b)

! add month day number (except the days in current month) 
if( mod(time_date%year,4_i2b) == 0 )then         
	julian_time_days = julian_time_days + real( bgn_day_ly(time_date%month), r8b)  !leap year
else                                                     
	julian_time_days = julian_time_days + real( bgn_day(time_date%month), r8b)
endif  

! add day number of current month
julian_time_days = julian_time_days + real(time_date%day,kind=r8b) 

! correct for difference between astronomical and civil time_date, i.e. civil day less 12 hours
julian_time_days = julian_time_days - 0.5_r8b                        

! correct for origin time at 2000-01-01
julian_time_days = julian_time_days - 2451558.0_r8b

! converts to seconds and adds hours, minutes and seconds
julian_time_sec = julian_time_days*86400.0_r8b + real(time_date%hour,kind=r8b)*3600.0_r8b &
				+ real(time_date%minutes,kind=r8b)*60.0_r8b + real(time_date%seconds,kind=r8b)


end function date_to_j2000_sec

!-----------------------------



!-----------------------------
! PURPOSE: Convert J2000 seconds to date 
!  in a structure containing year,month,day, hour,minutes,seconds

type(date) function date_from_j20002(J2000time_sec) result(time_date)

implicit none

real (kind=r8b), intent(in) :: J2000time_sec
integer (kind=i2b) :: i
integer (kind=i4b) :: J2000time
real (kind=r8b) :: residual_sec

integer (kind=i2b):: days_months (12)
integer (kind=i4b) :: month_sec_upp, month_sec

integer (kind=i4b), parameter :: day_secs = 86400

!---------------

days_months = (/31,28,31,30,31,30,31,31,30,31,30,31/)

J2000time = ceiling(J2000time_sec)

SELECT CASE (J2000time)
case(:-43201)
	time_date%year = 9999
    residual_sec = 9999
case(-43200:31579199)
	time_date%year = 2000
    residual_sec = J2000time_sec + 43200.0
case(31579200:63115199)
	time_date%year = 2001
    residual_sec = J2000time_sec - 31579200.0
case(63115200:94651199)
	time_date%year = 2002
    residual_sec = J2000time_sec - 63115200.0
case(94651200:126187199)
	time_date%year = 2003
    residual_sec = J2000time_sec - 94651200.0
case(126187200:157809599)
	time_date%year = 2004
    residual_sec = J2000time_sec - 126187200.0
case(157809600:189345599)
    time_date%year = 2005
    residual_sec = J2000time_sec - 157809600.0
case(189345600:220881599)
    time_date%year = 2006
    residual_sec = J2000time_sec - 189345600.0
case(220881600:252417599)
    time_date%year = 2007
    residual_sec = J2000time_sec - 220881600.0
case(252417600:284039999)
	time_date%year = 2008
    residual_sec = J2000time_sec - 252417600.0
case(284040000:315575999)
	time_date%year = 2009
    residual_sec = J2000time_sec - 284040000.0  
case(315576000:)
	time_date%year = 2010
    residual_sec = J2000time_sec - 315576000.0  
end select 

! set february days number for leap years
if ((time_date%year == 2000).or. &
  	(time_date%year == 2004).or. &
    (time_date%year == 2008)) then
	days_months(2)=29
end if

month_sec = 0
do i=1,12
  month_sec_upp = day_secs*days_months(i)+ month_sec
  if (residual_sec < month_sec_upp) then
    residual_sec = residual_sec - month_sec
    exit
  else
    month_sec = month_sec_upp
  end if
end do

time_date%month = i
time_date%day = int(residual_sec/day_secs)
residual_sec = residual_sec - (time_date%day*day_secs)
time_date%day = time_date%day+1
time_date%hour = int(residual_sec/3600.0)
residual_sec = residual_sec - (time_date%hour*3600)
time_date%minutes = int(residual_sec/60.0)
time_date%seconds = residual_sec - (time_date%minutes*60.0)	
 
  
end function date_from_j20002
    


end module time_processing

!------------------------------------------------




!------------------------------------------------ 


module GLA12structure


use time_processing


! GLA12 Input Record Structure

type     :: GLA12_prod_input
  sequence
  integer (kind=i4b) :: i_rec_ndx                 
  integer (kind=i4b) :: i_UTCTime           (2)       
  integer (kind=i2b) :: i_transtime               
  integer (kind=i1b) :: i_Spare1            (2)                  
  integer (kind=i4b) :: i_deltagpstmcor           
  integer (kind=i4b) :: i_dShotTime         (39)      
  integer (kind=i4b) :: i_lat               (40)      
  integer (kind=i4b) :: i_lon               (40)      
  integer (kind=i4b) :: i_elev              (40) 
  integer (kind=i1b) :: i_campaign          (2)  
  integer (kind=i2b) :: i_spare40  
  integer (kind=i4b) :: i_cycTrk
  integer (kind=i4b) :: i_localSolarTime 
  integer (kind=i4b) :: i_spare41           (7) 
  integer (kind=i2b) :: i_deltaEllip        (40)   
  integer (kind=i4b) :: i_beamCoelv         (40)
  integer (kind=i4b) :: i_beamAzimuth       (40)
  integer (kind=i4b) :: i_d2refTrk          (40)
  integer (kind=i4b) :: i_SigBegOff         (40)
  integer (kind=i1b) :: i_DEM_hires_src     (40)
  integer (kind=i2b) :: i_DEMhiresArElv     (9,40)
  integer (kind=i2b) :: i_ElevBiasCorr      (40) 
  integer (kind=i2b) :: i_GmC               (40)  
  integer (kind=i2b) :: i_spare42           (3, 40) 
  integer (kind=i2b) :: i_sigmaatt          (40)      
  integer (kind=i4b) :: i_Azimuth                 
  integer (kind=i4b) :: i_SolAng                  
  integer (kind=i4b) :: i_tpintensity_avg         
  integer (kind=i2b) :: i_tpazimuth_avg           
  integer (kind=i2b) :: i_tpeccentricity_avg      
  integer (kind=i2b) :: i_tpmajoraxis_avg 
  integer (kind=i1b) :: i_poleTide          (2)                 
  integer (kind=i2b) :: i_gdHt              (2)       
  integer (kind=i2b) :: i_erElv             (2)       
  integer (kind=i2b) :: i_spElv             (4)       
  integer (kind=i2b) :: i_ldElv             (4) 
  integer (kind=i2b) :: i_spare12           (2)      
  integer (kind=i2b) :: i_wTrop             (2)       
  integer (kind=i2b) :: i_dTrop             (40)      
  integer (kind=i1b) :: i_surfType                
  integer (kind=i1b) :: i_spare11           (3)       
  integer (kind=i4b) :: i_DEM_elv           (40)      
  integer (kind=i4b) :: i_refRng            (40)      
  integer (kind=i4b) :: i_TrshRngOff        (40)      
  integer (kind=i4b) :: i_isRngOff          (40)      
  integer (kind=i4b) :: i_SigEndOff         (40)      
  integer (kind=i4b) :: i_cntRngOff         (40)      
  integer (kind=i4b) :: i_reflctUC          (40)      
  integer (kind=i4b) :: i_reflCor_atm             
  integer (kind=i2b) :: i_maxSmAmp          (40)  
  integer (kind=i2b) :: i_ocElv             (40)     
  integer (kind=i1b) :: i_numPk             (40)      
  integer (kind=i2b) :: i_kurt2             (40)      
  integer (kind=i2b) :: i_skew2             (40) 
  integer (kind=i1b) :: i_spare4            (160) 
  integer (kind=i4b) :: i_IsRngLast         (40)      
  integer (kind=i4b) :: i_IsRngFst          (40)      
  integer (kind=i2b) :: i_IceSVar           (40)      
  integer (kind=i1b) :: i_ElvuseFlg         (5)       
  integer (kind=i1b) :: i_atm_avail
  integer (kind=i1b) :: i_spare16           (4)  
  integer (kind=i1b) :: i_cld1_mswf
  integer (kind=i1b) :: i_MRC_af  
  integer (kind=i1b) :: i_spare9            (40) 
  integer (kind=i1b) :: i_ElvFlg            (40) 
  integer (kind=i2b) :: i_rng_UQF           (40) 
  integer (kind=i1b) :: i_spare49           (10) 
  integer (kind=i2b) :: i_timecorflg              
  integer (kind=i1b) :: i_APID_AvFlg        (8)       
  integer (kind=i1b) :: i_AttFlg2           (20)      
  integer (kind=i1b) :: i_spare5                  
  integer (kind=i1b) :: i_FrameQF                 
  integer (kind=i1b) :: i_OrbFlg            (2)       
  integer (kind=i1b) :: i_rngCorrFlg        (2)       
  integer (kind=i1b) :: i_CorrStatFlg       (2)  
  integer (kind=i1b) :: i_spare15           (8)            
  integer (kind=i2b) :: i_AttFlg1                 
  integer (kind=i1b) :: i_Spare6            (2)  
  integer (kind=i1b) :: i_spare44           (120) 
  integer (kind=i1b) :: i_satNdx            (40)
  integer (kind=i2b) :: i_satElevCorr       (40)
  integer (kind=i1b) :: i_satCorrFlg        (40)
  integer (kind=i2b) :: i_satNrgCorr        (40)
  integer (kind=i2b) :: i_spare13           (40) 
  integer (kind=i2b) :: i_gval_rcv          (40)
  integer (kind=i2b) :: i_RecNrgAll         (40)
  integer (kind=i2b) :: i_FRir_cldtop       (40)
  integer (kind=i1b) :: i_FRir_qaFlag       (40)
  integer (kind=i2b) :: i_atm_char_flag 
  integer (kind=i2b) :: i_atm_char_conf  
  integer (kind=i1b) :: i_spare48           (36) 
  integer (kind=i2b) :: i_FRir_intsig       (40)
  integer (kind=i1b) :: i_spare14           (120) 
  integer (kind=i2b) :: i_Surface_temp
  integer (kind=i2b) :: i_Surface_pres
  integer (kind=i2b) :: i_Surface_relh
  integer (kind=i2b) :: i_maxRecAmp         (40)
  integer (kind=i2b) :: i_sDevNsOb1         (40)
  integer (kind=i1b) :: i_pctSAT            (40)
  integer (kind=i2b) :: i_TxNrg             (40) 
  integer (kind=i2b) :: i_eqElv             (2)    
  integer (kind=i1b) :: i_spare7            (282) 
  

end type GLA12_prod_input


! Pole datum - defined by latitude and longitude, in decimal degrees

type :: pole
sequence
    real (kind=r8b) :: lat_degr, lon_degr
end type pole
  

! GLA12 Output Record Structure - subset of the input structure, with converted unit measure, plus identifiers of track and shot

type     :: GLA12_prod_output
  sequence

  integer (kind=i4b) :: id_shot				! id of shot
  character (len=45) :: GLA12sourcefile     		! GLA12 file name
  integer (kind=i4b) :: id_track  			! id of track	 	
  integer (kind=i4b) :: rec_ndx				! GLA12 record index
  real    (kind=r8b) :: UTCTime_julsec			! shot time in Julian seconds
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
  
end type GLA12_prod_output


integer (kind=i4b), parameter :: OutputRecLength = 133


end module GLA12structure


!------------------------------------------------



!------------------------------------------------

module UserInputs

use  GLA12structure

implicit none

integer (i2b) :: ios, dummy_int 
character (len= 37) :: InputFilesListName, InputParametersName 


! Analysis filtering parameter structure

type :: analysis_param

	! geographic parameters
	real (r8b) :: lat_min, lat_max, long_min, long_max
	! temporal parameters
	logical :: temporal_filtering
	real (r8b) :: timewindow_julsec(2) 
	type(date) :: timewindow_date(2)  
	! quality parameters
	logical :: data_quality_filtering
	integer (i2b) :: maxallow_SatElevCorr, maxallow_Gain

end type


contains

!-------------------------
! choice of input file storing the list of GLA12 files

subroutine InputFileDef()

	do

		write (*,"(A)", ADVANCE="no") 'Enter input list filename: '
		read (*,*) InputFilesListName

		open (unit=15,file=InputFilesListName,status='OLD',access='sequential', iostat=ios)

		if (ios /= 0) then
  			write(*,*) 'Input file not found - Choose again'
        else
          	exit
		end if

    end do

	write (*,*)

    
end subroutine InputFileDef

!-------------------------



!-------------------------
! define output files storing results

subroutine OutputFilesDef()

	character (len= 37) :: OutputFileName
  
    
	do
		write (*,"(A)", ADVANCE="no") 'Enter output filename (without extensions): '
		read (*,*) OutputFileName

		! create direct-access output file (for analysis with HeightVarGLA Fortran program)
		open (unit=17,file=trim(OutputFileName)//'_an.dat',status='NEW' &
  		, access='direct', form='unformatted',recl=OutputRecLength, iostat=ios)

		if (ios /= 0) then
  			write (*,"(A,/,A)") 'Error with output file creation.','Change name'
  			cycle
		end if
  

		! create metadata output file 
		open (unit=18,file=trim(OutputFileName)//'_md.txt',status='NEW' &
  		, access='sequential', form='formatted', iostat=ios)

		if (ios /= 0) then
  			write (*,"(A,/,A)") 'Error with output file creation.','Change name'
  			cycle
		end if

        exit

	end do

    write (*,*)
    

end subroutine OutputFilesDef

!-------------------------



!-------------------------
! define parameters of analysis
!   (i.e. geographical, time and quality subsetting)

subroutine ParamsDef(params)

	type(analysis_param), intent(out) :: params
    type(date):: dummy_date(2)
    
	do

		write (*,"(A)", ADVANCE="no") 'Enter parameters filename: '
		read (*,*) InputParametersName

		open (unit=20,file=InputParametersName,status='OLD',access='sequential', iostat=ios)

		if (ios /= 0) then
  			write(*,*) 'Input parameters file not found - Choose again'
        else
          	exit
		end if

    end do

	! reads the boundary region

    !! latitude
	read (20,*) params%lat_min, params%lat_max

	if (params%lat_min >= params%lat_max) then
		write (*,*) 'Error: minimum value of latitude larger or equal to maximum value'
		write (*,*) 'Program will stop'
        stop
	end if  


    !! longitude
	read (20,*) params%long_min, params%long_max

	if (params%long_min >= params%long_max) then
		write (*,*) 'Error: minimum value of longitude larger or equal to maximum value '
		write (*,*) 'Program will stop'
        stop
	end if


    !! time
	read (20,*) params%timewindow_date(1)%year &
    			,params%timewindow_date(1)%month,params%timewindow_date(1)%day &
                ,params%timewindow_date(2)%year,params%timewindow_date(2)%month  &
            	,params%timewindow_date(2)%day 

	if (params%timewindow_date(1)%year == -1) then
		params%temporal_filtering = .false.
	else 
		params%temporal_filtering = .true.
	end if
           
	if (params%temporal_filtering) then

    	! check initial time    
		if ((params%timewindow_date(1)%month<1).or. &
			(params%timewindow_date(1)%month>12).or.  &
			(params%timewindow_date(1)%day<1).or.  &
			(params%timewindow_date(1)%day>31)) then
			write(*,*) 'Error in intial time month/day value(s)'
			write (*,*) 'Program will stop'
        	stop  
		end if

		params%timewindow_date(1)%hour = 0_i2b
		params%timewindow_date(1)%minutes = 0_i2b
		params%timewindow_date(1)%seconds = 0.0_r4b            

		params%timewindow_julsec(1) = date_to_j2000_sec(params%timewindow_date(1))

        
    	! check final time 
		if ((params%timewindow_date(2)%month<1).or. &
			(params%timewindow_date(2)%month>12).or.  &
			(params%timewindow_date(2)%day<1).or.  &
			(params%timewindow_date(2)%day>31)) then
			write(*,*) 'Error in final time month/day value(s)'
			write (*,*) 'Program will stop'
        	stop  
		end if

		params%timewindow_date(2)%hour = 0_i2b
		params%timewindow_date(2)%minutes = 0_i2b
		params%timewindow_date(2)%seconds = 0.0_r4b            

		params%timewindow_julsec(2) = date_to_j2000_sec(params%timewindow_date(2)) 


		if (params%timewindow_julsec(2)<=params%timewindow_julsec(1)) then
			write(*,*) 'Error: final time lower or equal to initial time'
			write (*,*) 'Program will stop'
        	stop  
		end if
        

	end if
    

    !! quality filtering
	read (20,*) params%maxallow_SatElevCorr, params%maxallow_Gain  
	if ((params%maxallow_SatElevCorr==-1).and.  &
		(params%maxallow_Gain==-1)) then
		params%data_quality_filtering = .false.
    else
		params%data_quality_filtering = .true.     
	end if               

        
    close(unit=20)

	write (*,*)


end subroutine ParamsDef

!------------------------




end module UserInputs

!------------------------------------------------




!------------------------------------------------ 

module InputAnalysis

use UserInputs


implicit none

contains


!-----------------------------
! calculate number of GLA12 file to read

function GLA12FilesNumber() result (GLA12Files_Number)

	integer (kind=i2b) :: GLA12Files_Number
	character (len= 45) :: GLA12FileName

	GLA12Files_Number=0
	do
		read(15, *, iostat=ios) GLA12FileName
		if (ios==0) then
			GLA12Files_Number=GLA12Files_Number+1
		else
  			exit
		end if
	end do

	rewind(unit=15,iostat=ios)
	if (ios/=0) then
		write(*,*) ' error in rewinding the input list of file'
    	read(*,*)
    	stop
	end if

end function GLA12FilesNumber


end module InputAnalysis


!------------------------------------------------





!------------------------------------------------

module InOutputsData


use InputAnalysis


implicit none

! name of input GLA12 files
character (len= 45) :: GLA12FileName

! first and last analysed time_date
real (kind=r8b) :: i_UTCTime_sec_MIN, i_UTCTime_sec_MAX


contains


!-----------------------------
! initialization of minimum and maximum analysed time_date

subroutine MinMaxTime_Initialization()

	i_UTCTime_sec_MAX = 0
	i_UTCTime_sec_MIN = huge(i_UTCTime_sec_MAX)

    
end subroutine MinMaxTime_Initialization

!-----------------------------



!-----------------------------
! processing of input GLA12 file 

subroutine process_GLA12file (GLA12FileName, RecNum, params, numRecsUsedGLA12File)

implicit none

character (len=45), intent(in) :: GLA12FileName
integer (kind=i4b), intent(inout) :: RecNum, numRecsUsedGLA12File
type(analysis_param)  :: params

type(analysis_param) :: FilteringParameters 

					
integer (kind=i2b) :: ios_inpt, n

integer (kind=i4b) :: m


!------------------
! variable initialization

type (GLA12_prod_input) :: GLA12_prod_in !! GLA12 product vars
    

!------------------
! create sequential-access output file (for import in GIS)

open (unit=19,file=trim(GLA12FileName)//'_gis.txt',status='NEW' &
, access='sequential', form='formatted', iostat=ios)    

if (ios /= 0) then
	print*, 'Unable to create output GIS file. Program will stop'
	stop
end if  

! writes header of GIS-read file
call GISReadFileHeader()

    
!------------------
! read and process records in GLA12 input file
open (unit=16,file=GLA12FileName,status='OLD',access='direct', &
      recl=6600,iostat=ios_inpt)

! local initialisations
n = 0
m = 0
numRecsUsedGLA12File = 0

! read cycle
do  
  	n = n + 1 
	read(16,rec=n,iostat=ios_inpt) GLA12_prod_in
	if (ios_inpt==0) then
		call process_GLA12record(params, GLA12FileName, GLA12_prod_in, RecNum, m, numRecsUsedGLA12File)
	else
  		exit
	end if
end do


if (numRecsUsedGLA12File > 0) then
	close(unit=19, STATUS='keep')
else    
	close(unit=19, STATUS='delete')
end if   

end subroutine process_GLA12file

!------------------------



!------------------------
! processing of single GLA12 record

subroutine process_GLA12record(params, GLA12FileName, GLA12_prod_in, RecNum, m, numRecsUsedGLA12File)	

! variable declarations
character (len=45), intent(in) :: GLA12FileName
type(analysis_param), intent(in) :: params
integer (kind=i4b), intent(inout) :: RecNum, numRecsUsedGLA12File, m 
type (GLA12_prod_input) :: GLA12_prod_in
type (GLA12_prod_output) :: GLA12_prod_out

real (kind=r8b) :: conv_long
    
character (len=45) :: GLA12file_R


integer (kind=i1b) :: j_BElvuseFlg, j_bitElvuseFlg, ElvuseFlg_Byte
integer (kind=i1b), dimension(40) ::  ShotElvuseFlg

integer (kind=i2b) :: i, ndx_rec_EUF 

integer (kind=i4b), save :: cod_prof


real (kind=r4b)	:: 	i_erElv_1_R, i_erElv_2_R, i_erElv_delta_R &
					, i_gdHt_1_R, i_gdHt_2_R, i_gdHt_delta_R

type(date) :: shot_time

! logical variable that defines whether to print out shot data
logical :: ShotDataPrintOut


! declarations of time_date variables 
integer (kind=i4b), save :: i_UTCTime_previous 
real (kind=r8b) :: i_UTCTime_sec_R, i_UTCTime_sec(40)

 
!----------------
! initialize variables
GLA12file_R = GLA12FileName

! time_date of first shot in record
i_UTCTime_sec_R = GLA12_prod_in%i_UTCTime(1) + (GLA12_prod_in%i_UTCTime(2)/div_D06)

! START of elevation use flag processing
do j_BElvuseFlg = 5,1,-1

	ElvuseFlg_Byte = GLA12_prod_in%i_ElvuseFlg(j_BElvuseFlg)

	do j_bitElvuseFlg = 0,7

		ndx_rec_EUF = (5-j_BElvuseFlg)*8 + (j_bitElvuseFlg+1)
		ShotElvuseFlg(ndx_rec_EUF) = ibits(ElvuseFlg_Byte,j_bitElvuseFlg,1)

	end do

end do
! END of elevation use flag processing

    
! START of definition of cod_prof - time_date difference with previous record greater than 100 [seconds]
if (m==1) then
  cod_prof = 1
  i_UTCTime_previous = GLA12_prod_in%i_UTCTime(1)
end if

if (GLA12_prod_in%i_UTCTime(1)>(i_UTCTime_previous + 100)) then
  cod_prof = cod_prof + 1
end if

i_UTCTime_previous = GLA12_prod_in%i_UTCTime(1)
! END of definition of cod_prof


! START of processing of geoid height
if ((GLA12_prod_in%i_gdHt(1) /= gi_invalid_i2b) .and. &
    (GLA12_prod_in%i_gdHt(2) /= gi_invalid_i2b)) then
		i_gdHt_1_R = GLA12_prod_in%i_gdHt(1)/div_D02
		i_gdHt_2_R = GLA12_prod_in%i_gdHt(2)/div_D02
		i_gdHt_delta_R = (i_gdHt_2_R - i_gdHt_1_R)/39.0  
end if
! END of processing of geoid height


! START of Solid Earth Tide Elevation (at first & last shot) processing
if ((GLA12_prod_in%i_erElv(1) /= gi_invalid_i2b) .and. &
    (GLA12_prod_in%i_erElv(2) /= gi_invalid_i2b)) then

		i_erElv_1_R = GLA12_prod_in%i_erElv(1)/div_D03
		i_erElv_2_R = GLA12_prod_in%i_erElv(2)/div_D03
		i_erElv_delta_R = (i_erElv_2_R - i_erElv_1_R)/39.0
end if
! END of Solid Earth Tide Elevation (at first & last shot) processing


! START  shot-by-shot reading of record data
ShotDataPrintOut = .false.

do i=1,40

  	m = m + 1

    ! START OF RECORD FILTERING
    
    	! invalid latitude, longitude or elevation values
    if ((GLA12_prod_in%i_lat(i) == gi_invalid_i4b) .or. &
        (GLA12_prod_in%i_lon(i) == gi_invalid_i4b) .or. &
        (GLA12_prod_in%i_elev(i) == gi_invalid_i4b)) then 
			cycle	
    end if 
    
  	! geographic filtering
    ! latitude
    if ((GLA12_prod_in%i_lat(i)/div_D06 < params%lat_min) .or. &
        (GLA12_prod_in%i_lat(i)/div_D06 > params%lat_max)) then 
			cycle	
    end if       
    ! longitude
    ! longitude conversion
    conv_long = GLA12_prod_in%i_lon(i)/div_D06
    if  (conv_long > 180.0) then
      conv_long = conv_long - 360.0
    end if
        
    if ((conv_long < params%long_min) .or. &
        (conv_long > params%long_max)) then 
			cycle	
    end if    
  	! END geographic filtering/processing

     ! julian time processing    
	if (i==1) then
		i_UTCTime_sec(i) = i_UTCTime_sec_R
	else
		i_UTCTime_sec(i) = i_UTCTime_sec_R + (GLA12_prod_in%i_dShotTime(i-1)/div_D06)
    end if    
	! time filtering
	if (params%temporal_filtering) then
		if ((i_UTCTime_sec(i) < params%timewindow_julsec(1)).or.  &
  		(i_UTCTime_sec(i) > params%timewindow_julsec(2))) then
    		cycle
		end if
	end if
     ! time date processing    
	shot_time = date_from_j20002(i_UTCTime_sec(i))     
	! END of time filtering

    ! START quality data filtering/processing
	if (params%data_quality_filtering) then    
   		! gain
        if (params%maxallow_Gain /= -1) then
    		if ((GLA12_prod_in%i_gval_rcv(i) == gi_invalid_i2b) .or. &
			(GLA12_prod_in%i_gval_rcv(i) > params%maxallow_Gain)) then 
				cycle	
    		end if 
        end if
		! saturation elevation correction
        if (params%maxallow_SatElevCorr /= -1) then
    		if ((GLA12_prod_in%i_satElevCorr(i) == gi_invalid_i2b) .or. &
          	(GLA12_prod_in%i_satElevCorr(i) > params%maxallow_SatElevCorr)) then 
				cycle	
    		end if 
        end if
	end if !(params%data_quality_filtering)

    
    ! END quality data filtering/processing
    ! END OF RECORD FILTERING/PROCESSING

	!!
    ! ! IF WE ARE HERE, SHOT DATA WILL BE PRINT OUT
    !!
    
    ! setting of variable for min and max time_date
    ShotDataPrintOut = .true.    

	! setting of RecNum, the progressive counter of filtered-in records from all GLA12 file
    RecNum = RecNum + 1
    
    ! setting of the number of filtered-in records from the current GLA12 file
    numRecsUsedGLA12File = numRecsUsedGLA12File +1

	! definition of shot parameters 
	GLA12_prod_out%id_shot = RecNum
	GLA12_prod_out%GLA12sourcefile = GLA12file_R    
	GLA12_prod_out%id_track = cod_prof
	GLA12_prod_out%rec_ndx = GLA12_prod_in%i_rec_ndx
	GLA12_prod_out%UTCTime_julsec = i_UTCTime_sec(i)		!time_date in Julian seconds 
	GLA12_prod_out%time_shot = shot_time   
	GLA12_prod_out%shot_loc%lat_degr = GLA12_prod_in%i_lat(i)/div_D06
	GLA12_prod_out%shot_loc%lon_degr = GLA12_prod_in%i_lon(i)/div_D06
    if  (GLA12_prod_out%shot_loc%lon_degr > 180.0) then
      GLA12_prod_out%shot_loc%lon_degr = GLA12_prod_out%shot_loc%lon_degr - 360.0
    end if
	GLA12_prod_out%ElvuseFlg = ShotElvuseFlg(i)
	GLA12_prod_out%elev = GLA12_prod_in%i_elev(i)/div_D03

    ! TEST su corretto processing di Saturation Elevation Correction
	if (mod(RecNum,10) == 0) then
		GLA12_prod_in%i_satElevCorr(i) = gi_invalid_i2b
    end if
    
    ! Saturation Elevation Correction   
    if (GLA12_prod_in%i_satElevCorr(i) /= gi_invalid_i2b) then     
    	GLA12_prod_out%satElevCorr = GLA12_prod_in%i_satElevCorr(i)/div_D03 
	else
    	GLA12_prod_out%satElevCorr = gi_invalid_r4b     
	end if
    ! Geoid
    if ((GLA12_prod_in%i_gdHt(1) /= gi_invalid_i2b) .and. &   
     (GLA12_prod_in%i_gdHt(2) /= gi_invalid_i2b)) then
    	GLA12_prod_out%gdHt = i_gdHt_1_R + (i-1)*i_gdHt_delta_R 
    else
    	GLA12_prod_out%gdHt = gi_invalid_r4b 
    end if
	! Solid earth tide elevation (linearly interpolated between first and last shot)
	if ((GLA12_prod_in%i_erElv(1) /= gi_invalid_i2b) .and. &
      (GLA12_prod_in%i_erElv(2) /= gi_invalid_i2b)) then
    	GLA12_prod_out%erElv = i_erElv_1_R + (i-1)*i_erElv_delta_R 
    else
    	GLA12_prod_out%erElv = gi_invalid_r4b 
	end if
	! Gain Value used for Received Pulse
    if (GLA12_prod_in%i_gval_rcv(i) /= gi_invalid_i2b) then 
    	GLA12_prod_out%gval_rcv = GLA12_prod_in%i_gval_rcv(i)    
    else
    	GLA12_prod_out%gval_rcv = gi_invalid_i2b         
    end if  
   ! Attitude quality indicator
    if (GLA12_prod_in%i_sigmaatt(i) /= gi_invalid_i2b) then
    	GLA12_prod_out%sigmaatt = GLA12_prod_in%i_sigmaatt(i) 
    else
    	GLA12_prod_out%sigmaatt = gi_invalid_i2b 
    end if	
    GLA12_prod_out%RecNrgAll = GLA12_prod_in%i_RecNrgAll(i)/div_D02  ! Received Energy signal begin to signal end 
    GLA12_prod_out%numPk = GLA12_prod_in%i_numPk(i) ! number of peaks found in the return 
    ! Kurtosis of the received echo (standard)
    if (GLA12_prod_in%i_kurt2(i) /= gi_invalid_i2b) then 
    	GLA12_prod_out%kurt2 = GLA12_prod_in%i_kurt2(i)/div_D02 
	else
    	GLA12_prod_out%kurt2 = gi_invalid_r4b 
    end if    
	! Skewness
    if (GLA12_prod_in%i_skew2(i) /= gi_invalid_i2b) then
    	GLA12_prod_out%skew2 = GLA12_prod_in%i_skew2(i)/div_D02 
	else
    	GLA12_prod_out%skew2 = gi_invalid_r4b 
    end if
    ! Standard deviation of the ice sheet Gaussian fit
    if (GLA12_prod_in%i_IceSVar(i) /= gi_invalid_i2b) then
    	GLA12_prod_out%IceSVar  = GLA12_prod_in%i_IceSVar(i)
    else
    	GLA12_prod_out%IceSVar = gi_invalid_i2b 
    end if
	! Saturation Index
    if (GLA12_prod_in%i_satNdx(i) /= gi_invalid_i1b) then 
    	GLA12_prod_out%satNdx = GLA12_prod_in%i_satNdx(i) 
	else
    	GLA12_prod_out%satNdx = gi_invalid_i1b     
    end if
	! Percent Saturation
    if (GLA12_prod_in%i_pctSAT(i) /= gi_invalid_i1b) then
    	GLA12_prod_out%pctSAT = GLA12_prod_in%i_pctSAT(i) 
	else
    	GLA12_prod_out%pctSAT = gi_invalid_i1b     
    end if


    call printout_shotdata(GLA12_prod_out, RecNum)

	! time parameters update
	if (ShotDataPrintOut) then
		i_UTCTime_sec_MIN = min(i_UTCTime_sec(i), i_UTCTime_sec_MIN)
		i_UTCTime_sec_MAX = max(i_UTCTime_sec(i), i_UTCTime_sec_MAX)
	end if           

end do
! END shot-by-shot reading of record data  


end subroutine process_GLA12record

!------------------------


!------------------------
! write program header on screen

subroutine OutputScreen_01()


write (*,"(A)") '*****************************************'
write (*,*)
write (*,"(16X,A)") 'readGLA12'
write (*,*)
write (*,"(16X,A)")	'vers. 6.0'
write (*,"(16X,A)") '2016-04-30'
write (*,*)
write (*,"(5X,A)") 'program for reading GLA12 files'
write (*,*) 
write (*,"(A)") '*****************************************'

write (*,*)
write (*,*)
write (*,"(4X,A)") 'Input parameters definition'
write (*,*)

end subroutine OutputScreen_01

!-----------------------------


!-----------------------------
! write metadata on screen 

subroutine OutputScreen_02(GLA12Files_Number)

integer (i2b) :: GLA12Files_Number
write (*,*)
write (*,*) '--------------------------------'
write (*,*)
write (*,"(A,I4)") 'Number of input GLA12 files: ',GLA12Files_Number 
write (*,*)
write (*,*) 'Record reading and filtering ..... '
write (*,*)

end subroutine OutputScreen_02

!-----------------------------


!-----------------------------
! writes header of  output GIS-read file

subroutine GISReadFileHeader()

	write(19,*) 'id_shot',',','GLA12FileName',',','id_track',',','rec_ndx',',','UTCTime_sec',',','year'  &
    		,',', 'month',',', 'day',',', 'hour',',', 'minutes',',','sec',',','lat'  &
            ,',','lon',',','ShotElvuseFlg',',','elev',',','satElevCorr',',','gdHt'  &
            ,',','erElv',',','gval_rcv',',','sigmaatt',',','RecNrgAll',',','numPk'  &
            ,',','kurt2',',','skew2',',','IceSVar',',','satNdx',',','pctSAT'

end subroutine GISReadFileHeader

!-----------------------------


!-----------------------------
! write shot parameters in output files

subroutine printout_shotdata(GLA12_prod_out, RecNum)

	integer (kind=i4b), intent(inout) :: RecNum
	integer :: ios_inpt
	type(GLA12_prod_output), intent(in) :: GLA12_prod_out
	character (len=226) :: GISOutputString
	character, parameter  :: SeparatorString = ","


	! OUTPUT for ANALYSIS FILE	
	! output writing operations
	write(17,rec=RecNum,iostat=ios_inpt) GLA12_prod_out

    
	! OUTPUT for GIS IMPORT FILE
    ! shot record number
    write(GISOutputString,"(I10,A)") GLA12_prod_out%id_shot,SeparatorString
    !GLA12 file name
    write(GISOutputString,"(3A)") trim(GISOutputString),GLA12_prod_out%GLA12sourcefile,SeparatorString    
    !id_track, the numeric identifier for track
    write(GISOutputString,"(A,I6,A)") trim(GISOutputString),GLA12_prod_out%id_track,SeparatorString
    !GLA12 record index
    write(GISOutputString,"(A,I14,A)") trim(GISOutputString),GLA12_prod_out%rec_ndx,SeparatorString    
	!time_date in Julian seconds
    write(GISOutputString,"(A,F14.3,A)") trim(GISOutputString),GLA12_prod_out%UTCTime_julsec,SeparatorString 
	! year
    write(GISOutputString,"(A,I4,A)") trim(GISOutputString),GLA12_prod_out%time_shot%year,SeparatorString 
    ! month
    write(GISOutputString,"(A,I2,A)") trim(GISOutputString),GLA12_prod_out%time_shot%month,SeparatorString 
    ! day
    write(GISOutputString,"(A,I2,A)") trim(GISOutputString),GLA12_prod_out%time_shot%day,SeparatorString 
    ! hour
    write(GISOutputString,"(A,I2,A)") trim(GISOutputString),GLA12_prod_out%time_shot%hour,SeparatorString 
	! minutes
    write(GISOutputString,"(A,I2,A)") trim(GISOutputString),GLA12_prod_out%time_shot%minutes,SeparatorString 
	! seconds
    write(GISOutputString,"(A,F7.4,A)") trim(GISOutputString),GLA12_prod_out%time_shot%seconds,SeparatorString 
	! latitude
    write(GISOutputString,"(A,F10.6,A)") trim(GISOutputString),GLA12_prod_out%shot_loc%lat_degr,SeparatorString 
	! longitude
    write(GISOutputString,"(A,F11.6,A)") trim(GISOutputString),GLA12_prod_out%shot_loc%lon_degr,SeparatorString 
	! elevation use flag: 0 - valid elevation; 1 - invalid elevation
    write(GISOutputString,"(A,I1,A)") trim(GISOutputString),GLA12_prod_out%ElvuseFlg,SeparatorString 
	! elevation (meters)
    write(GISOutputString,"(A,F9.3,A)") trim(GISOutputString),GLA12_prod_out%elev,SeparatorString 
    ! saturation elevation correction
    if (GLA12_prod_out%satElevCorr < gi_invalid_r4b) then 
    	write(GISOutputString,"(A,F5.2,A)") trim(GISOutputString),GLA12_prod_out%satElevCorr,SeparatorString 
	else
    	write(GISOutputString,"(A,A)") trim(GISOutputString),SeparatorString     
	end if
    ! geoid
    if (GLA12_prod_out%gdHt < gi_invalid_r4b) then
    	write(GISOutputString,"(A,F7.2,A)") trim(GISOutputString),GLA12_prod_out%gdHt,SeparatorString 
    else
    	write(GISOutputString,"(A,A)") trim(GISOutputString),SeparatorString 
    end if
	! solid earth tide elevation (linearly interpolated between first and last shot)
	if (GLA12_prod_out%erElv < gi_invalid_r4b) then
    	write(GISOutputString,"(A,F7.3,A)") trim(GISOutputString),GLA12_prod_out%erElv,SeparatorString 
    else
    	write(GISOutputString,"(A,A)") trim(GISOutputString),SeparatorString 
	end if
	! gain value used for received pulse
    if (GLA12_prod_out%gval_rcv /= gi_invalid_i2b) then 
    	write(GISOutputString,"(A,I3,A)") trim(GISOutputString),GLA12_prod_out%gval_rcv,SeparatorString    
    else
    	write(GISOutputString,"(A,A)") trim(GISOutputString),SeparatorString          
    end if     
    ! attitude quality indicator
    if (GLA12_prod_out%sigmaatt /= gi_invalid_i2b) then
    	write(GISOutputString,"(A,I4,A)") trim(GISOutputString),GLA12_prod_out%sigmaatt,SeparatorString 
    else
    	write(GISOutputString,"(A,A)") trim(GISOutputString),SeparatorString 
    end if
	! received energy signal begin to signal end
    write(GISOutputString,"(A,F8.2,A)") trim(GISOutputString),GLA12_prod_out%RecNrgAll,SeparatorString 
	! number of peaks found in the return
    write(GISOutputString,"(A,I1,A)") trim(GISOutputString),GLA12_prod_out%numPk,SeparatorString 
    ! kurtosis of the received echo (standard)
    if (GLA12_prod_out%kurt2 < gi_invalid_r4b) then 
    	write(GISOutputString,"(A,F6.2,A)") trim(GISOutputString),GLA12_prod_out%kurt2,SeparatorString 
	else
    	write(GISOutputString,"(A,A)") trim(GISOutputString),SeparatorString 
    end if
	! skewness
    if (GLA12_prod_out%skew2 < gi_invalid_r4b) then
    	write(GISOutputString,"(A,F7.2,A)") trim(GISOutputString),GLA12_prod_out%skew2,SeparatorString 
	else
    	write(GISOutputString,"(A,A)") trim(GISOutputString),SeparatorString 
    end if
    ! standard deviation of the ice sheet gaussian fit
    if (GLA12_prod_out%IceSVar /= gi_invalid_i2b) then
		write(GISOutputString,"(A,I5,A)") trim(GISOutputString),GLA12_prod_out%IceSVar,SeparatorString 
    else
		write(GISOutputString,"(A,A)") trim(GISOutputString),SeparatorString 
    end if
	! saturation Index
    if (GLA12_prod_out%satNdx /= gi_invalid_i1b) then 
    	write(GISOutputString,"(A,I3,A)") trim(GISOutputString),GLA12_prod_out%satNdx,SeparatorString 
	else
    	write(GISOutputString,"(A,A)") trim(GISOutputString),SeparatorString     
    end if
	! percent Saturation
    if (GLA12_prod_out%pctSAT /= gi_invalid_i1b) then
      write(GISOutputString,"(A,I4)") trim(GISOutputString),GLA12_prod_out%pctSAT 
	else
      write(GISOutputString,"(A)") trim(GISOutputString)     
    end if
    
 	write(19, "(A)") trim(GISOutputString)  


end subroutine printout_shotdata

!-----------------------------


!-----------------------------
! write time parameters in metadata file

subroutine MetaDataOutput_01(time_values_initial)

integer, dimension(8) :: time_values_initial

write (18,*) 'Metadata of readGLA12 session'
write (18,"(A,2X,I4,A,I2,A,I2)") 'analysis of: ',time_values_initial(1),'/',time_values_initial(2),'/',time_values_initial(3)
write (18,*)

end subroutine MetaDataOutput_01

!-----------------------------


!-----------------------------
! write filtering parameters in metadata file

subroutine MetaDataOutput_02(params)

type(analysis_param), intent(in) :: params

! Geographical boundaries values
write (18,*) 'Geographical boundaries:'
write (18,"(19X,A,15X,A)") 'min','max'
write (18,"(A,8X,F8.4,10X,F8.4)") 'Latitude:',params%lat_min,params%lat_max
write (18,"(A,6X,F9.4,9X,F9.4)") 'Longitude:',params%long_min,params%long_max
write (18,*)

! Time window values
write (18,*) 'Time window:'
if (.not.params%temporal_filtering) then
	write (18,*) ' none'
else if (params%temporal_filtering) then
  	write(18,"(23X,A,9X,A)") 'date','julian 2000 (sec)'           
	write (18,"(A,7X,I4,2(A,I2),5X,F14.2)") 'Initial time:'  &
    ,params%timewindow_date(1)%year,'/',params%timewindow_date(1)%month  &
    ,'/',params%timewindow_date(1)%day,params%timewindow_julsec(1)
	write (18,"(A,9X,I4,2(A,I2),5X,F14.2)") 'Final time:'  &
    ,params%timewindow_date(2)%year,'/',params%timewindow_date(2)%month  &
    ,'/',params%timewindow_date(2)%day,params%timewindow_julsec(2)
	write (18,*)
else
	write (18,*) '   Undefined parameters, beware of possible output errors!'
end if
write (18,*) 

! Data quality filtering values
write (18,*) 'Data quality filtering:'
if (.not.params%data_quality_filtering) then
	write (18,*) ' none'
else if (params%data_quality_filtering) then
	if (params%maxallow_SatElevCorr==-1) then
		write (18,*) '   No saturation elevation correction filter'
    else     
		write (18,*) '   Saturation elevation correction maximum allowed value: ',params%maxallow_SatElevCorr,'[mm]'
    end if
	if (params%maxallow_Gain==-1) then    
		write (18,*) '   No gain filter'
    else     
		write (18,*) '   Gain maximum allowed value: ',params%maxallow_Gain
    end if    
else
	write (18,*) '   Undefined parameters, beware of possible output errors!'
end if
write (18,*)


end subroutine MetaDataOutput_02

!-----------------------------


!-----------------------------
! write source files parameters in metadata file 

subroutine MetaDataOutput_03(GLA12Files_Number)


integer (i2b) :: GLA12Files_Number
write (18,*)
write (18,*) 'Source list file: ', InputFilesListName
write (18,*)
write (18,*) 'Number of input GLA12 files: ',GLA12Files_Number
write (18,*)
write (18,*) 'List of analysed files'
write (18,*)
write (18,"(12X,A,22X,A)") 'GLA12FileName','number_recs'

end subroutine MetaDataOutput_03

!-----------------------------


!-----------------------------
! write number and time of filtered data in metadata file 

subroutine MetaDataOutput_04(RecNum)

integer (kind=i4b) :: RecNum
type(date) :: shots_time_boundary(2)

write (18,*)
write (18,"(A,I8)") 'Total number of filtered-in data:', RecNum


if (RecNum > 0) then

	shots_time_boundary(1)= date_from_j20002(i_UTCTime_sec_MIN) 
	shots_time_boundary(2)= date_from_j20002(i_UTCTime_sec_MAX) 

	write (18,*)
	write (18,"(A,2X,I4,4(A,I2),A,F5.2)") 'Start time of analysed data:'  &
			,shots_time_boundary(1)%year,'/',shots_time_boundary(1)%month  &
            ,'/',shots_time_boundary(1)%day,' ',shots_time_boundary(1)%hour  &
            ,':',shots_time_boundary(1)%minutes,':',shots_time_boundary(1)%seconds
	write (18,"(A,4X,I4,4(A,I2),A,F5.2)") 'End time of analysed data:'  &
			,shots_time_boundary(2)%year,'/',shots_time_boundary(2)%month  &
            ,'/',shots_time_boundary(2)%day,' ',shots_time_boundary(2)%hour  &
            ,':',shots_time_boundary(2)%minutes,':',shots_time_boundary(2)%seconds

end if

end subroutine MetaDataOutput_04

!-----------------------------


!-----------------------------
! writes analysis start time in metadata file 

subroutine MetaDataOutput_05(time_values_initial)

integer, dimension(8) :: time_values_initial

write (18,*) 
write (18,*) '------------------------------------------------'
write (18,*) 
write (18,"(A,2X,I4,A,I2,A,I2)") 'analysis of:',time_values_initial(1)  &
				,'-',time_values_initial(2),'-',time_values_initial(3)
write (18,"(A,4X,I2,A,I2,A,I2)") 'starts at:',time_values_initial(5)    &
				,':',time_values_initial(6),':',time_values_initial(7)


end subroutine MetaDataOutput_05

!-----------------------------


!-----------------------------
! writes analysis end time in metadata file

subroutine MetaDataOutput_06(time_values_final)

	integer, dimension(8) :: time_values_final

	write (18,"(A,6X,I2,A,I2,A,I2)") 'ends at:'   &
    	,time_values_final(5),':',time_values_final(6),':',time_values_final(7)
	write (18,*) 
	write (18,*) '------------------------------------------------'

end subroutine MetaDataOutput_06

!-----------------------------



end module InOutputsData


!------------------------------------------------ 




!------------------------------------------------
!------------------------------------------------

program readGLA12

use InOutputsData

implicit none

integer (kind=i2b) :: n, GLA12Files_Number
integer, dimension(8) :: time_values_initial, time_values_final  ! analysis date/time information
integer (kind=i4b) :: RecNum, numRecsUsedGLA12File


type(analysis_param) :: params


!-------------------------------
! get initial time_date of analysis 
call DATE_AND_TIME(VALUES=time_values_initial)


!-----------------------------
! write program header to screen
call OutputScreen_01()

 
!-----------------------------
! define input and output files
call InputFileDef()
call OutputFilesDef()


!-----------------------------
! define filtering parameters
call ParamsDef(params)


!------------------------------
! define file number to read in input file
GLA12Files_Number = GLA12FilesNumber()


!--------------------
! write to screen initial information on processing
call OutputScreen_02(GLA12Files_Number)


!------------------------------
! write introductive information in output files

! header and time_date information
call MetaDataOutput_01(time_values_initial)
! filtering parameters
call MetaDataOutput_02(params)
! number of input GLA12 files
call MetaDataOutput_03(GLA12Files_Number)



!-----------------------------   
! data reading and writing

call MinMaxTime_Initialization()

RecNum = 0

write (*,"(3X,A,24X,A)") 'Analysing file','% done' 
do n=1,GLA12Files_Number
  
	read(15, *, iostat=ios) GLA12FileName
	! write in metadata file and to screen
	write (*,"(A,4X,I3)") GLA12FileName,(n-1)*100/GLA12Files_Number    
	call process_GLA12file(GLA12FileName, RecNum, params, numRecsUsedGLA12File)
	write (18,"(A,4X,I8)") GLA12FileName, numRecsUsedGLA12File

end do

!------------------------------
! write final information into metadata file

! total number of analysed data
! and minimum and maximum time_date of data
call MetaDataOutput_04(RecNum) 

! start time_date information
call MetaDataOutput_05(time_values_initial)

! end time_date information
call DATE_AND_TIME(VALUES=time_values_final)
call MetaDataOutput_06(time_values_final)



!----------------------------- 
! program closing  

close(unit=18)

if (RecNum > 0) then
	close(unit=17, STATUS='keep')
else    
	close(unit=17, STATUS='delete')
    write (*,*)
	write (*,*) 'Sorry, no found data within defined ranges'
    write (*,*) 'press any key to end program'
    read (*,*)
end if

write (*,*) 'Finished'
    
end program readGLA12
