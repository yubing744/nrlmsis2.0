!##############################################################################
! MSIS� (NRL-SOF-014-1) SOFTWARE
!
! MSIS� is a registered trademark of the Government of the United States of 
! America, as represented by the Secretary of the Navy. Unauthorized use of 
! the trademark is prohibited. 
!
! The MSIS� Software (hereinafter Software) is property of the United States 
! Government, as represented by the Secretary of the Navy. Methods performed
! by this software are covered by U.S. Patent Number 10,641,925. The Government
! of the United States of America, as represented by the Secretary of the Navy, 
! herein grants a non-exclusive, non-transferable license to the Software for 
! academic, non-commercial, purposes only. A user of the Software shall not: 
! (i) use the Software for any non-academic, commercial purposes, (ii) make 
! any modification or improvement to the Software, (iii) disseminate the 
! Software or any supporting data to any other person or entity who will use 
! the Software for any non-academic, commercial purposes, or (iv) copy the 
! Software or any documentation related thereto except for (a) distribution 
! among the user�s personal computer systems, archival, or emergency repair 
! purposes, or (b) distribution for non-commercial, academic purposes, without 
! first obtaining the written consent of IP Counsel for the Naval Research 
! Laboratory. 
!
! As the owner of MSIS�, the United States, the United States Department of 
! Defense, and their employees: (1) Disclaim any warranties, express, or 
! implied, including but not limited to any implied warranties of 
! merchantability, fitness for a particular purpose, title or non-infringement, 
! (2) Do not assume any legal liability or responsibility for the accuracy, 
! completeness, or usefulness of the software, (3) Do not represent that use of 
! the software would not infringe privately owned rights, (4) Do not warrant 
! that the software will function uninterrupted, that is error-free or that any 
! errors will be corrected.
!
! BY USING THIS SOFTWARE YOU ARE AGREEING TO THE ABOVE TERMS AND CONDITIONS.  
!##############################################################################

!!! ===========================================================================
!!! NRLMSIS 2.0:
!!! Neutral atmosphere empirical model from the surface to lower exosphere
!!! John Emmert (john.emmert@nrl.navy.mil)
!!! Doug Drob (douglas.drob@nrl.navy.mil)
!!! ===========================================================================
!!!
!!! MSISINIT: Initialization of MSIS parameters, switches, and options.
!
!     PREREQUISITES:
!       MSIS binary parameter file (msis2.0.parm)
!
!     CALLING SEQUENCE:
!       CALL MSISINIT([OPTIONAL ARGUMENTS])
!  
!     OPTIONAL ARGUMENTS:
!       parmpath        File path pointing to the MSIS parameter file.
!                         Default: Null string (current directory)
!       parmfile        Name of MSIS parameter file.
!                         Default: 'msis2.0.parm'
!       iun             File unit number for reading parameter file.
!                         Default: 67
!       switch_gfn      Logical array of 512 swtiches for individual terms. For
!                         advanced users.
!                         Default values: True (all switches on)
!       switch_legacy   Floating point array (1:25) of legacy switches that
!                         control groups of terms:
!                            1 - F10.7
!                            2 - Time independent
!                            3 - Symmetrical annual
!                            4 - Symmetrical semiannual
!                            5 - Asymmetrical annual
!                            6 - Asymmetrical semiannual
!                            7 - Diurnal
!                            8 - Semidiurnal
!                            9 - Geomagnetic activity:
!                                  1.0 = Daily Ap mode
!                                 -1.0 = Storm-time ap mode
!                           10 - All UT/long effects
!                           11 - Longitudinal
!                           12 - UT and mixed UT/long
!                           13 - Mixed Ap/UT/long
!                           14 - Terdiurnal
!                           15-25 - Not used in NRLMSIS 2.0
!                         For all switches:
!                           0.0 = Off
!                           1.0 = On
!                           2.0 = Main effects off, cross terms on
!                         Default values: 1.0
!       lzalt_type      Logical flag for altitude input type:
!                         True = Geodetic altitude (km)
!                         False = Geopotential height (km)
!                         Default: True (Geodetic altitude)
!       lspec_select    Logical array (1:10) flagging which densities to 
!                         calculate.
!                         True = Calculate, False = Do not calculate
!                            1 - Mass density
!                            2 - N2
!                            3 - O2
!                            4 - O
!                            5 - He
!                            6 - H
!                            7 - Ar
!                            8 - N
!                            9 - Anomalous O
!                           10 - Not used in NRLMSIS 2.0
!                         Default values: True
!       lmass_include   Logical array (1:10) flagging which species to include
!                         in mass density calculation. Same ordering as 
!                         lspec_select.
!                         Default values: True
!       lN2_msis00      Logical flag for retrieving NRLMSISE-00 upper
!                         thermospheric N2 variation. See paper for details.
!                           False: Thermospheric N2 determined entirely by
!                             temperature profile and the constant mixing ratio
!                             of N2 in the lower atmosphere. 
!                           True: Upper thermospheric N2 relaxes to NRLMSISE-00
!                             Values.
!                         Default: False
!
!     NOTES:
!       - The switch_legacy optional argument performs the same function as
!         TSELEC(SW) in NRLSMSISE-00, except that switches 15-25 are not used in
!         NRLMSIS 2.0. The change in the switch-setting call is illustrated as
!         follows, where SW is the 25-element array of switches:
!           NRLMSISE-00: CALL TSELEC(SW)
!           NRLMSIS 2.0: call msisinit(switch_legacy=SW)
!
!!! ===========================================================================

!**************************************************************************************************
! MSIS_INIT Module: Contains initialization subroutines, model options, and model parameters
!**************************************************************************************************
module msis_param_export

  use msis_constants, only    : rp, nspec, nl, maxnbf, mbf

  implicit none
  
  !Model flags
  logical       :: initflag = .false.           !Flags whether model has been initialized
  logical       :: haveparmspace = .false.      !Flags whether parameter space has been initialized and allocated
  logical       :: zaltflag = .true.            !true: height input is geometric, false: height input is geopotential
  logical       :: specflag(1:nspec-1) = .true. !Array flagging which species densities are required
  logical       :: massflag(1:nspec-1) = .true. !Array flagging which species should be included in mass density
  logical       :: N2Rflag = .false.            !Flag for retrieving NRLMSISE-00 thermospheric N2 variations
  logical       :: zsfx(0:mbf) = .false.        !Flags zonal mean terms to be modulated by F1 (MSISE-00 legacy multiplier)
  logical       :: tsfx(0:mbf) = .false.        !Flags tide terms to be modulated by F2 (MSISE-00 legacy multiplier)
  logical       :: psfx(0:mbf) = .false.        !Flags SPW terms to be modulated by F3 (MSISE-00 legacy multiplier)
  logical       :: smod(0:nl) = .false.         !Flags which temperature levels get solar flux modulation; loadparmset turns flags on based on parameter values
  logical       :: swg(0:maxnbf-1) = .true.     !Switch array for globe subroutine.
  real(kind=rp) :: masswgt(1:nspec-1)  = 0.0_rp !Weights for calculating mass density
  real(4)       :: swleg(1:25)=1.0, swc(1:25), sav(1:25) !Legacy switch arrays

  ! Model parameter arrays
  type basissubset
    sequence
    character(8)               :: name
    integer                    :: bl,nl
    real(kind=rp), allocatable :: beta(:,:)
    !logical, allocatable       :: active(:,:)
    !integer, allocatable       :: fitb(:,:)
  end type basissubset
  type (basissubset)     :: TN
  type (basissubset)     :: PR
  type (basissubset)     :: N2
  type (basissubset)     :: O2
  type (basissubset)     :: O1
  type (basissubset)     :: HE
  type (basissubset)     :: H1
  type (basissubset)     :: AR
  type (basissubset)     :: N1
  type (basissubset)     :: OA   !Anomalous O
  type (basissubset)     :: NO
  integer                :: nvertparm
  
  ! Reciprocal node difference arrays (constant values needed for B-spline calculations)
  real(kind=rp)          :: etaTN(0:30,2:6) = 0.0_rp
  real(kind=rp)          :: etaO1(0:30,2:6) = 0.0_rp
  real(kind=rp)          :: etaNO(0:30,2:6) = 0.0_rp

  ! C1 constraint terms for O and NO related to the tapered logistic correction
  real(kind=rp)          :: HRfactO1ref, dHRfactO1ref, HRfactNOref, dHRfactNOref

contains

  !==================================================================================================
  ! MSISINIT: Entry point for initializing model and loading parameters
  !==================================================================================================
  subroutine msisexport(parmpath,parmfile,outfile,iun,switch_gfn,switch_legacy, &
                      lzalt_type,lspec_select,lmass_include,lN2_msis00)

    use msis_constants, only : specmass, nspec, maxnbf 

    implicit none

    character(len=*), intent(in), optional    :: parmpath                 !Path to parameter file
    character(len=*), intent(in), optional    :: parmfile                 !Parameter file name
    character(len=*), intent(in), optional    :: outfile                  !Parameter output file name
    integer, intent(in), optional             :: iun                      !File unit number for reading parameter file
    logical, intent(in), optional             :: switch_gfn(0:maxnbf-1)   !Switch array for globe subroutine.
    real(4), intent(in), optional             :: switch_legacy(1:25)      !Legacy switch array
    logical, intent(in), optional             :: lzalt_type               !true: height input is geometric, false: height input is geopotential
    logical, intent(in), optional             :: lspec_select(1:nspec-1)  !Array flagging which species densities are required
    logical, intent(in), optional             :: lmass_include(1:nspec-1) !Array flagging which species should be included in mass density
    logical, intent(in), optional             :: lN2_msis00               !Flag for retrieving NRLMSISE-00 thermospheric N2 variations

    character(len=128)                        :: parmpath1
    character(len=128)                        :: parmfile1
    integer                                   :: iun1

    ! Path to parameter file
    if (present(parmpath)) then
      parmpath1 = parmpath
    else
      parmpath1 = ''
    endif

    ! Parameter file name
    if (present(parmfile)) then
      parmfile1 = parmfile
    else
      parmfile1 = 'msis2.0.parm'
    endif

    ! Initialize model parameter space
    if (.not. haveparmspace) call initparmspace()

    ! Load parameter set
    if (present(iun)) then
      iun1 = iun
    else
      iun1 = 67
    endif

    call exportparmset(trim(parmpath1)//trim(parmfile1),iun1,outfile)

    return

  end subroutine msisexport

  !==================================================================================================
  ! INITPARMSPACE: Initialize and allocate the model parameter space
  !==================================================================================================
  subroutine initparmspace()

    use msis_constants, only : nl, nls, nodesTN, ndO1, nsplO1, nodesO1, nsplNO, ndNO, nodesNO, &
                               zetagamma, Hgamma, zetarefO1, zetarefNO, maxnbf, ctide, cspw

    implicit none

    integer             :: n, m, j, k
    real(kind=rp)       :: gammaterm0

    ! Vertical parameter counter (number of vertical parameters in the parmeter file)
    nvertparm = 0

    ! Model formulation parameter subsets
    call initsubset(TN,0,nl,        maxnbf,'TN')
    call initsubset(PR,0,nl,        maxnbf,'PR')
    call initsubset(N2,0,nls,       maxnbf,'N2')
    call initsubset(O2,0,nls,       maxnbf,'O2')
    call initsubset(O1,0,nls+nsplO1,maxnbf,'O1')
    call initsubset(HE,0,nls,       maxnbf,'HE')
    call initsubset(H1,0,nls,       maxnbf,'H1')
    call initsubset(AR,0,nls,       maxnbf,'AR')
    call initsubset(N1,0,nls,       maxnbf,'N1')
    call initsubset(OA,0,nls,       maxnbf,'OA')
    call initsubset(NO,0,nls+nsplNO,maxnbf,'NO')

    ! Add the surface pressure parameter to the vertical parameter counter
    nvertparm = nvertparm + 1

    ! Set solar flux modulation flags
    zsfx(:) = .false.
    tsfx(:) = .false.
    psfx(:) = .false.
    ! F1, solar flux modulation of the zonal mean asymmetric annual terms
    zsfx(9:10) = .true.    !Pl(1,0) annual terms
    zsfx(13:14) = .true.   !Pl(3,0) annual terms
    zsfx(17:18) = .true.   !Pl(5,0) annual terms
    ! F2, solar sflux modulation of the tides
    tsfx(ctide:cspw-1) = .true.
    ! F3, solar sflux modulation of stationary planetary wave 1
    psfx(cspw:cspw+59) = .true. 

    ! Calculate reciprocal node difference arrays
    do k = 2, 6
      do j = 0, nl
        etaTN(j,k) = 1.0_rp / (nodesTN(j+k-1) - nodesTN(j))
      enddo
    enddo
    do k = 2, 4
      do j = 0, ndO1-k+1
        etaO1(j,k) = 1.0_rp / (nodesO1(j+k-1) - nodesO1(j))
      enddo
      do j = 0, ndNO-k+1
        etaNO(j,k) = 1.0_rp / (nodesNO(j+k-1) - nodesNO(j))
      enddo
    enddo

    ! Calculate C1 constraint terms for O and NO related to the tapered logistic correction
    gammaterm0 = tanh((zetarefO1 - zetagamma)*Hgamma)
    HRfactO1ref = 0.5_rp * (1.0_rp + gammaterm0)
    dHRfactO1ref = (1.0_rp - (zetarefO1 - zetagamma)*(1.0_rp - gammaterm0)*Hgamma) / HRfactO1ref
    gammaterm0 = tanh((zetarefNO - zetagamma)*Hgamma)
    HRfactNOref = 0.5_rp * (1.0_rp + gammaterm0)
    dHRfactNOref = (1.0_rp - (zetarefNO - zetagamma)*(1.0_rp - gammaterm0)*Hgamma) / HRfactNOref

    ! Set parameter space initialization flag
    haveparmspace = .true.

    return

  contains

      !--------------------------------------------------------------------------------------------------
      ! INITSUBSET: Initialize and allocate a parameter subset
      !--------------------------------------------------------------------------------------------------
      subroutine initsubset(subset,bl,nl,maxnbf,name)

        implicit none

        type (basissubset), intent(inout) :: subset
        integer, intent(in)               :: bl
        integer, intent(in)               :: nl
        integer, intent(in)               :: maxnbf
        character(2), intent(in)          :: name

        integer                           :: iz

        ! Allocate and initialize subset structure
        subset%name = name
        subset%bl = bl
        subset%nl = nl
        allocate(subset%beta(0:maxnbf-1,bl:nl))
        !allocate(subset%beta(0:maxnbf-1,bl:nl), &
        !         subset%active(0:maxnbf-1,bl:nl), &
        !         subset%fitb(0:maxnbf-1,bl:nl))
        subset%beta = 0.0_rp
        !subset%active = .false.
        !subset%fitb = 0
        
        ! Increment vertical parameter counter except for pressure
        if (name .ne. 'PR') nvertparm = nvertparm + nl - bl + 1

        return

      end subroutine initsubset

  end subroutine initparmspace

  !==================================================================================================
  ! LOADPARMSET: Read in a parameter file
  !==================================================================================================
  subroutine exportparmset(name,iun,outfile)

    use msis_constants, only      : maxnbf, csfxmod

    implicit none

    character(len=*), intent(in) :: name
    integer, intent(in)          :: iun
    character(len=*), intent(in) :: outfile

    integer                      :: i0, i1
    logical                      :: havefile
    real(8), allocatable         :: parmin(:,:)

    ! Check if file exists
    inquire(file=trim(name),exist=havefile)
    if (havefile) then
       open(unit=iun,file=trim(name),status='old',access='stream',convert='little_endian')
    else
       print *,"MSIS parameter set ",trim(name)," not found. Stopping."
       stop
    endif

    ! Read in parameter values into temporary double-precision array
    allocate(parmin(0:maxnbf-1,0:nvertparm-1))
    read(iun) parmin
    close(iun)

    open(78,file=outfile,status='replace')
    
    ! Transfer parameters to structures
    i0 = 0
    i1 = TN%nl - TN%bl
    TN%beta = parmin(:,i0:i1)
    call printAndSaveArray("TN", TN%beta, 78)

    i0 = i1 + 1
    i1 = i0
    PR%beta(:,0) = parmin(:,i0)
    i0 = i1 + 1
    i1 = i0 + N2%nl - N2%bl
    N2%beta = parmin(:,i0:i1)
    i0 = i1 + 1
    i1 = i0 + O2%nl - O2%bl
    O2%beta = parmin(:,i0:i1)
    i0 = i1 + 1
    i1 = i0 + O1%nl - O1%bl
    O1%beta = parmin(:,i0:i1)
    i0 = i1 + 1
    i1 = i0 + HE%nl - HE%bl
    HE%beta = parmin(:,i0:i1)
    i0 = i1 + 1
    i1 = i0 + H1%nl - H1%bl
    H1%beta = parmin(:,i0:i1)
    i0 = i1 + 1
    i1 = i0 + AR%nl - AR%bl
    AR%beta = parmin(:,i0:i1)
    i0 = i1 + 1
    i1 = i0 + N1%nl - N1%bl
    N1%beta = parmin(:,i0:i1)
    i0 = i1 + 1
    i1 = i0 + OA%nl - OA%bl
    OA%beta = parmin(:,i0:i1)
    i0 = i1 + 1
    i1 = i0 + NO%nl - NO%bl
    NO%beta = parmin(:,i0:i1)

    !Set solar flux modulation flags; if on for a given vertical parameter, then sfluxmod is called by tfnparm
    smod(:) = .false.
    where((Tn%beta(csfxmod+0,:) .ne. 0) .or. &
          (Tn%beta(csfxmod+1,:) .ne. 0) .or. &
          (Tn%beta(csfxmod+2,:) .ne. 0)) smod = .true.

    close(78)

    return

  end subroutine exportparmset


  subroutine printAndSaveArray(name, a, oun)
    implicit none

    character(len=*), intent(in) :: name
    real(4), dimension (:,:) :: a
    integer, intent(in)          :: oun
    integer::i,j,s1,s2
    integer, dimension (:) ::s

    s = shape(a)
    s1 = s(0)
    s2 = s(1)

    Print *, name
    write(oun, *) name

    do i = 1, s1           
      do j = 1, s2                
        Print *, a(i,j)
        write(oun, *) a(i,j)        
      end do      
    end do

  end subroutine printAndSaveArray

end module msis_param_export
