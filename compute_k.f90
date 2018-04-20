program compute_k

    !============================================
    !
    !          -- compute_k -- 
    !
    !============================================

    !=================================================
    ! MODULE LOAD
    !=================================================
    use xdr, only: trrfile

    !=================================================
    ! VARIABLE DEFINITION
    !=================================================
    implicit none

    ! Constants
    real(8),parameter :: AMUtoKG   = 1.66053873d-27,  &
                         KJmoltocm1= 83.593d0,        &
                         Nav       = 6.02214d23 
    real(8)           :: Factor
    
    ! System vars
    integer                            :: Nat, Nselec=-1
    real(8),dimension(:),allocatable   :: Mass, X,Y,Z
    real(8)                            :: Mtot, X0,Y0,Z0 ! COM
    type(trrfile)                      :: traj
    real(8),dimension(:),allocatable   :: E 
    
    !IO
    character(len=150)                 :: trjfile = 'system.trr', &
                                          massfile= 'masses.dat', &
                                          outfile = 'system_tat.dat'
    integer :: I_TRJ = 10, &
               I_MASS= 11, &
               O_OUT = 20
    integer :: IOStatus

    ! Counters
    integer :: i

    !=================================================
    !
    ! INITZILIZATION OF VARIABLES
    !
    !=================================================
    ! Conversion factor (gromacs units -> cm-1)
    Factor = 1.d3 * AMUtoKg * NAv * KJmoltocm1

    !=================================================
    !
    ! READ GENERAL INPUT
    !
    !=================================================
    ! Parse command line 
    call command_line_options(trjfile,massfile,Nselec,outfile)
    
    !=======================================
    !
    ! DATA FILES 
    !
    !=======================================
    ! Check files
    i=10
    open(i,file=trjfile,status='old',iostat=IOStatus)
    if (IOStatus /= 0) call alert_msg('fatal','Cannot open trjfile: '//trim(trjfile))
    close(i)
    open(i,file=massfile,status='old',iostat=IOStatus)
    if (IOStatus /= 0) call alert_msg('fatal','Cannot open massfile: '//trim(massfile))
    close(i)

    !----------------------
    ! Analyze trajectory
    !----------------------
    ! Open traj to read
    call traj%init(trjfile)
    ! Read first step (to get Nat)
    call traj%read()
    Nat = traj%natoms

    if (Nselec>Nat) then
        call alert_msg('fatal','Nselec atoms not consistent with Nat')
    endif
    ! Default (all atoms)
    if ( Nselec<0 ) Nselec=Nat

    ! Allocation
    allocate(E(Nselec),X(Nselec),Y(Nselec),Z(Nselec),Mass(Nselec))
    ! Output file
    open(O_OUT,file=outfile,status='replace')
    ! And read masses this first time
    open(I_MASS,file=massfile,status='old')
    do i=1,Nselec
        read(I_MASS,*) Mass(i)
    enddo


    do while ( traj%STAT == 0 )
        ! Get positions and velocities
        do i=1, Nselec
            X(i) = traj%vel(1,i) ! nm/ps
            Y(i) = traj%vel(2,i) ! nm/ps
            Z(i) = traj%vel(3,i) ! nm/ps
        enddo
        ! Compute Kinentic Energy per atom
        do i=1, Nselec
            E(i) = 0.5d0 * (X(i)**2+Y(i)**2+Z(i)**2) * Mass(i) * Factor
        enddo
        write(O_OUT,'(1000(F8.2,X))') E(1:Nselec)
        
        ! Read next frame
        call traj%read()
    enddo
    call traj%close()
    close(O_OUT)
                            
    stop
    
    contains

    subroutine alert_msg(attype,SENTENCE)

        implicit none

        character(len=*),intent(in):: attype, SENTENCE
        
        ! Local valrs
        integer :: line_length = 80, &
                   sentence_length
        integer :: i,ki,kf,nsplit
        integer :: n_notes,n_warns
        ! Write to stderr
        integer :: alert_unt=0
        
        sentence_length=len_trim(SENTENCE)
        
        nsplit = sentence_length/line_length
        if (mod(sentence_length,line_length) /= 0) nsplit=nsplit+1
        
        select case (adjustl(attype))
            case ("note")
                kf=min(sentence_length,line_length)
                write(alert_unt,'(/,A,A)') "NOTE: ",SENTENCE(1:kf)
                sentence_length = sentence_length - line_length
                do i=2,nsplit
                    ki=(i-1)*line_length+1
                    kf=min(sentence_length,line_length)+ki-1
                    write(alert_unt,'(6X,A)') SENTENCE(ki:kf)
                    sentence_length = sentence_length - line_length
                enddo
                write(alert_unt,*) ''
                n_notes = n_notes + 1

            !The following is deprecated (maintained for backward compatibility)
            case ("error")
                write(alert_unt,'(/,A,A,/)') "WARNING: ",SENTENCE
                n_warns = n_warns + 1

            case ("warning")
                write(alert_unt,'(/,A,A,/)') "WARNING: ",SENTENCE
                n_warns = n_warns + 1

            case ("fatal")
                write(alert_unt,'(/,A,/)') "============================================"
                write(alert_unt,'(A,A,/)') "FATAL ERROR: ",trim(SENTENCE)
                write(alert_unt,'(A)')     "============================================"
                write(0,'(A,/)') "Exiting with error..."
                stop 1

            case default
                write(0,'(/,A,A,A,/)') attype," ", SENTENCE
        end select

        return

    end subroutine

    subroutine command_line_options(trrfile,massfile,Nselec,outfile)

        ! Args
        character(len=*),intent(inout) :: trrfile
        character(len=*),intent(inout) :: massfile
        integer,intent(inout)          :: Nselec
        character(len=*),intent(inout) :: outfile
        
        ! Local
        logical :: argument_retrieved,  &
                   need_help = .false.
        integer:: i
        character(len=200) :: arg

        argument_retrieved=.false.
        do i=1,iargc()
            if (argument_retrieved) then
                argument_retrieved=.false.
                cycle
            endif
            call getarg(i, arg) 
            select case (adjustl(arg))
                case ("-t")
                    call getarg(i+1, trrfile)
                    argument_retrieved=.true.
                case ("-m")
                    call getarg(i+1, massfile)
                    argument_retrieved=.true.
                case ("-o")
                    call getarg(i+1, outfile)
                    argument_retrieved=.true.
                case ("-nsel")
                    call getarg(i+1, arg)
                    read(arg,*) Nselec
                    argument_retrieved=.true.
        
                case ("-h")
                    need_help=.true.

                case default
                    call alert_msg('fatal','Unknown input command: '//trim(arg))
            end select
        enddo 


       !Print options (to stderr)
        if (need_help) then

       !Print options (to stderr)
        write(6,'(/,A)') '========================================================'
        write(6,'(/,A)') '                ** compute_k **'    
        write(6,'(/,A)') '      Compute kinetic energy along a trajectory (trr)'     
        write(6,'(/,A)') '========================================================'
        write(6,'(/,A)') '-------------------------------------------------------------------'
        write(6,'(A)')   ' Flag         Description                   Value'
        write(6,'(A)')   '-------------------------------------------------------------------'
        write(6,*)       '-t           Trajectory (trr) file         ', trim(adjustl(trrfile))
        write(6,'(X,A,I0)') &
                         '-nsel        Number of selected atoms      ', Nselec
        write(6,*)       '             (from 1 to Nsel)              '
        write(6,*)       '-m           Mass file                     ', trim(adjustl(massfile))
        write(6,*)       '-o           Output file                   ', trim(adjustl(outfile))
        write(6,*)       '-h           Display this help            ',  need_help
        write(6,'(A,/)') '-------------------------------------------------------------------'
        stop

        endif

        return
    end subroutine command_line_options



end program compute_k
