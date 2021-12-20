module ui
    use sailor_type
    use position_type
    implicit none

    logical :: clear_terminal

    contains

    integer function launch() result(out)
        implicit none
        integer :: usrin
        call initialize()
        call welcome()
        do while (out == 0)     ! Launch user interface loop
            usrin = get_input()
            out = handle_input(usrin)
        enddo
    end function

    integer function handle_input(usrin) result(out)
        implicit none
        integer, intent(in) :: usrin
        integer,parameter :: known_commands(4)= (/1,2,3,4/)

        out = 0

        select case (usrin) ! handle input command
            case ( known_commands(1) )
                out = 1
            case ( known_commands(2) )
                call help()
            case ( known_commands(3) )
                call simulations()
            case ( known_commands(4) )
                call options()
            case default
                print*,'Command not understood!'
        endselect
        
    end function

    subroutine simulations()
        implicit none
        type(sailor) :: sailorin
        integer(kind=ik) :: number_of_walks,goals(3)
        integer(kind=ik),allocatable :: dataframe(:)
        logical :: wtf
        
        do while( .true. )
            print'(a)',             'Give the number of random walks (integer) to simulate:'
            number_of_walks = get_input()
            if ( number_of_walks == -1 ) then
                print*,             'There was a problem with processing your input! Try again.'
            else
                allocate(dataframe(number_of_walks))
                goals = 0

                print'(a)',         'Do you want to use a self avoiding walk (SAW) algorithm (y/n)?'
                sailorin%saw = y_n_input_handler()

                print'(a)',         'Do you want the trajectories to be written into a file (y/n)?'
                if ( .not. sailorin%saw .and. number_of_walks >= 100 ) then
                    print'(a)',     'WARNING! This is not suggested for non-SAW simulations with more'
                    print'(9x,a)',           'than 100 sailors. This will result in large file sizes!'
                endif
                wtf = y_n_input_handler()
                
                call simulation_subhandler(number_of_walks,sailorin,wtf,dataframe,goals)    ! Launch simulation subhandler

                call data_output(dataframe, goals, sailorin%saw)
                deallocate(dataframe)
                exit
            endif
        enddo
    end subroutine

    subroutine data_output(dataframe, goals, saw)
        implicit none
        integer(kind=ik),intent(in) :: dataframe(:),goals(:)
        integer :: n
        logical,intent(in) :: saw
        
        n = size(dataframe)
        print'(/,3a)',repeat('#', 25),' SIMULATION RESULTS ',repeat('#',25)
        print '(a,f38.3)','Average walked distance (in km):',mean(dataframe)*.1
        print '(a,f38.3)','Minimum walked distance (in km):',minval(real(dataframe))*.1
        print '(a,f38.3)','Maximum walked distance (in km):',maxval(real(dataframe))*.1
        print '(a,i23,a,i0)','Number of sailors who made it to shore on time:',goals(1),'/',n
        if (saw) then
            print '(a,i29,a,i0)','Number of sailors who got into a deadend:',goals(3),'/',n
        else
            print '(a,i43,a,i0)','Number of sailors who died:',goals(2),'/',n
        endif

        write(*,'(a,2x)',advance='no') 'Time (average):'
        call time_data_output(get_time(mean(dataframe)*.1))
        write(*,*)
        write(*,'(a,2x)',advance='no') 'Time (minimum):'
        call time_data_output(get_time(minval(real(dataframe))*.1))
        write(*,*)
        write(*,'(a,2x)',advance='no') 'Time (maximum):'
        call time_data_output(get_time(maxval(real(dataframe))*.1))
        write(*,*)
        print'(a,/)',repeat('#', 70)
        
    end subroutine

    recursive subroutine time_data_output(dataframe)
        implicit none
        integer(kind=ik),intent(in) :: dataframe(:)
        if (dataframe(1) /= 0) then         ! Recursively go through all time values present in the data and print them
            select case (size(dataframe))
                case(5)
                    write (*,'(i0,x,"years",x)',advance='no') dataframe(1)
                    call time_data_output( dataframe(2:) )
                case(4)
                    write (*,'(i0,x,"days",x)',advance='no') dataframe(1)
                    call time_data_output( dataframe(2:) )
                case(3)
                    write (*,'(i0,x,"hours",x)',advance='no') dataframe(1)
                    call time_data_output( dataframe(2:) )
                case(2)
                    write (*,'(i0,x,"minutes",x)',advance='no') dataframe(1)
                    call time_data_output( dataframe(2:) )
                case(1)
                    write (*,'(i0,x,"seconds",x)',advance='no') dataframe(1)
            end select
        else
            call time_data_output( dataframe(2:) )
        endif
    end subroutine

    subroutine simulation_subhandler(n,sailorin,wtf,datain,goals)
        implicit none
        type(sailor),intent(inout) :: sailorin
        logical,intent(in) :: wtf
        integer(kind=ik),intent(out) :: datain(:)
        integer(kind=ik), intent(inout) :: goals(:)
        integer(kind=ik),intent(in) :: n
        integer(kind=ik) :: i,j
        
        if ( wtf ) open(unit=10,file='../run/trajectory.dat')

        do i=1,n
            call progress(i,n)  ! Print overall simulation progress
            !call start_simulations(sailorin,wtf)
            call run_simulation(sailorin,wtf)

            datain(i) = sailorin%blocks ! Log traversed blocks and goals
            do j=1,3
                if (sailorin%achievements(j)) goals(j) = goals(j) + 1
            enddo
        enddo

        if ( wtf ) close(10)
    end subroutine

    subroutine progress(j,len)
        implicit none
        integer(kind=ik)::j,k,len
        character(len=17)::bar="???% |          |"

        if ( j==1 ) bar="???% |          |"     ! Reset the bar if we are starting a new simulation

        write(unit=bar(1:3),fmt="(i3)") 100*j/len
        do k=1, int(10*j/len)
            bar(6+k:6+k)="*"
        enddo
        ! print the progress bar.
        if ( clear_terminal ) call execute_command_line('clear')
        write(unit=6,fmt="(a1,a1,a17)") '+',char(13), bar
    end subroutine

    subroutine options()
        implicit none
        character(len=3) :: checkbox='[ ]'
        integer :: cmd
        
        ! Run the option environment in the UI
        do while ( .true. )
            print '(/,4x,a)','1: exit'
            write(*,'(4x,a,5x)',advance='no')'2: Clearing terminal to display simulation progress more nicely'
            if (clear_terminal) then
                checkbox(2:2) = 'X'
            else
                checkbox(2:2) = ' '
            endif
            write(*,*) checkbox
            write(*,'(3x)',advance='no')
            cmd = get_input()

            select case (cmd)
            case (1)
                exit
            case (2)
                clear_terminal = .not. clear_terminal
            end select
        enddo
        print*,''

    end subroutine

    logical function y_n_input_handler() result(out)
        implicit none
        character(len=80) :: input

        do while(.true.)
            read(*,*) input
            if ( trim(input) == 'y') then
                out = .true.
                exit
            else if ( trim(input) == 'n' ) then
                out = .false.
                exit
            else 
                print*,'Input not recognised! Try again!'
            endif
        enddo
    end function
    
    integer function get_input() result(out)
        implicit none
        integer :: ios
        character(len=80) :: input

        write(*,'(a,x)',advance='no') '>>>' 
        read(5,*,iostat=ios) input
        if (ios /= 0) then  ! Error handling
            out = -1
        else
            read(input,*,iostat=ios)out
            if (ios /= 0) then  ! Error handling
                out = -1
            endif
        endif
    end function

    subroutine welcome()
        print '(/,a)','Welcome to Drunken Sailor!'
        call help()
    end subroutine

    subroutine help()
        print '(/,a,11x,a)','Input','Function'
        print '(a,15x,a)','1','exit the application'
        print '(a,15x,a)','2','see this command list again'
        print '(a,15x,a)','3','start the simulation'
        print '(a,15x,a)','4','change options'
    end subroutine

    subroutine initialize()
        implicit none
        
        clear_terminal = .true.
    end subroutine

end module