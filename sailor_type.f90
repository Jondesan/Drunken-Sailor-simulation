module sailor_type
    use mtmod
    use position_type
    implicit none

    !############################################################################
    !   Sailor type variable:
    !
    !   r keeps track of sailor position
    !   blocks tracks how many blocks sailor has traversed
    !   Achievements logs the goals the simulated sailor unlocks
    !   saw holds information on if the sailor is simulated with SAW rules
    !   path is a 2D matrix used to log the visited blocks in SAW simulations
    !
    !############################################################################

    type :: sailor
        type(position) :: r
        integer(kind=ik) :: blocks
        logical :: achievements(3),saw,path(1201,1201),deadend
    end type sailor

    contains

    subroutine move_sailor(sailorin)
        implicit none
        type(sailor),intent(inout) :: sailorin
        logical :: direction_map(3,3)

        if (.not. sailorin%saw) then                ! If sailor doesn't have SAW rules applied simply move the sailor
            call move(sailorin%r)
            
            sailorin%blocks = sailorin%blocks + 1
        else                ! Else get the 3x3 array around the sailor containing the information of legal directions
            direction_map = get_direction_subarray(sailorin)
            call move( sailorin%r, direction_map, sailorin%deadend )    ! Move the sailor according to SAW rules
            sailorin%path( sailorin%r%x+1200, 1201-(sailorin%r%y+600) ) = .true.    ! Mark visited block i.e. drop a bottle
            sailorin%blocks = sailorin%blocks + 1
        endif
    end subroutine

    function get_direction_subarray(sailorin) result(out)
        implicit none
        logical :: out(3,3)
        type(sailor) :: sailorin
        integer(kind=ik) :: arr_len,y_lower,y_upper,x_lower,x_upper

        arr_len = size(sailorin%path,1)
        y_lower = arr_len-(sailorin%r%y+601)
        y_upper = arr_len-(sailorin%r%y+599)
        x_lower = sailorin%r%x+1199
        x_upper = sailorin%r%x+1201

        ! Get the 3x3 matrix around sailor, these contain the legal directions
        out = sailorin%path( x_lower:x_upper, y_lower:y_upper )
    end function

    subroutine run_simulation(sailorin,wtf)
        implicit none
        type(sailor),intent(inout) :: sailorin
        integer(kind=ik) :: i
        logical :: flag
        logical,intent(in) :: wtf

        call reset_sailor(sailorin)     ! Reset the sailor base settings
        if ( wtf ) write(10,*) 'next',new_line('c'),sailorin%r%x,sailorin%r%y
        flag = .true.
        i = 1

        do while(flag)  ! Move the sailor according to simulation rules until an end condition is met
            call move_sailor(sailorin)
            if (wtf) write(10,*) sailorin%r%x,sailorin%r%y
            
            ! Log achieved goals to sailor data
            if ( sailorin%r%x == 0 .or. i == 26280000 .or. sailorin%deadend .or. sailorin%saw .and. i == 600) then
                flag = .false.
                sailorin%achievements(1) = sailorin%r%x == 0 .and. i <= 600 ! GETS TO THE SHORE ON TIME
                sailorin%achievements(2) = i == 26280000                    ! DIED (RAW ONLY)
                sailorin%achievements(3) = sailorin%deadend                 ! DEAD END (SAW ONLY)
            endif
            i = i + 1
        enddo
    end subroutine

    subroutine reset_sailor(sailorin)
        implicit none
        type(sailor),intent(inout) :: sailorin

        sailorin%r%x = -10
        sailorin%r%y = 0
        sailorin%blocks = 0
        sailorin%achievements = .false.
        sailorin%path = .false.
        sailorin%path( 1190, 601 )=.true.
        sailorin%deadend = .false.
    end subroutine

    real function mean(arr) result(out)
        implicit none
        integer(kind=ik),intent(in) :: arr(:)

        out = sum(real(arr))/real(size(arr))
    end function

    function get_time(distance) result(values)
        implicit none
        integer(kind=ik) :: values(5)
        real,intent(in) :: distance
        real :: seconds

        seconds = distance*10*60            
        values(5) = mod(int(seconds),60)
        values(4) = mod(int(seconds/60),60)
        values(3) = mod(int(seconds/60/60),24)
        values(2) = mod(int(seconds/60/60/24),365)
        values(1) = int(seconds/60/60/24/365)
    end function

end module