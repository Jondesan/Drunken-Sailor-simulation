module position_type
    use mtmod
    implicit none
    integer,parameter :: ik=selected_int_kind(18)
    character(len=1) :: all_possible_directions(4)=(/'E','N','W','S'/)

    !############################################################################
    !   Position type variable:
    !
    !   x and y depict the x and y coordinates of position.
    !
    !############################################################################

    type :: position
        integer(kind=ik) :: x=-10,y=0
    end type position

    contains

    subroutine move(pos,arr,deadend)
        implicit none
        type(position),intent(inout) :: pos
        logical,intent(in),optional :: arr(:,:)
        logical,intent(out),optional :: deadend
        integer :: direction
        character(len=1),allocatable :: possible_directions(:)
        
        if ( present(arr) .and. present(deadend) ) then     ! If optional parameters present use SAW rules
            possible_directions = get_possible_paths(arr)
        else                                                ! Else all directions are possible
            possible_directions = all_possible_directions
        endif

        do while(.true.)    ! Due to a bug in grnd (which carries on to igrnd) we make sure the rng values are acceptable
            direction = igrnd( 1, size( possible_directions ) )
            if (any((/1,2,3,4/)==direction)) exit
        enddo


        select case ( possible_directions( direction ) )    ! Based on rng change the position values
            case ( 'E' )  ! East
                pos%x = pos%x + 1
            case ( 'N' )  ! North
                pos%y = pos%y + 1
            case ( 'W' )  ! West
                pos%x = pos%x - 1
            case ( 'S' )  ! South
                pos%y = pos%y - 1
            case ( '0' )  ! No possible directions, end run
                deadend = .true.
        endselect
        deallocate(possible_directions)
    end subroutine

    function get_possible_paths(arr) result(possible_paths)
        implicit none
        logical,intent(in) :: arr(:,:)
        logical :: arr_to_queue(4)
        character(len=1),allocatable :: possible_paths(:)
        character(len=1) :: dummy(4)

        ! Take the possible directions into a 1D array for easy handling
        arr_to_queue = (/ arr(3,2), arr(2,1), arr(1,2), arr(2,3) /)

        if ( count(arr_to_queue) == 4) then ! If there are no legal moves, possible paths is 0
            allocate ( possible_paths(1) )
            possible_paths='0'
        else                                ! Else create an array of the possible directions
            allocate( possible_paths(count( .not. arr_to_queue )) )
            where ( .not. arr_to_queue)
                dummy = all_possible_directions
            elsewhere
                dummy = '0'
            endwhere

            possible_paths = pack(dummy, dummy /= '0')
        endif
    end function

end module