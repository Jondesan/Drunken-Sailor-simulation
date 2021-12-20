program main
    use mtmod
    use ui
    implicit none
    integer :: i

    i = launch()       ! Main program execution
    if (i == -1) then
        print*,'There was a problem in the execution!'
    endif
    
end program main