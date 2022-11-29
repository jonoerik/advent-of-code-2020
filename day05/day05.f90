program day05
    use flap
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit

    implicit none
    type(command_line_interface) :: cli
    integer :: error
    logical :: flag_part1
    logical :: flag_part2
    logical :: flag_test
    character(len=1024) :: input_path
    character(len=32) :: result_str

    call cli%init(description="Advent of Code 2020 - Day 05", disable_hv=.true.)
    call cli%add(switch="--help", switch_ab="-h", help="Print this help message", &
        act="print_help", def="", error=error)
    if (error /= 0) error stop
    call cli%add(switch="--part1", switch_ab="-1", help="Run program for puzzle part 1", &
        act="store_true", def=".false.", error=error)
    if (error /= 0) error stop
    call cli%add(switch="--part2", switch_ab="-2", help="Run program for puzzle part 2", &
        act="store_true", def=".false.", error=error)
    if (error /= 0) error stop
    call cli%add(switch="--test", help="Run program tests", &
        act="store_true", def=".false.", error=error)
    if (error /= 0) error stop
    call cli%add(positional=.true., position=1, help="File from which to read input data", &
        required=.false., def="", error=error)
    if (error /= 0) error stop
    call cli%parse(error=error)
    if (error /= 0) call exit(1)

    call cli%get(switch="-1", val=flag_part1, error=error)
    if (error /= 0) error stop
    call cli%get(switch="-2", val=flag_part2, error=error)
    if (error /= 0) error stop
    call cli%get(switch="--test", val=flag_test, error=error)
    if (error /= 0) error stop
    call cli%get(position=1, val=input_path, error=error)
    if (error /= 0) error stop

    if (flag_test) then
        call run_tests()
        call exit(0)
    end if

    if (.not. cli%is_passed(position=1)) then
        write (stderr, "(A)") "Input path must be specified."
        call exit(1)
    end if

    if (flag_part1 .eqv. flag_part2) then
        write (stderr, "(A)") "Exactly one of --part1 or --part2 must be specified."
        call exit(1)
    end if

    write (result_str, "(I32)") part1(trim(input_path))
    write (*, "(A)") trim(adjustl(result_str))

    contains
        function part1(input_path) result(highest)
            use, intrinsic :: iso_fortran_env, only : iostat_end
            implicit none
            character(len=*), intent(in) :: input_path
            integer :: highest

            integer :: handle
            character(len=32) :: line
            integer :: error
            integer :: sval

            highest = 0

            open(newunit=handle, file=input_path, status="old", action="read")
            do
                read (handle, "(A)", iostat=error) line
                select case (error)
                case(0)
                    if (len(trim(line)) /= 10) then
                        write (stderr, "(A)") "Input line not of expected length."
                        call exit(1)
                    end if
                    sval = seat_value(line)
                    if (sval > highest) then
                        highest = sval
                    end if
                case(iostat_end)
                    exit
                case default
                    write (stderr, "(A)") "Error reading from input file."
                    call exit(1)
                end select
            end do
            close(handle)
        end function

        function seat_value(s) result(val)
            implicit none
            character(len=*), intent(in) :: s
            integer :: val
            integer :: i

            val = 0
            do i = 1, 10
                val = lshift(val, 1)
                if ((s(i:i) == "B") .or. (s(i:i) == "R")) then
                    val = or(val, 1)
                end if
            end do
        end function

        subroutine run_tests()
            implicit none
            integer :: i = 1
            logical :: exists
            character(len=32) :: filename
            character(len=32) :: test_filename
            logical :: any_failed = .false.
            integer :: test_file_handle
            integer :: expected_result
            integer :: result

            do
                write (filename, "(I32)") i
                filename = "data/sample"//trim(adjustl(filename))
                inquire(file=filename, exist=exists)
                if (.not. exists) then
                    exit
                end if

                test_filename = trim(filename)//".answer1"
                inquire(file=test_filename, exist=exists)
                if (exists) then
                    open (newunit=test_file_handle, file=test_filename, status="old", action="read")
                    read (test_file_handle, *) expected_result
                    close (test_file_handle)

                    result = part1(filename)
                    if (result == expected_result) then
                        write (*, "(A)") trim(filename) // " : part1 : PASSED"
                    else
                        write (*, "(A)") trim(filename) // " : part1 : FAILED"
                        write (*, "(A, I6, A, I6)") "Expected:", expected_result, " Got:", result
                        any_failed = .true.
                    end if
                end if

                i = i + 1
            end do

            if (any_failed) then
                call exit(1)
            end if
        end subroutine
end program day05
