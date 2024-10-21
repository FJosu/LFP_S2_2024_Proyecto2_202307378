Module Errors_list
    implicit none 
    type :: Errors
        character(len=100) :: Error_type
        integer :: Error_line
        integer :: Error_column
        character(len=50) :: expected_token
        character(len=100) :: Description
    end type Errors

    type(Errors), allocatable :: Errors_array(:)

contains

    subroutine add_errors(Error_type, Error_line, Error_column, expected_token, Description)
        character(len=*), intent(in) :: Error_type
        integer, intent(in) :: Error_line
        integer, intent(in) :: Error_column
        character(len=*), intent(in) :: expected_token
        character(len=*), intent(in) :: Description

        type(Errors) :: temp_error
        integer :: x
        type(Errors), allocatable :: temp(:)

        temp_error%Error_type = Error_type
        temp_error%Error_line = Error_line
        temp_error%Error_column = Error_column
        temp_error%expected_token = expected_token
        temp_error%Description = Description

        if (.not. allocated(Errors_array)) then
            ! Inicializar el arreglo si no está asignado
            allocate(Errors_array(1))
            Errors_array(1) = temp_error
        else
            ! Redimensionar el arreglo usando un arreglo temporal
            x = size(Errors_array)
            allocate(temp(x+1))
            temp(:x) = Errors_array
            temp(x+1) = temp_error

            ! Mover la asignación con move_alloc para evitar desasignación manual
            call move_alloc(temp, Errors_array)
        end if
    end subroutine add_errors

    subroutine generate_error_html()

        integer :: i, total_errors
        open(unit=10, file='Errores.html', status='replace', action='write')

        if (.not. allocated(Errors_array)) then
            total_errors = 0
        else
            total_errors = size(Errors_array)
        end if

        write(10, *) '<html>'
        write(10, *) '<head><title>Error Report</title></head>'
        write(10, *) '<body>'
        write(10, *) '<h1>Error Report</h1>'
        write(10, *) '<table border="1">'
        write(10, *) '<tr><th>#</th><th>Error Type</th><th>Line</th><th>Column</th><th>Expected Token</th><th>Description</th></tr>'

        do i = 1, total_errors
            write(10, '(A)', advance='no') '<tr><td>'
            write(10, '(I0)', advance='no') i
            write(10, '(A)', advance='no') '</td><td>'
            write(10, '(A)', advance='no') trim(Errors_array(i)%Error_type)
            write(10, '(A)', advance='no') '</td><td>'
            write(10, '(I0)', advance='no') Errors_array(i)%Error_line
            write(10, '(A)', advance='no') '</td><td>'
            write(10, '(I0)', advance='no') Errors_array(i)%Error_column
            write(10, '(A)', advance='no') '</td><td>'
            write(10, '(A)', advance='no') trim(Errors_array(i)%expected_token)
            write(10, '(A)', advance='no') '</td><td>'
            write(10, '(A)', advance='no') trim(Errors_array(i)%Description)
            write(10, '(A)', advance='no') '</td></tr>'
        end do

        write(10, *) '</table>'
        write(10, *) '<p>Total Errors: ', total_errors, '</p>'
        write(10, *) '</body>'
        write(10, *) '</html>'

        close(10)
    end subroutine generate_error_html

END MODULE
