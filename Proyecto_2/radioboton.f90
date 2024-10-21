module radioboton

    implicit none

    type :: contender
        character(len=100)::id
        character(len=100)::type
        character(len=100)::high
        character(len=100)::width
        character(len=200)::group
        character(len=100)::marked
        character(len=10)::position_x
        character(len=10)::position_y

    end type contender

    type(contender), allocatable :: contender_list(:)

contains

    subroutine add_radioboton(id)
        character(len=*), intent(in)::id

        type(contender) :: new_contender
        integer :: x
        type(contender), allocatable :: temp(:)


        new_contender%id = id
        new_contender%type = "Boton"
        new_contender%high = "25"
        new_contender%width = "100"
        new_contender%group = ""
        new_contender%marked = ""
        new_contender%position_x = ""
        new_contender%position_y = ""

        if(.not. allocated(contender_list))then
            allocate(contender_list(1))
            contender_list(1) = new_contender
        else
            x = size(contender_list)
            allocate(temp(x+1))
            temp(:x) = contender_list
            temp(x+1) = new_contender
            deallocate(contender_list)
            allocate(contender_list(x+1))
            contender_list=temp
        end if

    end subroutine add_radioboton

    subroutine print_radioboton()

        integer :: i
        if(.not. allocated(contender_list))then
            print*, "No hay botones"
        else
            print*, "Total de botones", size(contender_list)
            do i=1, size(contender_list)
                print*, "Boton ", i, ":"
                print*, "-----------------------------------------------"
                print*, "ID: ", trim(contender_list(i)%id)
                print*, "Tipo: ", trim(contender_list(i)%type)
                print*, "Alto: ", trim(contender_list(i)%high)
                print*, "Ancho: ", trim(contender_list(i)%width)
                print*, "Texto: ", trim(contender_list(i)%group)
                print*, "Alineacion: ", trim(contender_list(i)%marked)
                print*, "Posicion X: ", trim(contender_list(i)%position_x)
                print*, "Posicion Y: ", trim(contender_list(i)%position_y)
                print*, "---------------------------------------------------"
            end do

        end if
    end subroutine print_radioboton

    

    subroutine radioboton_setGrupo(id,group)
        character(len=*), intent(in) :: id 
        character(len=*), intent(in)::group
        integer :: i
        if(.not. allocated(contender_list))then
            print*, ""
        else
            do i=1, size(contender_list)
                if(trim(contender_list(i)%id) == trim(id))then
                    contender_list(i)%group = group
                end if
            end do
        end if
        
    end subroutine radioboton_setGrupo

    subroutine radioboton_setMarcada(id,marked)
        character(len=*), intent(in) :: id 
        character(len=*), intent(in)::marked
        integer :: i
        if(.not. allocated(contender_list))then
            print*, ""
        else
            do i=1, size(contender_list)
                if(trim(contender_list(i)%id) == trim(id))then
                    contender_list(i)%marked = marked
                end if
            end do
        end if
        
    end subroutine radioboton_setMarcada





    subroutine radioboton_setposition(id,position_x,position_y)
        character(len=*), intent(in) :: id 
        character(len=*), intent(in)::position_x
        character(len=*), intent(in) :: position_y
        integer :: i
        if(.not. allocated(contender_list))then
            print*, ""
        else
            do i=1, size(contender_list)
                if(trim(contender_list(i)%id) == trim(id))then
                    contender_list(i)%position_x = position_x
                    contender_list(i)%position_y = position_y
                end if
            end do
        end if
        
    end subroutine radioboton_setposition

    

    function get_radioboton(id) RESULT(find)
        character(len=*), intent(in)::id
        logical :: find
        integer :: i

        find = .false.

        if(.not. allocated(contender_list))then
            print*, ""
        else
            do i=1, size(contender_list)
                if(trim(contender_list(i)%id) == trim(id))then
                    find = .true.
                    return
                end if
            end do
        end if
    end function get_radioboton
    
end module radioboton
