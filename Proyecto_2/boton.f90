module boton

    implicit none

    type :: contender
        character(len=100)::id
        character(len=100)::type
        character(len=100)::high
        character(len=100)::width
        character(len=200)::text
        character(len=100)::alignment
        character(len=10)::position_x
        character(len=10)::position_y

    end type contender

    type(contender), allocatable :: contender_list(:)

contains

    subroutine add_boton(id)
        character(len=*), intent(in)::id

        type(contender) :: new_contender
        integer :: x
        type(contender), allocatable :: temp(:)


        new_contender%id = id
        new_contender%type = "Boton"
        new_contender%high = "25"
        new_contender%width = "100"
        new_contender%text = ""
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

    end subroutine add_boton

    subroutine print_boton()

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
                print*, "Texto: ", trim(contender_list(i)%text)
                print*, "Alineacion: ", trim(contender_list(i)%alignment)
                print*, "Posicion X: ", trim(contender_list(i)%position_x)
                print*, "Posicion Y: ", trim(contender_list(i)%position_y)
                print*, "---------------------------------------------------"
            end do

        end if
    end subroutine print_boton

    

    subroutine boton_settext(id,text)
        character(len=*), intent(in) :: id 
        character(len=*), intent(in)::text
        integer :: i
        if(.not. allocated(contender_list))then
            print*, ""
        else
            do i=1, size(contender_list)
                if(trim(contender_list(i)%id) == trim(id))then
                    contender_list(i)%text = text
                end if
            end do
        end if
        
    end subroutine boton_settext

    subroutine boton_setAlignment(id,alignment)
        character(len=*), intent(in) :: id 
        character(len=*), intent(in)::alignment
        integer :: i
        if(.not. allocated(contender_list))then
            print*, ""
        else
            do i=1, size(contender_list)
                if(trim(contender_list(i)%id) == trim(id))then
                    contender_list(i)%alignment = alignment
                end if
            end do
        end if
        
    end subroutine boton_setAlignment





    subroutine boton_setposition(id,position_x,position_y)
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
        
    end subroutine boton_setposition

    

    function get_boton(id) RESULT(find)
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
    end function get_boton
    
end module boton