module contenedor

    implicit none

    type :: contender
        character(len=100)::id
        character(len=100)::type
        character(len=100)::high
        character(len=100)::width
        character(len=10)::color_r
        character(len=10)::color_g
        character(len=10)::color_b
        character(len=10)::position_x
        character(len=10)::position_y

    end type contender

    type(contender), allocatable :: contender_list(:)

contains

    subroutine add_contender(id)
        character(len=*), intent(in)::id

        type(contender) :: new_contender
        integer :: x
        type(contender), allocatable :: temp(:)


        new_contender%id = id
        new_contender%type = "Contenedor"
        new_contender%high = ""
        new_contender%width = ""
        new_contender%color_r = ""
        new_contender%color_g = ""
        new_contender%color_b = ""
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

    end subroutine add_contender

    subroutine print_contenders()

        integer :: i
        if(.not. allocated(contender_list))then
            print*, "No hay Contenedores"
        else
            print*, "Total de contenedores", size(contender_list)
            do i=1, size(contender_list)
                print*, "Contenedor ", i, ":"
                print*, "-----------------------------------------------"
                print*, "ID: ", trim(contender_list(i)%id)
                print*, "Tipo: ", trim(contender_list(i)%type)
                print*, "Alto: ", trim(contender_list(i)%high)
                print*, "Ancho: ", trim(contender_list(i)%width)
                print*, "Color R: ", trim(contender_list(i)%color_r)
                print*, "Color G: ", trim(contender_list(i)%color_g)
                print*, "Color B: ", trim(contender_list(i)%color_b)
                print*, "Posicion X: ", trim(contender_list(i)%position_x)
                print*, "Posicion Y: ", trim(contender_list(i)%position_y)
                print*, "---------------------------------------------------"
            end do

        end if
    end subroutine print_contenders

    

    subroutine contender_setwidth(id, width)
        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: width
        integer :: i
        if (.not. allocated(contender_list)) then
            print *, "No hay Contenedores"
        else
            do i = 1, size(contender_list)
                if (trim(contender_list(i)%id) == trim(id)) then
                    contender_list(i)%width = width
                else
                end if
            end do
        end if
    end subroutine contender_setwidth


    subroutine contender_sethigh(id,high)
        character(len=*), intent(in) :: id 
        character(len=*), intent(in)::high
        integer :: i
        if(.not. allocated(contender_list))then
            print*, "No hay Contenedores"
        else
            do i=1, size(contender_list)
                if(trim(contender_list(i)%id) == trim(id))then
                    contender_list(i)%high = high
                end if
            end do
        end if
        
    end subroutine contender_sethigh


    subroutine contender_setcolor(id,color_r,color_g,color_b)
        character(len=*), intent(in) :: id 
        character(len=*), intent(in)::color_r
        character(len=*), intent(in)::color_g
        character(len=*), intent(in)::color_b

        integer :: i
        if(.not. allocated(contender_list))then
            print*, "No hay Contenedores"
        else
            do i=1, size(contender_list)
                if(trim(contender_list(i)%id) == trim(id))then
                    contender_list(i)%color_r = color_r
                    contender_list(i)%color_g = color_g
                    contender_list(i)%color_b = color_b
                end if
            end do
        end if
        
    end subroutine contender_setcolor

    subroutine contender_setposition(id,position_x,position_y)
        character(len=*), intent(in) :: id 
        character(len=*), intent(in)::position_x
        character(len=*), intent(in) :: position_y
        integer :: i
        if(.not. allocated(contender_list))then
            print*, "No hay Contenedores"
        else
            do i=1, size(contender_list)
                if(trim(contender_list(i)%id) == trim(id))then
                    contender_list(i)%position_x = position_x
                    contender_list(i)%position_y = position_y
                end if
            end do
        end if
        
    end subroutine contender_setposition

    

    function get_contenders(id) RESULT(find)
        character(len=*), intent(in)::id
        logical :: find
        integer :: i

        find = .false.

        if(.not. allocated(contender_list))then
            print*, "No hay Contenedores"
        else
            do i=1, size(contender_list)
                if(trim(contender_list(i)%id) == trim(id))then
                    find = .true.
                    return
                end if
            end do
        end if
    end function get_contenders
    
end module contenedor