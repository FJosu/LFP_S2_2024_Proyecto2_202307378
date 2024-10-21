module etiquetas

    implicit none

    type :: contender
        character(len=100)::id
        character(len=100)::type
        character(len=100)::high
        character(len=100)::width
        character(len=200)::text
        character(len=10)::color_r
        character(len=10)::color_g
        character(len=10)::color_b
        character(len=10)::position_x
        character(len=10)::position_y

    end type contender

    type(contender), allocatable :: contender_list(:)

contains

    subroutine add_etiqueta(id)
        character(len=*), intent(in)::id

        type(contender) :: new_contender
        integer :: x
        type(contender), allocatable :: temp(:)


        new_contender%id = id
        new_contender%type = "Etiqueta"
        new_contender%high = ""
        new_contender%width = ""
        new_contender%text = ""
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

    end subroutine add_etiqueta

    subroutine print_etiquetas()

        integer :: i
        if(.not. allocated(contender_list))then
            print*, "No hay etiquetas"
        else
            print*, "Total de etiquetas", size(contender_list)
            do i=1, size(contender_list)
                print*, "Etiqueta ", i, ":"
                print*, "-----------------------------------------------"
                print*, "ID: ", trim(contender_list(i)%id)
                print*, "Tipo: ", trim(contender_list(i)%type)
                print*, "Alto: ", trim(contender_list(i)%high)
                print*, "Ancho: ", trim(contender_list(i)%width)
                print*, "Texto: ", trim(contender_list(i)%text)
                print*, "Color R: ", trim(contender_list(i)%color_r)
                print*, "Color G: ", trim(contender_list(i)%color_g)
                print*, "Color B: ", trim(contender_list(i)%color_b)
                print*, "Posicion X: ", trim(contender_list(i)%position_x)
                print*, "Posicion Y: ", trim(contender_list(i)%position_y)
                print*, "---------------------------------------------------"
            end do

        end if
    end subroutine print_etiquetas

    

    subroutine etiqueta_setwidth(id, width)
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
    end subroutine etiqueta_setwidth

    subroutine etiqueta_settext(id, text)
        character(len=*), intent(in) :: id
        character(len=*), intent(in) :: text
        integer :: i
        if (.not. allocated(contender_list)) then
            print *, "No hay Contenedores"
        else
            do i = 1, size(contender_list)
                if (trim(contender_list(i)%id) == trim(id)) then
                    contender_list(i)%text = text
                else
                end if
            end do
        end if
    end subroutine etiqueta_settext


    subroutine etiqueta_sethigh(id,high)
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
        
    end subroutine etiqueta_sethigh


    subroutine etiqueta_setcolorletra(id,color_r,color_g,color_b)
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
        
    end subroutine etiqueta_setcolorletra

    subroutine etiqueta_setposition(id,position_x,position_y)
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
        
    end subroutine etiqueta_setposition

    

    function get_etiqueta(id) RESULT(find)
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
    end function get_etiqueta
    
end module etiquetas