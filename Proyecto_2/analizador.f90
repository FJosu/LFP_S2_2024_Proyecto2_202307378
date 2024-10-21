PROGRAM analizador
    use token_list
    use Errors_list
    use contenedor
    use etiquetas
    use boton
    use check
    use radioboton
    use texto
    use areatexto
    use clave

    implicit none
    integer :: n, line, column, state, p, ios, h, initial_column
    character(len=100000):: content, buffer
    character(len=1):: char
    character(len=100):: token_temp
    character(len=255):: archivo_org  
    integer :: unit_num

    state = 0
    p = 1
    line = 1
    column = 0
    token_temp = ""
    content = ""

    ! Abre el archivo pasado desde Python
    unit_num = 10  ! Número de unidad para el archivo
    open(unit=unit_num, file="entrada1.LFP", status='old', action='read', iostat=ios)

    if (ios /= 0) then
        print*, "Error al abrir el archivo: entrada1.LFP"
        stop
    else
        print*, "Archivo abierto correctamente"
    end if
    

    ! Leer el contenido del archivo
    print*, "Leyendo el contenido del archivo..."
    content = ""
    do
        read(unit_num,'(A)',iostat=ios) buffer
        if (ios /= 0) exit  ! Sale del bucle si llega al final del archivo
        content = trim(content)//trim(buffer) // new_line('a') ! Concatenar líneas en la variable 'content'
    end do
    close(unit_num)  ! Cierra el archivo

    n = len_trim(content)

    do while(p <= n)
        char = content(p:p)

        select case(state)
            case(0)
                if(char >='A' .and. char<= 'Z' .or.(char >='a' .and. char<= 'z'))then
                    initial_column = column  ! Guarda la columna inicial                
                    state = 1
                    column = column + 1
                else if(char >='0' .and. char <= '9')then
                    initial_column = column
                    state = 2
                else if(char =='"')then
                    token_temp = trim(token_temp)// char
                    initial_column = column
                    column = column + 1
                    p=p + 1
                    state = 3
                else if(char == '(' .or. char == ')' .or. char == '<' .or. char == '>' & 
                    .or. char == ';' .or. char == '-' .or. char == '.' .or. char == '!' & 
                    .or. char == '/' .or. char == '*' .or. char == '$' .or. char == '#' &
                    .or. char == ',')then
                    initial_column = column + 1
                    state = 4
                    column = column + 1

                else if (ichar(char) == 10 .or. ichar(char) == 13) then
                    line = line + 1
                    column = 1  ! Reinicia column
                    p = p + 1   ! Asegúrate de avanzar el índice

                else if(ichar(char)==9)then
                    column = column + 4
                    p = p + 1
                else if(ichar(char)==32)then
                    column = column + 1
                    p = p + 1
                else
                    call add_errors("Error Lexico", line, column, "N/A",&
                    "Unrecognized symbol: " // char)
                    column = column  +  1
                    p = p + 1
                end if
            
            case(1)
                if ((char>= 'A' .and. char<='Z') .or. (char>= 'a' .and. char<='z') .or. (char>= '0' .and. char<='9'))then
                    token_temp = trim(token_temp)// char
                    column = column + 1
                    p = p + 1
                else
                    if((token_temp == 'Contenedor'))then
                        call add_token(token_temp, 'token_contenedor',line,initial_column) 
                    else if((token_temp == 'Etiqueta'))then
                        call add_token(token_temp, 'token_etiqueta',line,initial_column)
                    else if((token_temp == 'Boton'))then
                        call add_token(token_temp, 'token_boton',line,initial_column)
                    else if((token_temp == 'Check'))then
                        call add_token(token_temp, 'token_check',line,initial_column)
                    else if((token_temp == 'RadioBoton'))then
                        call add_token(token_temp, 'token_radioboton',line,initial_column)
                    else if((token_temp == 'Texto'))then
                        call add_token(token_temp, 'token_texto',line,initial_column)
                    else if((token_temp == 'AreaTexto'))then
                        call add_token(token_temp, 'token_areatexto',line,initial_column)
                    else if((token_temp == 'Clave'))then
                        call add_token(token_temp, 'token_clave',line,initial_column)
                    else if((token_temp == 'setAncho'))then
                        call add_token(token_temp, 'token_setAncho',line,initial_column)
                    else if((token_temp == 'setAlto'))then
                        call add_token(token_temp, 'token_setalto',line,initial_column)
                    else if((token_temp == 'setColorFondo'))then
                        call add_token(token_temp, 'token_setColor',line,initial_column)
                    else if((token_temp == 'setColorLetra'))then
                        call add_token(token_temp, 'token_setcolorletra',line,initial_column)
                    else if((token_temp == 'setTexto'))then
                        call add_token(token_temp, 'token_settexto',line,initial_column)
                    else if((token_temp == 'setPosicion'))then
                        call add_token(token_temp, 'token_setposicion',line,initial_column)
                    else if((token_temp == 'setAlineacion'))then
                        call add_token(token_temp, 'token_setalineacion',line,initial_column)
                    else if((token_temp == 'setGrupo'))then
                        call add_token(token_temp, 'token_setgrupo',line,initial_column)
                    else if((token_temp == 'setMarcada'))then
                        call add_token(token_temp, 'token_setmarcada',line,initial_column)
                    else if((token_temp == 'this'))then
                        call add_token(token_temp, 'token_this',line,initial_column)
                    else if((token_temp == 'add'))then
                        call add_token(token_temp, 'token_add',line,initial_column)
                    else if((token_temp == 'Controles'))then
                        call add_token(token_temp, 'token_controles',line,initial_column)
                    else if((token_temp == 'Propiedades'))then
                        call add_token(token_temp, 'token_propiedades',line,initial_column)
                    else if ((token_temp == 'Colocacion'))then
                        call add_token(token_temp, 'token_colocacion',line,initial_column)
                    else if ((token_temp == 'centro'))then
                        call add_token(token_temp, 'token_alineacion',line,initial_column)
                    else if ((token_temp == 'izquierdo'))then
                        call add_token(token_temp, 'token_alineacion',line,initial_column)
                    else if ((token_temp == 'derecho'))then
                        call add_token(token_temp, 'token_alineacion',line,initial_column)
                    else if ((token_temp == 'true'))then
                        call add_token(token_temp, 'token_verificar',line,initial_column)
                    else if ((token_temp == 'false'))then
                        call add_token(token_temp, 'token_verificar',line,initial_column)
                    
                    else
                        call add_token(token_temp, 'identificador',line,initial_column)
                    end if
                    token_temp = ""
                    state = 0
                end if

            case(2)
                if(char >= '0' .and. char <= '9')then
                    token_temp = trim(token_temp)// char
                    column = column + 1
                    p = p + 1
                else
                    call add_token(token_temp, 'Entero',line,initial_column)
                    token_temp = ""
                    state = 0
                end if

            case(3)
                if(ichar(char) >= 0 .and. ichar(char) <=235 .and. char .ne. '"')then
                    token_temp = trim(token_temp)// char
                    column = column + 1
                    p = p + 1
                    state = 6
                else if(char == '"')then
                    state = 5
                else
                    call add_errors("Error Lexico", line, initial_column, "N/A",&
                    "Debe estar entre comillas")
                    token_temp = ""
                    state = 0
                end if

            case(4)
                if(char == '(')then
                    call add_token(char,'Parentesis_izquierdo',line,initial_column)
                else if(char == ')')then
                    call add_token(char, 'Parentesis_derecho',line,initial_column)
                else if(char == '<')then
                    call add_token(char, 'Menor_Que',line,initial_column)
                else if(char == '>')then
                    call add_token(char, 'Mayor_Que',line,initial_column)
                else if(char == ';')then
                    call add_token(char, 'PuntoYcoma',line,initial_column)
                else if(char == '-')then
                    call add_token(char, 'Guion',line,initial_column)
                else if(char == '.')then
                    call add_token(char, 'Punto',line,initial_column)
                else if(char == '!')then
                    call add_token(char, 'Exclamacion',line,initial_column)
                else if(char == '/')then
                    call add_token(char, 'Diagonal',line,initial_column)
                else if(char == '*')then
                    call add_token(char, 'Asterisco',line,initial_column)
                else if(char == '$')then
                    call add_token(char, 'Signo_dolar',line,initial_column)
                else if(char == '#')then
                    call add_token(char, 'Numeral',line,initial_column)
                else if(char == ',')then
                    call add_token(char, 'Coma',line,initial_column)
                end if
                p = p + 1  ! Asegúrate de avanzar al siguiente carácter
                state = 0  ! Reinicia el estado

                token_temp = ""
            case(5)

                token_temp = trim(token_temp)//char
                column = column + 1
                p = p + 1
                call add_token(token_temp, 'Cadena',line,initial_column)
                token_temp = ""
                state = 0

            case(6)
                if(ichar(char)>= 0 .and. ichar(char) <= 235 .and. char .ne. '"')then
                    token_temp = trim(token_temp)// char
                    column = column + 1
                    p = p + 1
                else if(char == '"')then
                    state = 5
                else
                    call add_errors("Error Lexico", line, initial_column, "N/A",&
                    "Debe estar entre comillas")
                    token_temp = ""
                    state = 0
                    
                    token_temp = ""
                    state = 0
                end if

        end select

    end do
    call analizador_sintactico! Imprime todos los tokens al finalized
    call print_tokens_html! Imprime todos los tokens al final
    call generate_error_html ! Imprime todos los tokens al final
    call print_contenders ! Imprime todos los tokens al final
    call print_etiquetas
    call print_boton
    call print_check
    call print_radioboton
    call print_texto
    call print_areatexto
    call print_clave
end PROGRAM