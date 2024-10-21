MODULE token_list

    use Errors_list
    use contenedor
    use etiquetas
    use boton
    use check
    use radioboton
    use texto
    use areatexto
    use clave

    IMPLICIT NONE
    type :: token
        character(len=30) :: lexeme
        character(len=30):: type
        integer :: line
        integer :: column
    End type token

    !Arreglo dinámico
    type(token), allocatable :: tokens_array(:)

contains
    subroutine add_token(lexeme, type, line, column)
        character(len=*), intent(in) :: lexeme
        character(len=*), intent(in) :: type
        integer, intent(in) :: line
        integer, intent(in) :: column

        type(token) :: new_token
        integer :: x
        type(token), allocatable :: temp(:)

        new_token%lexeme = lexeme
        new_token%type = type
        new_token%line = line
        new_token%column = column

        if(.not. allocated(tokens_array))then
            allocate(tokens_array(1))
            tokens_array(1) = new_token
        else
            x = size(tokens_array)
            allocate(temp(x+1))
            temp(:x) = tokens_array
            temp(x+1) = new_token
            deallocate(tokens_array)
            allocate(tokens_array(x+1))
            tokens_array=temp
        end if
    end subroutine add_token

    subroutine print_tokens()
        integer :: i
        character(len=5) :: str_line,str_solumn

        if(.not. allocated(tokens_array))then
            print *,"No hay tokens"
        else
            print *, "Tokens encontrados: ", size(tokens_array)

            do i=1, size(tokens_array)
                write(str_line,'(i0)') tokens_array(i)%line
                write(str_solumn,'(i0)') tokens_array(i)%column
                print *, 'Lexema: ',trim(tokens_array(i)%lexeme)
                print *, 'Tipo: ',trim(tokens_array(i)%type)
                print *, 'Línea: ',str_line
                print *, 'Columna: ',str_solumn
                print *, '-----------------------------------'

            end do
        end if
    
    end subroutine print_tokens

    subroutine print_tokens_html()
    integer :: i
    character(len=5) :: str_line, str_column
    character(len=255) :: html_row

    open(unit=11, file="tokens.html", status='replace', action='write')

    ! Imprimir la cabecera de la tabla HTML
    write(11,*) '<html><body><table border="1">'
    write(11,*) '<tr><th>Lexema</th><th>Tipo</th><th>Línea</th><th>Columna</th></tr>'

    if (.not. allocated(tokens_array)) then
        write(11,*) '<tr><td colspan="4">No hay tokens</td></tr>'
    else
        do i = 1, size(tokens_array)
            write(str_line, '(i0)') tokens_array(i)%line
            write(str_column, '(i0)') tokens_array(i)%column
            ! Crear una fila HTML con los datos de cada token
            html_row = '<tr><td>' // trim(tokens_array(i)%lexeme) // '</td>' // &
                       '<td>' // trim(tokens_array(i)%type) // '</td>' // &
                       '<td>' // str_line // '</td>' // &
                       '<td>' // str_column // '</td></tr>'
            write(11,*) trim(html_row)
        end do
    end if

    ! Cerrar la tabla y el archivo HTML
    write(11,*) '</table></body></html>'
    close(11)
    
    print*, "Tokens exportados a tokens.html"
end subroutine print_tokens_html

    subroutine analizador_sintactico()
        integer :: i

        if(.not. allocated(tokens_array))then
            print *,"No hay tokens"
        else

            do i=1, size(tokens_array)

                !Función para dectectar el inicio de una apertura
                if(tokens_array(i)%type =='Menor_Que')then
                    if(tokens_array(i+1)%type == 'Exclamacion')then
                        if(tokens_array(i+2)%type == 'Guion')then
                            if(tokens_array(i+3)%type == 'Guion')then
                                if(tokens_array(i+4)%type == 'token_controles')then
                                    print *, "Controles"
                                elseif(tokens_array(i+4)%type == 'token_propiedades')then
                                    print *, "Controles"
                                elseif(tokens_array(i+4)%type == 'token_colocacion')then
                                    print *, "Controles"
                                else
                                    call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                                    'Se esperaba Controles,Propiedades o Colocacion','')
                                end if

                            else
                                call add_errors('Error sintactico', tokens_array(i+3)%line, tokens_array(i+3)%column,&
                                'Se esperaba -','')

                            end if

                        else
                            call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                            'Se esperaba -','')

                        end if

                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba !','')
                    end if
                end if
                !Funcion para verifical el final de controes, propiedades y colocacion
                if(tokens_array(i)%type =='token_controles'.and. tokens_array(i)%column == 1)then
                    if(tokens_array(i+1)%type =='Guion')then
                        if(tokens_array(i+2)%type =='Guion')then
                            if(tokens_array(i+3)%type =='Mayor_Que')then

                            else
                                call add_errors('Error sintactico', tokens_array(i+3)%line, tokens_array(i+3)%column,&
                                'Se esperaba >','Debe ir un Mayor que')
                            end if
                            else
                                call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                                'Se esperaba -','Debe ir un Guion')
                        end if
                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba -','Debe ir un Guion')
                    
                    end if
                elseif(tokens_array(i)%type =='token_propiedades'.and. tokens_array(i)%column == 1)then
                    if(tokens_array(i+1)%type =='Guion')then
                        if(tokens_array(i+2)%type =='Guion')then
                            if(tokens_array(i+3)%type =='Mayor_Que')then

                            else
                                call add_errors('Error sintactico', tokens_array(i+3)%line, tokens_array(i+3)%column,&
                                'Se esperaba >','Debe ir un Mayor que')
                            end if
                            else
                                call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                                'Se esperaba -','Debe ir un Guion')
                        end if
                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba -','Debe ir un Guion')
                    
                    end if

                elseif(tokens_array(i)%type =='token_colocacion'.and. tokens_array(i)%column == 1)then
                    if(tokens_array(i+1)%type =='Guion')then
                        if(tokens_array(i+2)%type =='Guion')then
                            if(tokens_array(i+3)%type =='Mayor_Que')then

                            else
                                call add_errors('Error sintactico', tokens_array(i+3)%line, tokens_array(i+3)%column,&
                                'Se esperaba >','Debe ir un Mayor que')
                            end if
                            else
                                call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                                'Se esperaba -','Debe ir un Guion')
                        end if
                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba -','Debe ir un Guion')
                    
                    end if

                end if

                !Funcion para guardar un contenedor
                if(tokens_array(i)%type =='token_contenedor')then
                    if(tokens_array(i+1)%type =='identificador')then
                        if(tokens_array(i+2)%type =='PuntoYcoma')then
                            call  add_contender(tokens_array(i+1)%lexeme)

                        else
                            call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                            'Se esperaba ;','Se esperaba punto y coma')
                        end if

                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba identificador','Se esperaba un identificador')
                    end if

                end if
                
                if(tokens_array(i)%type =='identificador' .and. tokens_array(i+1)%type =='Punto')then
                    if(tokens_array(i+2)%type =='token_setAncho')then
                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call contender_setwidth(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
                    else if(tokens_array(i+2)%type =='token_setalto')then
                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call contender_sethigh(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
                    
                    else if(tokens_array(i+2)%type =='token_setColor')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba ,','Se esperaba una Coma')
                            
                        elseif(tokens_array(i+6)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ,','Se esperaba una ')
                            
                        elseif(tokens_array(i+7)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+7)%line,tokens_array(i+7)%column,&
                            'Se esperaba ,','Se esperaba una coma')
                        
                        elseif(tokens_array(i+8)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+8)%line,tokens_array(i+48)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+9)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+9)%line,tokens_array(i+9)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+10)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+10)%line,tokens_array(i+10)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call contender_setcolor(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme,&
                            tokens_array(i+6)%lexeme,tokens_array(i+8)%lexeme)
                        
                        end if
                    else if(tokens_array(i+2)%type =='token_setposicion')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba ,','Se esperaba una Coma')
                            
                        elseif(tokens_array(i+6)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ,','Se esperaba una ')
                            

                        elseif(tokens_array(i+7)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+7)%line,tokens_array(i+7)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+8)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+8)%line,tokens_array(i+8)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call contender_setposition(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme,&
                            tokens_array(i+6)%lexeme)
                        
                        end if
                    end if

                end if

                !Funcion para guardar y clasificar las etiquetas
                if(tokens_array(i)%type =='token_etiqueta')then
                    if(tokens_array(i+1)%type =='identificador')then
                        if(tokens_array(i+2)%type =='PuntoYcoma')then
                            call  add_etiqueta(tokens_array(i+1)%lexeme)

                        else
                            call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                            'Se esperaba ;','Se esperaba punto y coma')
                        end if

                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba identificador','Se esperaba un identificador')
                    end if

                end if

                if(tokens_array(i)%type =='identificador' .and. tokens_array(i+1)%type =='Punto')then
                    if(tokens_array(i+2)%type =='token_setAncho')then
                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call etiqueta_setwidth(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
                    else if(tokens_array(i+2)%type =='token_setalto')then
                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call etiqueta_sethigh(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
                    
                    else if(tokens_array(i+2)%type =='token_setcolorletra')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba ,','Se esperaba una Coma')
                            
                        elseif(tokens_array(i+6)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ,','Se esperaba una ')
                            
                        elseif(tokens_array(i+7)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+7)%line,tokens_array(i+7)%column,&
                            'Se esperaba ,','Se esperaba una coma')
                        
                        elseif(tokens_array(i+8)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+8)%line,tokens_array(i+48)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+9)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+9)%line,tokens_array(i+9)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+10)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+10)%line,tokens_array(i+10)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call etiqueta_setcolorletra(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme,&
                            tokens_array(i+6)%lexeme,tokens_array(i+8)%lexeme)
                        
                        end if
                    else if(tokens_array(i+2)%type =='token_setposicion')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba ,','Se esperaba una Coma')
                            
                        elseif(tokens_array(i+6)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ,','Se esperaba una ')
                            

                        elseif(tokens_array(i+7)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+7)%line,tokens_array(i+7)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+8)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+8)%line,tokens_array(i+8)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call etiqueta_setposition(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme,&
                            tokens_array(i+6)%lexeme)
                        
                        end if
                    else if(tokens_array(i+2)%type =='token_settexto')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'Cadena')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba una Cadena','Se esperaba una cadena')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call etiqueta_settext(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
    

                    end if

                end if

                !Funcion para guardar y clasificar los botones
                if(tokens_array(i)%type =='token_boton')then
                    if(tokens_array(i+1)%type =='identificador')then
                        if(tokens_array(i+2)%type =='PuntoYcoma')then
                            call  add_boton(tokens_array(i+1)%lexeme)

                        else
                            call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                            'Se esperaba ;','Se esperaba punto y coma')
                        end if

                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba identificador','Se esperaba un identificador')
                    end if

                end if

                if(tokens_array(i)%type =='identificador' .and. tokens_array(i+1)%type =='Punto')then
                    
                    if(tokens_array(i+2)%type =='token_setposicion')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba ,','Se esperaba una Coma')
                            
                        elseif(tokens_array(i+6)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ,','Se esperaba una ')
                            

                        elseif(tokens_array(i+7)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+7)%line,tokens_array(i+7)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+8)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+8)%line,tokens_array(i+8)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call boton_setposition(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme,&
                            tokens_array(i+6)%lexeme)
                        
                        end if
                    else if(tokens_array(i+2)%type =='token_settexto')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'Cadena')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba una Cadena','Se esperaba una cadena')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call boton_settext(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
                    
                    else if(tokens_array(i+2)%type =='token_setalineacion')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'token_alineacion')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba una Cadena','Se esperaba una cadena')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call boton_setAlignment(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
    

                    end if

                end if

                !Funcion para guardar y clasificar los check
                if(tokens_array(i)%type =='token_check')then
                    if(tokens_array(i+1)%type =='identificador')then
                        if(tokens_array(i+2)%type =='PuntoYcoma')then
                            call  add_check(tokens_array(i+1)%lexeme)

                        else
                            call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                            'Se esperaba ;','Se esperaba punto y coma')
                        end if

                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba identificador','Se esperaba un identificador')
                    end if

                end if

                if(tokens_array(i)%type =='identificador' .and. tokens_array(i+1)%type =='Punto')then
                    
                    if(tokens_array(i+2)%type =='token_setposicion')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba ,','Se esperaba una Coma')
                            
                        elseif(tokens_array(i+6)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ,','Se esperaba una ')
                            

                        elseif(tokens_array(i+7)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+7)%line,tokens_array(i+7)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+8)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+8)%line,tokens_array(i+8)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call check_setposition(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme,&
                            tokens_array(i+6)%lexeme)
                        
                        end if

                    else if(tokens_array(i+2)%type =='token_setgrupo')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'identificador')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba un identificador','Se esperaba un ID')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call check_setGrupo(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
                    
                    else if(tokens_array(i+2)%type =='token_setmarcada')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'token_verificar')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba una true o false','Se esperaba un true o false')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call check_setMarcada(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
    

                    end if

                end if

                !Funcion para guardar y clasificar los radioboton
                if(tokens_array(i)%type =='token_radioboton')then
                    if(tokens_array(i+1)%type =='identificador')then
                        if(tokens_array(i+2)%type =='PuntoYcoma')then
                            call  add_radioboton(tokens_array(i+1)%lexeme)

                        else
                            call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                            'Se esperaba ;','Se esperaba punto y coma')
                        end if

                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba identificador','Se esperaba un identificador')
                    end if

                end if

                if(tokens_array(i)%type =='identificador' .and. tokens_array(i+1)%type =='Punto')then
                    
                    if(tokens_array(i+2)%type =='token_setposicion')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba ,','Se esperaba una Coma')
                            
                        elseif(tokens_array(i+6)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ,','Se esperaba una ')
                            

                        elseif(tokens_array(i+7)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+7)%line,tokens_array(i+7)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+8)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+8)%line,tokens_array(i+8)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call radioboton_setposition(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme,&
                            tokens_array(i+6)%lexeme)
                        
                        end if

                    else if(tokens_array(i+2)%type =='token_setgrupo')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'identificador')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba un identificador','Se esperaba un ID')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call radioboton_setGrupo(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
                    
                    else if(tokens_array(i+2)%type =='token_setmarcada')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'token_verificar')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba una true o false','Se esperaba un true o false')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call radioboton_setMarcada(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
    

                    end if

                end if

                !Funcion para guardar y clasificar los Textos
                if(tokens_array(i)%type =='token_texto')then
                    if(tokens_array(i+1)%type =='identificador')then
                        if(tokens_array(i+2)%type =='PuntoYcoma')then
                            call  add_texto(tokens_array(i+1)%lexeme)

                        else
                            call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                            'Se esperaba ;','Se esperaba punto y coma')
                        end if

                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba identificador','Se esperaba un identificador')
                    end if

                end if

                if(tokens_array(i)%type =='identificador' .and. tokens_array(i+1)%type =='Punto')then
                    
                    if(tokens_array(i+2)%type =='token_setposicion')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba ,','Se esperaba una Coma')
                            
                        elseif(tokens_array(i+6)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ,','Se esperaba una ')
                            

                        elseif(tokens_array(i+7)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+7)%line,tokens_array(i+7)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+8)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+8)%line,tokens_array(i+8)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call texto_setposition(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme,&
                            tokens_array(i+6)%lexeme)
                        
                        end if

                    else if(tokens_array(i+2)%type =='token_settexto')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'Cadena')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba una cadena','Se esperaba una cadena')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call texto_settext(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
                    
                    else if(tokens_array(i+2)%type =='token_setalineacion')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'token_alineacion')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba una true o false','Se esperaba un true o false')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call texto_setAlignment(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
    

                    end if

                end if

                !Funcion para guardar y clasificar las Areastexto
                if(tokens_array(i)%type =='token_areatexto')then
                    if(tokens_array(i+1)%type =='identificador')then
                        if(tokens_array(i+2)%type =='PuntoYcoma')then
                            call  add_areatexto(tokens_array(i+1)%lexeme)

                        else
                            call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                            'Se esperaba ;','Se esperaba punto y coma')
                        end if

                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba identificador','Se esperaba un identificador')
                    end if

                end if

                if(tokens_array(i)%type =='identificador' .and. tokens_array(i+1)%type =='Punto')then
                    
                    if(tokens_array(i+2)%type =='token_setposicion')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba ,','Se esperaba una Coma')
                            
                        elseif(tokens_array(i+6)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ,','Se esperaba una ')
                            

                        elseif(tokens_array(i+7)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+7)%line,tokens_array(i+7)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+8)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+8)%line,tokens_array(i+8)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call areatexto_setposition(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme,&
                            tokens_array(i+6)%lexeme)
                        
                        end if

                    else if(tokens_array(i+2)%type =='token_settexto')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'Cadena')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba una cadena','Se esperaba una cadena')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call areatexto_settext(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
                    

                    end if

                end if


                !Funcion para guardar y clasificar la clave
                if(tokens_array(i)%type =='token_clave')then
                    if(tokens_array(i+1)%type =='identificador')then
                        if(tokens_array(i+2)%type =='PuntoYcoma')then
                            call  add_clave(tokens_array(i+1)%lexeme)

                        else
                            call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                            'Se esperaba ;','Se esperaba punto y coma')
                        end if

                    else
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba identificador','Se esperaba un identificador')
                    end if

                end if

                if(tokens_array(i)%type =='identificador' .and. tokens_array(i+1)%type =='Punto')then
                    
                    if(tokens_array(i+2)%type =='token_setposicion')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                            
                        elseif(tokens_array(i+4)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba numero entero','Se esperaba un Entero')

                        elseif(tokens_array(i+5)%type .ne. 'Coma')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba ,','Se esperaba una Coma')
                            
                        elseif(tokens_array(i+6)%type .ne. 'Entero')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ,','Se esperaba una ')
                            

                        elseif(tokens_array(i+7)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+7)%line,tokens_array(i+7)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+8)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+8)%line,tokens_array(i+8)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                        
                        else
                            call clave_setposition(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme,&
                            tokens_array(i+6)%lexeme)
                        
                        end if

                    else if(tokens_array(i+2)%type =='token_settexto')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'Cadena')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba una cadena','Se esperaba una cadena')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call clave_settext(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
                    
                    else if(tokens_array(i+2)%type =='token_setalineacion')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                        
                        elseif(tokens_array(i+4)%type .ne. 'token_alineacion')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba una true o false','Se esperaba un true o false')
                        
                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')
                        
                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')
                            
                        else
                            call clave_setAlignment(tokens_array(i)%lexeme,tokens_array(i+4)%lexeme)
                        
                        end if
    

                    end if

                end if

                if(tokens_array(i)%type =='token_this' .and. tokens_array(i+1)%type =='Punto')then
                    if(tokens_array(i+2)%type =='token_add')then

                        if(tokens_array(i+3)%type .ne. 'Parentesis_izquierdo')then
                            call add_errors('Error sintactico', tokens_array(i+3)%line,tokens_array(i+3)%column,&
                            'Se esperaba (','Se esperaba un Parentesis_izquierdo')
                                
                        elseif(tokens_array(i+4)%type .ne. 'Identificador')then
                            call add_errors('Error sintactico', tokens_array(i+4)%line,tokens_array(i+4)%column,&
                            'Se esperaba un identificador','Se esperaba un identificador')

                        elseif(tokens_array(i+5)%type .ne. 'Parentesis_derecho')then
                            call add_errors('Error sintactico', tokens_array(i+5)%line,tokens_array(i+5)%column,&
                            'Se esperaba )','Se esperaba un Parentesis_derecho')

                        elseif(tokens_array(i+6)%type .ne. 'PuntoYcoma')then
                            call add_errors('Error sintactico', tokens_array(i+6)%line,tokens_array(i+6)%column,&
                            'Se esperaba ;','Se esperaba un Punto y coma')

                        end if

                    else
                        call add_errors('Error sintactico', tokens_array(i+2)%line, tokens_array(i+2)%column,&
                        'Se esperaba add','Se esperaba add')
                    end if

                end if

                if(tokens_array(i)%type =='Diagonal')then
                    if(tokens_array(i+1)%type .ne. 'Diagonal')then
                        call add_errors('Error sintactico', tokens_array(i+1)%line, tokens_array(i+1)%column,&
                        'Se esperaba diagonal','Se esperaba diagonal')

                    end if


                end if

                

                

            end do
        end if

        
    

    end subroutine analizador_sintactico

END MODULE token_list