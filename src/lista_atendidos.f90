module lista_atendidos
    implicit none

    type ClienteAtendido
        character(len=40) :: nombre
        character(len=12) :: ventanillaAtendido
        integer :: img_g, img_p, id
        integer :: totalPasos
        type(ClienteAtendido), pointer :: siguiente => null()
    end type ClienteAtendido

    type clientePasos
        integer :: maxPasos = 0
        character(len=40) :: nombrePasos
        character(len=12) :: ventanilla
    end type clientePasos

    type ListaClientesAtendidos
        type(clientePasos) :: clientePasos
        type(ClienteAtendido), pointer :: head => null()
        contains
            procedure :: agregarCliente
            procedure :: graficar_atendidos
            procedure :: mostrar_lista
            procedure :: clonarLista
            procedure :: buscarCliente
    end type ListaClientesAtendidos

contains

    subroutine agregarCliente(lista, id, nombre, ventanilla, img_g, img_p, totalPasos)
        class(ListaClientesAtendidos), intent(inout) :: lista
        character(len=40), intent(in) :: nombre
        character(len=12), intent(in) :: ventanilla
        integer, intent(in) :: img_g, img_p, totalPasos, id
        type(ClienteAtendido), pointer :: nuevoCliente, actual

        ! Crear y configurar el nuevo cliente
        allocate(nuevoCliente)
        nuevoCliente%id = id
        nuevoCliente%nombre = nombre
        nuevoCliente%ventanillaAtendido = ventanilla
        nuevoCliente%img_g = img_g
        nuevoCliente%img_p = img_p
        nuevoCliente%totalPasos = totalPasos
        nuevoCliente%siguiente => null()

        ! Agregar el nuevo cliente al final de la lista
        if (.not. associated(lista%head)) then
            lista%head => nuevoCliente
        else
            actual => lista%head
            do while(associated(actual%siguiente))
                actual => actual%siguiente
            end do
            actual%siguiente => nuevoCliente
        end if

        write(*,'(A, A, A)')  'SE AGREGO EL CLIENTE ', trim(nuevoCliente%nombre), 'A LA LISTA DE ATENDIDOS'
    end subroutine agregarCliente

    subroutine graficar_atendidos(lista, filename)
        class(ListaClientesAtendidos), intent(in) :: lista
        character(len=*), intent(in) :: filename
        type(ClienteAtendido), pointer :: nodoActual
        integer :: fileUnit, iostat, contador
        character(len=256) :: dotPath, pngPath

        dotPath = 'dot/' // trim(filename)
        pngPath = 'img/' // trim(adjustl(filename)) // '.png'
    
        open(newunit=fileUnit, file=dotPath, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if
    
        write(fileUnit, *) "digraph lista_atendidos {"
        write(fileUnit, *) "    rankdir=LR;" 
        write(fileUnit, *) "    node [shape=record];"
    
        nodoActual => lista%head
            contador = 0
            do while(associated(nodoActual))
                contador = contador + 1
                write(fileUnit, *) '"Node', contador, '" [label="', nodoActual%nombre, ' \nAtendido por: ', &
                nodoActual%ventanillaAtendido, ' \nImg_g: ', nodoActual%img_g, ' \nImg_p: ', nodoActual%img_p, & 
                ' \nPasos totales: ', nodoActual%totalPasos, '"];'

                ! Conectar este nodo con el siguiente si existe
                if (associated(nodoActual%siguiente)) then
                    write(fileUnit, *) '    "Node', contador, '" -> "Node', contador+1, '";'
                end if
        
                nodoActual => nodoActual%siguiente
            end do
    
        write(fileUnit, *) "}"    
        close(fileUnit)
        call system('dot -Tpng ' // trim(dotPath) // ' -o ' // trim(adjustl(pngPath)) // '.png')    
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
    end subroutine graficar_atendidos

    subroutine mostrar_lista(lista, tipo)
        class(ListaClientesAtendidos), intent(in) :: lista
        type(ClienteAtendido), pointer :: nodoActual
        integer, intent(in) :: tipo
        integer :: i

        nodoActual => lista%head
        if (tipo == 1) then
            do i=1, 5
                if (associated(nodoActual)) then
                    write(*,'(I0, A, A, A, I0, A)') i, '. ', nodoActual%nombre, ' tiene ', nodoActual%img_g, ' imagenes Grandes'
                else 
                    exit
                end if
                nodoActual => nodoActual%siguiente
            end do
        elseif (tipo == 0) then
            do i=1, 5
                if (associated(nodoActual)) then
                    write(*,'(I0, A, A, A, I0, A)') i, '. ', nodoActual%nombre, ' tiene ', nodoActual%img_p, ' imagenes Pequenas'
                else 
                    exit
                end if
                nodoActual => nodoActual%siguiente
            end do
        elseif (tipo == 2) then
            do while(associated(nodoActual))
                write(*,'(I0, A, A)') nodoActual%id, ' - ', nodoActual%nombre
                nodoActual => nodoActual%siguiente
            end do
        end if
    end subroutine mostrar_lista

    subroutine clonarLista(original, copia)
        class(ListaClientesAtendidos), intent(in) :: original
        class(ListaClientesAtendidos), intent(out) :: copia
        type(ClienteAtendido), pointer :: nodoOriginal, nuevoNodo, ultimoNodo
    
        nodoOriginal => original%head
        ultimoNodo => null()
    
        ! Recorrer la lista original
        do while (associated(nodoOriginal))
            ! Crear un nuevo nodo con los mismos datos que el nodo original
            allocate(nuevoNodo)
            nuevoNodo%nombre = nodoOriginal%nombre
            nuevoNodo%ventanillaAtendido = nodoOriginal%ventanillaAtendido
            nuevoNodo%img_g = nodoOriginal%img_g
            nuevoNodo%img_p = nodoOriginal%img_p
            nuevoNodo%totalPasos = nodoOriginal%totalPasos
            nuevoNodo%siguiente => null()
    
            ! Agregar el nuevo nodo a la lista copia
            if (.not. associated(copia%head)) then
                copia%head => nuevoNodo
                ultimoNodo => nuevoNodo
            else
                ultimoNodo%siguiente => nuevoNodo
                ultimoNodo => nuevoNodo
            end if
    
            nodoOriginal => nodoOriginal%siguiente
        end do
    end subroutine clonarLista

    subroutine buscarCliente(lista, id)
        class(ListaClientesAtendidos), intent(in) :: lista
        integer, intent(in) :: id
        type(ClienteAtendido), pointer :: nodoActual

        nodoActual => lista%head

        do while(associated(nodoActual))
            if (nodoActual%id == id) then
                print *, "Cliente Encontrado"
                write (*,*) "Cliente: ", nodoActual%nombre
            end if
            nodoActual => nodoActual%siguiente
        end do        
    end subroutine buscarCliente

end module lista_atendidos