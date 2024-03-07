module cola_clientes
    use json_module
    implicit none
    private

    type, public :: Cliente
        integer :: id
        character(len=:), allocatable :: nombre
        integer :: img_g
        integer :: img_p
    end type Cliente

    type, public :: Nodo
        type(Cliente) :: cliente
        type(Nodo), pointer :: siguiente => null()
    end type Nodo

    type, public :: Cola
        type(Nodo), pointer :: head => null()
        type(Nodo), pointer :: final => null()
    contains
        procedure :: push
        procedure :: cargarClientes
        procedure :: mostrar_cola
        procedure :: pop
        procedure :: contenido
        procedure :: graficar_cola
    end type Cola

contains 
    subroutine push(this_cola, client)
        class(Cola), intent(inout) :: this_cola
        type(Cliente), intent(in) :: client
        type(Nodo), pointer :: newNodo

        allocate(newNodo) 
        newNodo%cliente = client 
        newNodo%siguiente => null()

        if (.not. associated(this_cola%head)) then
            this_cola%head => newNodo
            this_cola%final => newNodo
        else
            this_cola%final%siguiente => newNodo
            this_cola%final => newNodo
        end if
    end subroutine push

    subroutine cargarClientes(this_cola,filename)
        class(Cola), intent(inout) :: this_cola
        character(len=*), intent(in) :: filename
        type(json_file) :: json
        type(json_core) :: jCore
        type(json_value), pointer :: pJsonArray, pJsonValue
        type(Nodo) :: node
        type(Cliente) :: clienteTemp
        integer :: i, nItems
        logical :: found
    
        ! Inicializar la biblioteca JSON
        call json%initialize()
    
        ! Cargar el archivo JSON
        call json%load_file(filename)
    
        ! Obtener el objeto json_core asociado con el archivo json actualmente abierto
        call json%get_core(jCore)
    
        ! Obtener el puntero al array JSON
        call json%get('', pJsonArray, found)
        if (.not. found) then
            print *, "Error: El archivo JSON no contiene un objeto."
            call json%destroy()
            return
        end if
    
        ! Usar json%info para obtener el número de elementos en el array
        call json%info('', n_children=nItems)
    
        ! Iterar a través de cada elemento del array JSON
        do i = 1, nItems
            call jCore%get_child(pJsonArray, i, pJsonValue, found)
            if (.not. found) then
                print *, "Error: No se pudo obtener el elemento ", i, "."
                exit
            endif
    
            ! Extraer los datos del JSON y almacenarlos en el nodo
            call jCore%get(pJsonValue, 'id', clienteTemp%id, found)
            call jCore%get(pJsonValue, 'nombre', clienteTemp%nombre, found)
            call jCore%get(pJsonValue, 'img_g', clienteTemp%img_g, found)
            call jCore%get(pJsonValue, 'img_p', clienteTemp%img_p, found)
    
            call this_cola%push(clienteTemp)
        end do
    
        ! Finalizar la biblioteca JSON
        call json%destroy()
    end subroutine cargarClientes   
    
    
    subroutine mostrar_cola(this_cola)
        class(Cola), intent(in) :: this_cola
        type(Nodo), pointer :: nodoActual

        nodoActual => this_cola%head
        if (.not. associated(nodoActual)) then
            print *, "La cola está vacía."
            return
        end if

        print *, "Elementos de la cola:"
        do while(associated(nodoActual))
            print *, "ID:", nodoActual%cliente%id, "Nombre:", trim(nodoActual%cliente%nombre), &
            "Imagenes Grandes:", nodoActual%cliente%img_g, "Imagenes Pequenas:", nodoActual%cliente%img_p
            nodoActual => nodoActual%siguiente   
        end do
    end subroutine mostrar_cola
    
    function pop(this_cola) result(clienteRetornado)
        class(Cola), intent(inout) :: this_cola
        type(Cliente) :: clienteRetornado
        type(Nodo), pointer :: nodoTemp
    
        ! Verificar si la cola está vacía
        if (associated(this_cola%head)) then
            clienteRetornado = this_cola%head%cliente
            
            ! Mover el puntero head al siguiente nodo
            nodoTemp => this_cola%head
            this_cola%head => this_cola%head%siguiente
            
            ! Verificar si la cola está ahora vacía
            if (.not. associated(this_cola%head)) then
                this_cola%final => null()
            end if
            
            deallocate(nodoTemp)
        else
            return 
        end if
    end function pop
    
    function contenido(this_cola) result(content)
        class(Cola), intent(in) :: this_cola
        logical :: content
    
        content = associated(this_cola%head)
    end function contenido

    subroutine graficar_cola(this_cola, filename)
        class(Cola), intent(in) :: this_cola
        character(len=*), intent(in) :: filename
        type(Nodo), pointer :: nodoActual
        integer :: fileUnit, iostat, contador
        character(len=256) :: dotPath, pngPath

        dotPath = 'dot/' // trim(filename)
        pngPath = 'img/' // trim(adjustl(filename)) // '.png'
    
        ! Abrir archivo .dot para escritura
        open(newunit=fileUnit, file=dotPath, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if
    
        write(fileUnit, *) "digraph cola_clientes {"
        write(fileUnit, *) "    rankdir=LR;" 
        write(fileUnit, *) "    node [shape=record];"
    
        nodoActual => this_cola%head
    
        do while(associated(nodoActual))
            contador = contador + 1
            write(fileUnit, *) '"Node', contador, '" [label="', nodoActual%cliente%nombre, ' \nImg_g: ', &
            nodoActual%cliente%img_g, ' \nImg_p: ', nodoActual%cliente%img_p, '"];'

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
    end subroutine graficar_cola
    
end module cola_clientes