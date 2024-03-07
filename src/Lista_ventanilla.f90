! Pila de Imagenes de cada ventanilla

module pila_imagenes
    implicit none
    private
    public :: estaVacia

    type, public :: Imagen
        character(len=100) :: nombre
        character(len=12) :: ventanilla
        integer :: id, img_g, img_p, img_gTemp, img_pTemp, pasoInicial
    end type Imagen

    type, public :: nodo_imagen
        type(Imagen) :: dato
        type(nodo_imagen), pointer :: siguiente => null()
        integer :: contP = 0
        integer :: contG = 0
    end type nodo_imagen

    type, public :: PilaImagenes
        type(nodo_imagen), pointer :: top => null()
        contains
            procedure :: pushPila
            procedure :: pop
            procedure :: enviarImagen
            procedure :: clearPila
    end type PilaImagenes

contains

    subroutine pushPila(this_pila, image)
        class(PilaImagenes), intent(inout) :: this_pila
        type(Imagen), intent(in) :: image
        type(nodo_imagen), pointer :: newNodo

        allocate(newNodo)
        newNodo%dato = image
        newNodo%siguiente => this_pila%top
        this_pila%top => newNodo
    end subroutine pushPila

    subroutine pop(this_pila, image)
        class(PilaImagenes), intent(inout) :: this_pila
        type(Imagen), intent(out) :: image
        type(nodo_imagen), pointer :: temp

        if (.not. associated(this_pila%top)) then
            return
        end if
        image = this_pila%top%dato
        temp => this_pila%top
        this_pila%top => this_pila%top%siguiente
        deallocate(temp)
    end subroutine pop

    function estaVacia(this_pila) result(vacia)
        type(PilaImagenes), intent(in) :: this_pila
        logical :: vacia

        vacia = .not. associated(this_pila%top)
    end function estaVacia

    function enviarImagen(this_pila,colaImagenG,colaImagenP, listaEspera) result(estado)
        use cola_impresion
        use Listclientes_espera
        class(PilaImagenes), intent(inout) :: this_pila
        type(ColaImpresion), intent(inout) ::  colaImagenG, colaImagenP
        type(Lista_espera), intent(inout) :: listaEspera
        type(clientesEspera) :: clienteEspera
        type(nodo_imagen), pointer :: nodoActual
        type(Imagen_imp) :: imagenImp
        logical :: estado
        integer :: i

        nodoActual => this_pila%top
        do while(associated(nodoActual))
                write(*,'(A, A, A, A)') 'LA ',trim(nodoActual%dato%ventanilla),' RECIBIO UNA IMAGEN DEL CLIENTE ', &
                trim(nodoActual%dato%nombre)
                ! Restar 1 a img_gTemp e img_pTemp si son mayores que 0 para llevar el conteo
                if (nodoActual%dato%img_gTemp > 0) then
                    nodoActual%dato%img_gTemp = nodoActual%dato%img_gTemp - 1
                    nodoActual%contG = nodoActual%contG + 1
                    estado = .false.
                elseif (nodoActual%dato%img_pTemp > 0) then
                    nodoActual%dato%img_pTemp = nodoActual%dato%img_pTemp - 1
                    nodoActual%contP = nodoActual%contP + 1
                    estado = .false.
                else 
                    ! Cuando ya ha cargado todas sus imagenes (contadores = 0) se pasara el cliente a espera
                    clienteEspera%id = nodoActual%dato%id
                    clienteEspera%nombreCliente = nodoActual%dato%nombre
                    clienteEspera%img_p = nodoActual%dato%img_p
                    clienteEspera%img_g = nodoActual%dato%img_g
                    clienteEspera%ventanilla = nodoActual%dato%ventanilla
                    clienteEspera%pasoInicial = nodoActual%dato%pasoInicial
                    call listaEspera%insertarCliente(clienteEspera)

                    ! Agregando las imagenes cargadas en ventanilla a las colas de impresion
                    write(*,'(A, I0, A, A)') 'AGREGANDO IMAGENES GRANDES A LA IMPRESION DEL CLIENTE: ', &
                        nodoActual%dato%id, '-', nodoActual%dato%nombre
                    do i = 1, nodoActual%dato%img_g
                        imagenImp%id_Cliente = nodoActual%dato%id
                        imagenImp%nombre = nodoActual%dato%nombre
                        imagenImp%tipo = 1
                        imagenImp%id = colaImagenG%obtenerUltimoId()
                        call colaImagenG%encolarImpresion(imagenImp)
                    end do

                    write(*,'(A, I0, A, A)') 'AGREGANDO IMAGENES PEQUENAS A LA IMPRESION DEL CLIENTE: ', &
                    nodoActual%dato%id, '-', nodoActual%dato%nombre
                    do i = 1, nodoActual%dato%img_p
                        imagenImp%id_Cliente = nodoActual%dato%id
                        imagenImp%nombre = nodoActual%dato%nombre
                        imagenImp%tipo = 0
                        imagenImp%id = colaImagenP%obtenerUltimoId()
                        call colaImagenP%encolarImpresion(imagenImp)  
                    end do            
                    estado = .true.
                    call this_pila%clearPila()
                end if
                ! Salir del bucle una vez que se encuentra y modifica la imagen
                exit
            nodoActual => nodoActual%siguiente
        end do
    end function enviarImagen

    subroutine clearPila(this_pila)
        class(PilaImagenes), intent(inout) :: this_pila
        type(nodo_imagen), pointer :: temp
    
        ! Itera mientras haya elementos en la pila para eliminarlos
        do while (associated(this_pila%top))
            temp => this_pila%top             
            this_pila%top => this_pila%top%siguiente  
            deallocate(temp)                  
        end do
    end subroutine clearPila
    
    
end module pila_imagenes

! Lista de ventanillas disponibles

module lista_ventanilla
    use pila_imagenes
    use cola_impresion
    implicit none
    private

    type, public :: nodo_ventanilla
        integer :: id
        type(PilaImagenes) :: pilaImagenes
        logical :: agregado = .false.
        type(nodo_ventanilla), pointer :: siguiente => null()
    end type nodo_ventanilla

    type, public :: listaVentanilla
        type(nodo_ventanilla), pointer :: head => null()
        contains
            procedure :: insertarVentanilla
            procedure :: llenarPilaImagenes
            procedure :: siguientePaso
            procedure :: disponibilidad
    end type listaVentanilla
    
contains
    subroutine insertarVentanilla(lista, cantidad)
        class(listaVentanilla), intent(inout) :: lista
        type(nodo_ventanilla), pointer :: nuevaVentanilla, actual
        integer, intent(in) :: cantidad
        integer :: i

        do i = 1, cantidad
            allocate(nuevaVentanilla)
            nuevaVentanilla%id = i
            nuevaVentanilla%siguiente => null() 

            if (associated(lista%head)) then
                actual => lista%head
                do while(associated(actual%siguiente))
                    actual => actual%siguiente
                end do
                actual%siguiente => nuevaVentanilla               
            else
                lista%head => nuevaVentanilla
            end if
            write(*,'(A, I0)') 'Se creo la ventanilla ', nuevaVentanilla%id
        end do
    end subroutine insertarVentanilla

    subroutine llenarPilaImagenes(lista, image)
        class(listaVentanilla), intent(inout) :: lista
        type(Imagen), intent(out) :: image
        type(nodo_ventanilla), pointer :: currentVentanilla
        character(len=12) :: ventanilla
    
        currentVentanilla => lista%head
    
        ! Iterar sobre las ventanillas hasta encontrar una que pueda recibir al cliente
        do while(associated(currentVentanilla))
            if (.not. currentVentanilla%agregado) then
                write(ventanilla,'(A, I0)') "Ventanilla ", currentVentanilla%id
                image%ventanilla = ventanilla
                call currentVentanilla%pilaImagenes%pushPila(image)
                currentVentanilla%agregado = .true.  ! Marcar que se ha agregado una imagen
                write(*,'(A, I0, A, I0)') 'EL CLIENTE ', image%id, ' INGRESA A LA VENTANILLA ', currentVentanilla%id
                exit  ! Salir del bucle una vez que se agrega la imagen
            else
                currentVentanilla => currentVentanilla%siguiente
            end if
        end do
    
        if (.not. associated(currentVentanilla)) then
            print *, "No se encontró una ventanilla disponible para agregar la imagen."
        end if
    end subroutine llenarPilaImagenes

    subroutine siguientePaso(lista,colaImagenG,colaImagenP,listaEspera)
        use Listclientes_espera
        class(listaVentanilla), intent(inout) :: lista
        type(ColaImpresion), intent(inout) ::  colaImagenG, colaImagenP
        type(Lista_espera), intent(inout) :: listaEspera
        type(nodo_ventanilla), pointer :: currentVentanilla
        logical :: estado
        
        currentVentanilla => lista%head 
        
        do while(associated(currentVentanilla))
            if (currentVentanilla%agregado) then
                estado = currentVentanilla%pilaImagenes%enviarImagen(colaImagenG,colaImagenP, listaEspera)
                print *, ''
                if (estado) then
                    currentVentanilla%agregado = .false.
                end if
            end if
            currentVentanilla => currentVentanilla%siguiente
        end do
    
    end subroutine siguientePaso

    function disponibilidad(lista) result(estado)
        class(listaVentanilla), intent(in) :: lista
        logical :: estado
        type(nodo_ventanilla), pointer :: current
    
        current => lista%head
        estado = .false.  
    
        ! Recorrer todos los nodos de la lista
        do while(associated(current))
            if (.not. current%agregado) then
                ! Encontramos una ventanilla que no esta ocupada
                estado = .true.
                exit  
            end if
            current => current%siguiente  
        end do
    end function disponibilidad
    
end module lista_ventanilla