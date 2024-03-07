module cola_impresion
    implicit none
    private 

    type, public :: Imagen_imp
        character(len=100) :: nombre
        integer :: id, id_Cliente
        integer :: tipo  ! 0 para pequeña, 1 para grande
    end type Imagen_imp

    type, public :: NodoCola
        type(Imagen_imp) :: imagenImp
        type(NodoCola), pointer :: siguiente => null()
    end type NodoCola

    type, public :: ColaImpresion
        type(NodoCola), pointer :: head => null()
        type(NodoCola), pointer :: final => null()
        integer :: ultimoId = 1  ! Contador para mantener el último id utilizado
        integer :: contadorProduccion = 0
        contains
            procedure :: encolarImpresion
            procedure :: desencolarImpresion
            procedure, pass :: obtenerUltimoId => getUltimoId 
            procedure :: produccion
            procedure :: imprimirCola
            procedure :: graficar_colasImpresion
    end type ColaImpresion

contains

    subroutine encolarImpresion(this_cola, imagen)       
        class(ColaImpresion), intent(inout) :: this_cola
        type(Imagen_imp), intent(in) :: imagen
        type(NodoCola), pointer :: nuevoNodo
        allocate(nuevoNodo)
        this_cola%ultimoId = this_cola%ultimoId + 1  ! Incrementar el contador de id
        nuevoNodo%imagenImp = imagen
        nuevoNodo%imagenImp%id = this_cola%ultimoId  ! Asignar el nuevo id a la imagen
        nuevoNodo%siguiente => null()

        if (.not. associated(this_cola%head)) then
            this_cola%head => nuevoNodo
            this_cola%final => nuevoNodo
        else
            this_cola%final%siguiente => nuevoNodo
            this_cola%final => nuevoNodo
        end if
    end subroutine encolarImpresion

    subroutine desencolarImpresion(this_cola)
        class(ColaImpresion), intent(inout) :: this_cola
        type(NodoCola), pointer :: nodoTemp
        
        ! Verificar si la cola está vacía
        if (.not. associated(this_cola%head)) then
            print *, "La cola está vacía. No se puede desencolar."
            return
        end if
        
        nodoTemp => this_cola%head
        this_cola%head => this_cola%head%siguiente
        
        ! Si head es ahora null, entonces la cola está vacía y final también debe ser null
        if (.not. associated(this_cola%head)) then
            this_cola%final => null()
        end if
        
        deallocate(nodoTemp)
    end subroutine desencolarImpresion
    

    function getUltimoId(this_cola) result(ultimoId)
        class(ColaImpresion), intent(in) :: this_cola
        integer :: ultimoId
        ultimoId = this_cola%ultimoId
    end function getUltimoId

    subroutine produccion(this_cola, listaEspera, listAtendidos)
        use Listclientes_espera
        use lista_atendidos
        class(ColaImpresion), intent(inout) :: this_cola
        type(Lista_espera), intent(inout) :: listaEspera
        type(ListaClientesAtendidos), intent(inout) :: listAtendidos
        type(Imagen_espera) :: imagenEspera
        type(NodoCola), pointer :: nodoActual
        logical :: esPrimeraLlamada
            
        nodoActual => this_cola%head
        
        if (associated(nodoActual)) then
            this_cola%contadorProduccion = this_cola%contadorProduccion + 1
            esPrimeraLlamada = (this_cola%contadorProduccion == 2) 
            if (nodoActual%imagenImp%tipo == 0) then
                ! Eliminar el nodo actual de la cola
                write(*,'(A, I0, A, A)') 'SALIO DE LA COLA UNA IMAGEN PEQUENA DEL CLIENTE: ', &
                nodoActual%imagenImp%id_Cliente, '-', nodoActual%imagenImp%nombre
                call this_cola%desencolarImpresion()
                imagenEspera%nombre = "IMG_P"
                imagenEspera%id = 1
                call listaEspera%addImagenCliente(nodoActual%imagenImp%id_Cliente,imagenEspera, listAtendidos)

            elseif (nodoActual%imagenImp%tipo == 1 .and. esPrimeraLlamada) then
                write(*,'(A, I0, A, A)') 'SALIO DE LA COLA UNA IMAGEN GRANDE DEL CLIENTE: ', &
                nodoActual%imagenImp%id_Cliente, '-', nodoActual%imagenImp%nombre
                call this_cola%desencolarImpresion()          
                imagenEspera%nombre = "IMG_G"
                imagenEspera%id = 1 
                call listaEspera%addImagenCliente(nodoActual%imagenImp%id_Cliente, imagenEspera, listAtendidos)
            end if
        end if
    
        if (esPrimeraLlamada) then
            this_cola%contadorProduccion = 0  ! Restablecer el contador después de la 2ª llamada
        end if
    end subroutine produccion
    
    subroutine imprimirCola(this_cola)
        class(ColaImpresion), intent(in) :: this_cola
        type(NodoCola), pointer :: nodoActual
        character(len=8) :: tipoImagen 
    
        nodoActual => this_cola%head 
    
        if (.not. associated(nodoActual)) then
            print *, "La cola está vacía."
            return
        end if
    
        print *, "Contenido de la Cola:"
        do while(associated(nodoActual))
            ! Determinar el tipo de imagen
            if (nodoActual%imagenImp%tipo == 0) then
                tipoImagen = "Pequeña"
            else
                tipoImagen = "Grande"
            end if

            print *, "ID Cliente:", nodoActual%imagenImp%id_Cliente, & 
            "Nombre:", trim(nodoActual%imagenImp%nombre), "Tipo:", tipoImagen
    
            nodoActual => nodoActual%siguiente
        end do
    end subroutine imprimirCola

    subroutine graficar_colasImpresion(this_cola, cola_pequena, filename)
        class(ColaImpresion), intent(in) :: this_cola
        type(ColaImpresion), intent(in) :: cola_pequena
        character(len=*), intent(in) :: filename
        type(NodoCola), pointer :: nodoActualGrande, nodoActualPeque
        integer :: fileUnit, iostat, contador1, contador2   
        character(len=256) :: dotPath, pngPath    
    
    end subroutine graficar_colasImpresion
    
    
end module cola_impresion