module imagenes_modulo
    implicit none

    type, public :: Imagen_espera
        character(len=10) :: nombre
        integer :: id
    end type Imagen_espera

    type, public :: NodoImagen
        type(Imagen_espera) :: imagen
        type(NodoImagen), pointer :: siguiente => null()
    end type NodoImagen

    type, public :: ListaImagenes
        type(NodoImagen), pointer :: cabeza => null()
        contains
            procedure :: agregarImagen
    end type ListaImagenes

contains
    subroutine agregarImagen(lista, nuevaImagen)
        class(ListaImagenes), intent(inout) :: lista
        type(Imagen_espera), intent(in) :: nuevaImagen
        type(NodoImagen), pointer :: nuevoNodo, nodoActual

        allocate(nuevoNodo)
        nuevoNodo%imagen = nuevaImagen
        nuevoNodo%siguiente => null()

        if (.not. associated(lista%cabeza)) then
            ! Si la lista está vacía, el nuevo nodo se convierte en la cabeza de la lista.
            lista%cabeza => nuevoNodo
        else
            ! Si la lista no está vacía, recorrer hasta el final de la lista y agregar el nuevo nodo.
            nodoActual => lista%cabeza
            do while(associated(nodoActual%siguiente))
                nodoActual => nodoActual%siguiente
            end do
            nodoActual%siguiente => nuevoNodo
        end if
    end subroutine agregarImagen
end module imagenes_modulo


