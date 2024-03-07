module Listclientes_espera
    use imagenes_modulo
    implicit none

    type, public :: clientesEspera 
        character(len=100) :: nombreCliente
        character(len=12) :: ventanilla
        integer :: id, img_g, img_p, pasoInicial
    end type clientesEspera

    type, public :: ClienteNodo
        type(clientesEspera) :: cliente
        type(ListaImagenes) :: listImagenes
        integer :: contImg_g = 0
        integer :: contImg_p = 0
        type(ClienteNodo), pointer :: anterior => null()
        type(ClienteNodo), pointer :: siguiente => null()
    end type ClienteNodo

    type, public :: Lista_espera
        type(ClienteNodo), pointer :: head => null()
        type(ClienteNodo), pointer :: cola => null()
        contains
            procedure :: insertarCliente 
            procedure :: eliminarCliente
            procedure :: addImagenCliente           
            procedure :: graficar_listaEspera  
            procedure :: imprimirLista        
    end type Lista_espera

contains
    subroutine insertarCliente(lista, Cliente)
        class(Lista_espera), intent(inout) :: lista
        type(clientesEspera), intent(in) :: Cliente
        type(ClienteNodo), pointer :: nuevoCliente

        allocate(nuevoCliente)
        nuevoCliente%cliente = Cliente

        if (.not. associated(lista%head)) then
            ! La lista está vacía
            lista%head => nuevoCliente
            lista%cola => nuevoCliente
            nuevoCliente%siguiente => nuevoCliente  ! Circularidad
            nuevoCliente%anterior => nuevoCliente  ! Circularidad
        else
            ! Insertar al final 
            nuevoCliente%siguiente => lista%head
            nuevoCliente%anterior => lista%cola
            lista%head%anterior => nuevoCliente
            lista%cola%siguiente => nuevoCliente
            lista%cola => nuevoCliente
        end if

        write(*,'(A, A, A)') 'EL CLIENTE ',trim(nuevoCliente%cliente%nombreCliente),' HA SIDO TRANSFERIDO A LA LISTA DE ESPERA'
    end subroutine insertarCliente


    subroutine eliminarCliente(lista, idCliente)
        class(Lista_espera), intent(inout) :: lista
        integer, intent(in) :: idCliente
        type(ClienteNodo), pointer :: nodoActual, temporal
    
        if (.not. associated(lista%head)) then
            print *, "La lista está vacía."
            return
        end if
    
        nodoActual => lista%head
        do while (associated(nodoActual))
            if (nodoActual%cliente%id == idCliente) then
                if (associated(nodoActual%siguiente)) nodoActual%siguiente%anterior => nodoActual%anterior
                if (associated(nodoActual%anterior)) nodoActual%anterior%siguiente => nodoActual%siguiente
                if (associated(nodoActual,lista%head)) then
                    lista%head => nodoActual%siguiente
                    if (associated(lista%head,nodoActual)) lista%head => null()
                    if (associated(lista%cola)) lista%cola%siguiente => lista%head
                    if (associated(lista%head)) lista%head%anterior => lista%cola
                end if   
                if (associated(nodoActual,lista%cola)) then
                    lista%cola => nodoActual%anterior
                    if (associated(lista%cola,nodoActual)) lista%cola => null()
                    if (associated(lista%head)) lista%head%anterior => lista%cola
                    if (associated(lista%cola)) lista%cola%siguiente => lista%head
                end if
                deallocate(nodoActual)
                return
            end if
            temporal => nodoActual
            nodoActual => nodoActual%siguiente
    
            if(associated(temporal,lista%cola)) exit
        end do
    end subroutine eliminarCliente 

    subroutine addImagenCliente(listaClientes, idCliente, nuevaImagen, listAtendidos)
        use pasos
        use lista_atendidos
        class(Lista_espera), intent(inout) :: listaClientes
        integer, intent(in) :: idCliente
        type(Imagen_espera), intent(in) :: nuevaImagen
        type(ClienteNodo), pointer :: nodoActual
        type(ListaClientesAtendidos), intent(inout) :: listAtendidos
        integer :: pasoTotal
    
        ! Buscar el cliente por ID
        nodoActual => listaClientes%head
        do while (associated(nodoActual))
            if (nodoActual%cliente%id == idCliente) then
                call nodoActual%listImagenes%agregarImagen(nuevaImagen)

                if (nuevaImagen%nombre == "IMG_P") then
                    nodoActual%contImg_p = nodoActual%contImg_p + 1
                    write(*,'(A,A,A,I0,A)') "CANTIDAD DE IMAGENES PEQUENAS DEL CLIENTE ", &
                    trim(nodoActual%cliente%nombreCliente), " - ", nodoActual%contImg_p, ' imagen(es)'
                elseif (nuevaImagen%nombre == "IMG_G") then
                    nodoActual%contImg_g = nodoActual%contImg_g + 1
                    write(*,'(A,A,A,I0)') "CANTIDAD DE IMAGENES GRANDES DEL CLIENTE ", & 
                    trim(nodoActual%cliente%nombreCliente), " - ",nodoActual%contImg_g
                end if

                ! Conteo para ver si el cliente ya tiene todas sus imagenes
                if (nodoActual%contImg_g == nodoActual%cliente%img_g .and. &
                nodoActual%contImg_p == nodoActual%cliente%img_p) then
                    write (*,'(A,A,A)') 'EL CLIENTE ', trim(nodoActual%cliente%nombreCliente),' YA TIENE TODAS SUS IMAGENES'
                    pasoTotal = get_paso() - nodoActual%cliente%pasoInicial
                    call listAtendidos%agregarCliente(nodoActual%cliente%id, nodoActual%cliente%nombreCliente, &
                    nodoActual%cliente%ventanilla, nodoActual%cliente%img_g,nodoActual%cliente%img_p, pasoTotal)
                    call listaClientes%eliminarCliente(idCliente)
                    print *, "-------SE EJECUTO EL PROCESO CON EXITO-------"
                end if
            end if
            nodoActual => nodoActual%siguiente
            if (associated(nodoActual,listaClientes%head)) exit
        end do
    end subroutine addImagenCliente  

    subroutine graficar_listaEspera(lista, filename)
        class(Lista_espera), intent(in) :: lista
        character(len=*), intent(in) :: filename
        type(ClienteNodo), pointer :: nodoActual
        type(NodoImagen), pointer :: nodoImgActual
        integer :: fileUnit, iostat, contador, contadorImg
        character(len=256) :: dotPath, pngPath

        dotPath = 'dot/' // trim(filename)
        pngPath = 'img/' // trim(adjustl(filename)) // '.png'
    
        open(newunit=fileUnit, file=dotPath, status='replace', iostat=iostat)
        if (iostat /= 0) then
            print *, "Error al abrir el archivo."
            return
        end if
    
        write(fileUnit, *) "digraph impresiones {"
        write(fileUnit, *) "    rankdir=LR;"
        write(fileUnit, *) "    node [shape=record];"
    
    end subroutine graficar_listaEspera

    subroutine imprimirLista(lista)
        class(Lista_espera), intent(in) :: lista
        type(ClienteNodo), pointer :: nodoActual
        type(NodoImagen), pointer :: nodoImgActual
        logical :: esPrimero
    
        if (.not. associated(lista%head)) then
            print *, "La lista está vacía."
            return
        end if
    
        nodoActual => lista%head
        esPrimero = .true.
        do while(associated(nodoActual) .and. (esPrimero .or. .not. associated(nodoActual, lista%head)))
            print *, "Cliente ID: ", nodoActual%cliente%id, " - Nombre: ", trim(nodoActual%cliente%nombreCliente), &
            " - Ventanilla: ", trim(nodoActual%cliente%ventanilla), " - Img_G: ", nodoActual%cliente%img_g, &
            " - Img_P: ", nodoActual%cliente%img_p, " - Paso Inicial: ", nodoActual%cliente%pasoInicial
    
            if (associated(nodoActual%listImagenes%cabeza)) then
                print *, "    Imágenes asociadas:"
                nodoImgActual => nodoActual%listImagenes%cabeza
                do while(associated(nodoImgActual))
                    print *, "        ID: ", nodoImgActual%imagen%id, " - Nombre: ", trim(nodoImgActual%imagen%nombre)
                    nodoImgActual => nodoImgActual%siguiente
                end do
            else
                print *, "    No hay imágenes asociadas."
            end if
    
            nodoActual => nodoActual%siguiente
            esPrimero = .false.
        end do
    end subroutine imprimirLista
    
end module Listclientes_espera