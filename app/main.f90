program inicio
    use cola_clientes
    use pasos
    use lista_ventanilla
    use cola_impresion
    use pila_imagenes
    use Listclientes_espera
    use lista_atendidos
    implicit none
    integer :: opcion, cantidad, opcion2
    character(len=1) :: op
    character(len=100) :: rutaArchivo
    type(Cola) :: miColaDeClientes
    type(listaVentanilla) :: milistaVentanilla
    type(ColaImpresion) :: impresoraGrande
    type(ColaImpresion) :: impresoraSmall
    type(Cliente) :: clienteTemp
    type(Imagen) :: imagenTemp
    logical :: disponibilidad, ClientesContenido
    type(Lista_espera) :: listaEspera
    type(ListaClientesAtendidos) :: listAtendidos 

    disponibilidad = .true.
    
    do
        print *,'1. Parametros Iniciales'  
        print *,"2. Ejecutar Paso"  
        print *,"3. Estado en memoria de las estructuras"  
        print *,"4. Reportes"  
        print *,"5. Datos del estudiante"  
        print *,"6. Salir"  
        print *,"Selecciona una opcion por su numero"  

        read(*,*) opcion

        select case (opcion)
            case (1)
                print *,"a. Carga Masiva de Clientes"
                print *,"b. Cantidad de ventanillas"
                read(*,*) op
                    select case (op)
                        case ("a")
                            print *, "Ingrese la ruta del archivo JSON con los clientes:"
                            read(*,*) rutaArchivo
                            call miColaDeClientes%cargarClientes(rutaArchivo)
                            call miColaDeClientes%mostrar_cola()
                        case ("b")
                            print *, "Ingrese la cantidad de ventanillas que habra para atender"
                            read(*,*) cantidad
                            call milistaVentanilla%insertarVentanilla(cantidad)
                        case default
                            print *, "Seleccione una opción valida"
                        end select
            case (2)
                print *, ''
                call miColaDeClientes%agregarRandom()
                write(*,'(A, I0, A)') '---------------- PASO ',paso, ' ----------------'
                print *, "----------------------------------------"
                print *, "PRODUCCION IMPRESORA GRANDE:"
                call impresoraGrande%produccion(listaEspera, listAtendidos)
                print *, "----------------------------------"
                print *, "PRODUCCION IMPRESORA PEQUENA: "
                call impresoraSmall%produccion(listaEspera, listAtendidos)
                print *, "----------------------------------"
                call milistaVentanilla%siguientePaso(impresoraGrande,impresoraSmall, listaEspera)            
                ClientesContenido = miColaDeClientes%contenido()
                if (disponibilidad .and. ClientesContenido) then
                    clienteTemp = miColaDeClientes%pop()
                    imagenTemp%id = clienteTemp%id
                    imagenTemp%nombre = clienteTemp%nombre
                    imagenTemp%img_g = clienteTemp%img_g
                    imagenTemp%img_gTemp = clienteTemp%img_g               
                    imagenTemp%img_p = clienteTemp%img_p
                    imagenTemp%img_pTemp = clienteTemp%img_p  
                    imagenTemp%pasoInicial = paso
                    call milistaVentanilla%llenarPilaImagenes(imagenTemp)
                end if 
                disponibilidad = milistaVentanilla%disponibilidad()
                print *, "----------------------------------------"
                print *, ''
                paso = paso + 1
            case (3)
                call miColaDeClientes%graficar_cola('cola_clientes.dot') 
            case (4)
                print *,'1. Top 5 de clientes con mayor cantidad de imagenes grandes'  
                print *,"2. Top 5 de clientes con menor cantidad de imagenes pequenas"  
                print *,"3. Informacion del cliente que mas pasos estuvo en el sistema."  
                print *,"4. Datos de un cliente en especifico" 
                read (*,*) opcion2
            case (5)
                print *, ''
                print *, "Ingenieria en Ciencias y Sistemas"
                print *, "Estrcuturas de Datos Seccion C"
                print *, ''
            case (6)
                print *,"Se ha detenido la ejecucion del programa"
                stop
            case default
                print *, "Selecciona algun valor que este en el menu"
        end select
    end do  
end program inicio
