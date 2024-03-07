module pasos
    implicit none
    integer :: paso = 1

contains
    function get_paso() result(pasoActual)
        integer :: pasoActual
        pasoActual = paso
    end function get_paso

end module pasos 