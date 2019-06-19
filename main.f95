!!----------------------------------------
!!
!!----------------------------------------

program CartesianCoordinateLegendre
    implicit none
    integer(kind = 4):: degree
    character*80     :: outputFile

    call inputParams(degree, outputFile)
    call CartesianLegedre(degree, outputFile)
end program CartesianCoordinateLegendre

subroutine inputParams(degree, outputFile)
    implicit none
    integer(kind = 4):: degree
    character*80     :: outputFile, temp_string

    open(35, file = 'Parametre.txt')
    read(35, *) temp_string, degree
    read(35, *) temp_string, outputFile
    close(35)
end subroutine inputParams

subroutine CartesianLegedre(degree, outputFile)
    implicit none
    integer(kind = 4):: degree
    character*80     :: outputFile
    real(kind = 16)  :: pi = 4 * atan(1.000000)
    real(kind = 16)  :: radius
    real(kind = 16)  :: k
    real(kind = 16)  :: normalisedCoefficient
    real(kind = 16)  :: upper_k
    integer(kind = 2):: i, j
    real(kind = 16)  :: p, pn
    real(kind = 16)  :: x, y, z
    real(kind = 16)  :: factorial
    real(kind = 16)  :: i_flag
    real(kind = 16)  :: temp
    real(kind = 16)  :: two_with_i

    write(*, *) 'Already set x y z'
    x = 1.0000
    y = sqrt(real(2, kind=16))
    z = 1.000
    open(98, file = outputFile, status = 'unknown')
    radius = sqrt(x**2 + y**2 + z**2)
    do i = 0, degree
        two_with_i = 1
        !---------------------------------!
        !            1/(2 ^ i)
        !---------------------------------!
        if (i == 0) then
            two_with_i = 1
        else
            do i_flag = 1, i
                two_with_i = two_with_i / 2
            end do
        end if
        do j = 0, i
            write(*, *) 'degree, frequency'
            write(*, *) i, j
            upper_k = floor((i - j)/2.0)
            p = 0
            !!---------------------------------------------
            !!           Normalisation
            !!---------------------------------------------
            normalisedCoefficient = sqrt((4*i + 2)       &
                                        *                &
                                        factorial(i - j) &
                                        /                &
                                        factorial(i + j))&
                                        / 2
            do k = 0, upper_k
                temp = factorial(2*i - 2*k) / factorial(k)  &
                / factorial(i - k) / factorial(i - j - 2*k) &
                * radius**(2*k - 2*i - 1) * z**(i - j - 2*k)&
                * two_with_i
                p = p + temp
            end do !k
            pn = p * normalisedCoefficient
            write(98, *) pn
        end do !j
    end do !i
    close(98)
end subroutine CartesianLegedre

function factorial(n)
    implicit none
    real(kind = 16) factorial
    integer(kind = 4) i
    integer(kind = 4) n

    factorial = 1.0D+00

    do i = 1, n
        factorial = factorial * real(i, kind=16)
    end do

    return
end function factorial
