!  AOD_B1.f90 
!
!  FUNCTIONS:
!  AOD_B1 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: AOD_B1
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

program AOD_1B

    implicit none

    real(kind = 8)     :: start1, finish1
    character(len = 10):: date1, time1, pretty_time1, flag
    character(len = 10):: date2, time2, pretty_time2 

    call date_and_time(date1, time1) 
    pretty_time1 = time1(1: 2) // ':' // time1(3: 4) // ':' // time1(5: 10) 
    write(*, *) 'Time starting: ', date1, pretty_time1 
    write(*, *)
    write(*, *)

    call cpu_time(start1) 
    call main_program() 
    call cpu_time(finish1)

    call date_and_time(date2, time2) 
    pretty_time2 = time2(1: 2) // ':' // time2(3: 4) // ':' // time2(5: 10)
    write(*, *) 'Time ending  : ', date2, pretty_time2
    write(*, *)
    write(*, *)

    write(*, *) "The running time indicated by CPU_TIME(s) is: ",&
                 finish1 - start1
    write(*, *)
    write(*, *)
    write(*, *) "Input q to quit:"
    read(*, *) flag
    if(trim(flag) == "q") stop

end program AOD_1B

subroutine main_program()
    implicit none

    !--------------------variables---------------------!
    integer(kind = 8)                :: degree
    integer(kind = 8)                :: days
    integer(kind = 8)                :: interval
    character(len = 100)             :: path_csfiles
    character(len = 100)             :: date_begin
    character(len = 100), allocatable:: aod_file(:)
    character(len = 100), allocatable:: cts_file(:)
    
    !----------------------------read parameters-----------------------------!
    call read_cmd(degree, path_csfiles, date_begin, days, interval)
    !--------------------------end read parameters---------------------------!

    allocate(aod_file(days + 1))     !   this is for storing aod_glo_file
    allocate(cts_file(days + 1))     !   this is for storing cts_file

    !--------------------------get the dates on the trail--------------------!
    call get_t_span(date_begin, path_csfiles, days, aod_file, cts_file)
    !------------------------end get the dates in the trail------------------!

    !----------------------------interpolation-------------------------------!
    call interpolation(aod_file, days, degree, interval, cts_file)
    !----------------------------end interpolation---------------------------!
    
    deallocate(aod_file)
    deallocate(cts_file)

end subroutine main_program

subroutine read_cmd(degree, path_csfiles, date_begin, days, interval)

    implicit none
    integer(kind = 8)      :: degree
    integer(kind = 8)      :: days
    integer(kind = 8)      :: interval
    character(len = 100)   :: path_csfiles
    character(len = 100)   :: date_begin
    character(len = 100)   :: temp

    open(89, file = 'cmd.txt')
    read(89, *) temp, degree
    read(89, *) temp, path_csfiles
    read(89, *) temp, date_begin
    read(89, *) temp, days
    read(89, *) temp, interval
    close(89)
end subroutine read_cmd

subroutine get_t_span(date_begin, path_csfiles, days, aod_file, cts_file)
    implicit none

    !----------------------variables------------------------!
    character(len = 100)                :: path_csfiles
    character(len = 100)                :: date_begin
    integer(kind = 8)                   :: i, i_begin_aod
    integer(kind = 8)                   :: i_begin_cts
    integer(kind = 8)                   :: days
    character(len = 100)                :: aod_file(days + 1)
    character(len = 100)                :: cts_file(days + 1)
    integer(kind = 8)                   :: row_aod
    integer(kind = 8), external         :: GetFileN
    character(len = 100)                :: temp
    integer(kind = 8)                   :: year, month, date
    integer(kind = 8)                   :: leap_year(12)
    integer(kind = 8)                   :: normal_year(12)
    character(len = 5)                  :: cts_date_temp
    character(len = 10)                 :: cts_date
    character(len = 100)                :: results_dayBYday(days)

    leap_year = (/0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335/)
    normal_year = (/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/)

    open(56, file = path_csfiles)
    row_aod = GetFileN(56)

    read(date_begin(1: 4), "(i4)") year
    read(date_begin(6: 7), "(i2)") month
    read(date_begin(9: 10), "(i2)") date

    if(mod(year, 4) == 0) then 
        if(leap_year(month) + date < 10) then
            write(cts_date_temp, "(i1)") leap_year(month) + date
            cts_date = trim("00" // cts_date_temp)
        else if( &
                (leap_year(month) + date < 100) &
                .and. &
                (leap_year(month) + date > 9)   &
                ) then
            write(cts_date_temp, "(i2)") leap_year(month) + date
            cts_date = trim("0" // cts_date_temp)
        else
            write(cts_date, "(i3)") leap_year(month) + date
        end if
    else
        if(normal_year(month) + date < 10) then
            write(cts_date_temp, "(i1)") normal_year(month) + date
            cts_date = trim("00" // cts_date_temp)
        else if( &
                (normal_year(month) + date < 100) &
                .and. &
                (normal_year(month) + date > 9)   &
                ) then
            write(cts_date_temp, "(i2)") normal_year(month) + date
            cts_date = trim("0" // cts_date_temp)
        else
            write(cts_date, "(i3)") normal_year(month) + date
        end if
    end if

    do i = 1, row_aod
        read(56, *) temp
        if(index(temp, ("AOD1B_" // trim(date_begin))) /= 0) then
            i_begin_aod = i
        end if

        if(index(temp, ("2004" // trim(cts_date))) /= 0) then
            i_begin_cts = i
        end if
    end do

    rewind(56)
    do i = 1, i_begin_aod - 1
        read(56, *) temp
    end do
    do i = 1, days + 1
        read(56, *) aod_file(i)
    end do

    rewind(56)
    do i = 1, i_begin_cts - 1
        read(56, *) temp
    end do
    do i = 1, days + 1
        read(56, *) cts_file(i)
    end do   

    close(56)
end subroutine get_t_span

integer(kind = 8) function GetFileN(iFileUnit)
    implicit none

    !---------------variables----------------!
    integer(kind = 8), intent(in):: iFileUnit
    character(len = 1)           :: cDummy
    integer(kind = 8)            :: ierr

    GetFileN = 0
    rewind(iFileUnit)
    do
        read(iFileUnit, *, ioStat = ierr) cDummy
        if (ierr /= 0) exit
        GetFileN = GetFileN + 1
    end do
    rewind(iFileUnit)
end function GetFileN

subroutine interpolation(aod_file, days, degree, interval, cts_file)
    implicit none

    !--------------------------variables---------------------------!
    integer(kind = 8)               :: i, j, k, h, l
    integer(kind = 8)               :: file_unit
    integer(kind = 8)               :: next_file_unit
    integer(kind = 8)               :: output_unit1
    integer(kind = 8)               :: output_unit2
    integer(kind = 8)               :: days
    integer(kind = 8)               :: degree
    integer(kind = 8)               :: interval
    integer(kind = 8)               :: row_aod
    integer(kind = 8)               :: row_next_aod
    integer(kind = 8)               :: row_cts
    integer(kind = 8)               :: row_next_cts
    integer(kind = 8)               :: row_glo(24 / interval + 1)
    integer(kind = 8)               :: former_k
    integer(kind = 8)               :: cts_k
    integer(kind = 8), external     :: GetFileN
    integer(kind = 8)               :: flag_glo1, flag_glo2
    integer(kind = 8)               :: gross
    integer(kind = 8)               :: sample_num, total_time
    integer(kind = 8)               :: cts_unit
    integer(kind = 8)               :: next_cts_unit
    integer(kind = 8)               :: row_epoch12
    integer(kind = 8)               :: ierr
    real(kind = 16)                 :: cs_former((degree + 2) &
                                       *(degree + 1) / 2, 2)
    real(kind = 16)                 :: cs_latter((degree + 2) &
                                       *(degree + 1) / 2, 2)
    real(kind = 16)                 :: c((degree + 2) &
                                       *(degree + 1) / 2)
    real(kind = 16)                 :: s((degree + 2) &
                                       *(degree + 1) / 2)
    real(kind = 16)                 :: gradient_c, gradient_s
    real(kind = 16)                 :: x, y, z, t, d, d1
    real(kind = 16)                 :: epoch12, temp_num
    character(len = 100)            :: temp, temp1
    character(len = 100)            :: aod_file(days + 1)
    character(len = 100)            :: cts_file(days + 1)
    character(len = 100)            :: results_dayBYday1(days)
    character(len = 100)            :: results_dayBYday2(days)

    !------------------constants----------------!
    file_unit = 17
    next_file_unit = 19
    output_unit1 = 78
    output_unit2 = 65
    cts_unit = 34
    next_cts_unit = 24
    gross = (degree + 1) * (degree + 2) / 2
    sample_num = 24 / interval + 1
    row_glo = 0
    epoch12 = 0.500592407407407407407
    !--------------end constants----------------!

    call creat_output_files1(days, results_dayBYday1, aod_file)
    call creat_output_files2(days, results_dayBYday2, aod_file)

    do i = 2, days + 1
        k = 1
        row_epoch12 = 1

        open(file_unit, file = aod_file(i - 1))
        open(next_file_unit, file = aod_file(i))
        open(cts_unit, file = cts_file(i - 1))
        open(next_cts_unit, file = cts_file(i))
        open(output_unit1, file = results_dayBYday1(i - 1))
        open(output_unit2, file = results_dayBYday2(i - 1))

        call header_p(output_unit1, degree)
        call header_a(output_unit2, degree)
        
        row_aod = GetFileN(file_unit)
        row_next_aod = GetFileN(next_file_unit)
        row_cts = GetFileN(cts_unit)
        row_next_cts = GetFileN(next_cts_unit)

        !----get the row which contains TYPE glo----!
        read(cts_unit, *) d1
        do l = 1, row_cts
            read(cts_unit, *) temp_num
            row_epoch12 = row_epoch12 + 1
            if(abs(temp_num - epoch12) < 1e-10) exit
        end do

        do j = 1, row_aod
            read(file_unit, "(a100)") temp
            flag_glo1 = index(temp, "TYPE glo")
            flag_glo2 = index(temp, "12:00:00")
            if(flag_glo1 /= 0 .and. flag_glo2 /= 0) then
                row_glo(k) = j
                k = k + 1
            end if
            if(k > 1 .and. flag_glo1 /= 0 .and. flag_glo2 == 0) then
                row_glo(k) = j
                former_k = k
                k = k + 1
            end if
        end do

        do j = 1, row_next_aod
            read(next_file_unit, "(a100)") temp
            flag_glo1 = index(temp, "TYPE glo")
            flag_glo2 = index(temp, "12:00:00")
            if(flag_glo1 /= 0) then
                row_glo(k) = j
                cts_k = k - 1
                k = k + 1
                if(flag_glo2 /= 0) exit
            end if
        end do
        !--end get the row which contains TYPE glo--!

        k = 1
        rewind(file_unit)
        rewind(next_file_unit)
        do j = 1, sample_num - 1
            !---get the c and s in two successive sampling points---!
            rewind(file_unit)
            rewind(next_file_unit)
            if(j < former_k) then
                do k = 1, row_glo(j)
                    read(file_unit, *)  
                end do

                do k = 1, gross
                    read(file_unit, *) & 
                    temp, temp1, cs_former(k, 1), cs_former(k, 2)
                end do

                do k = 1, row_glo(j + 1) - row_glo(j) - gross
                    read(file_unit, *) 
                end do

                do k = 1, gross
                    read(file_unit, *) &
                    temp, temp1, cs_latter(k, 1), cs_latter(k, 2)
                end do
            end if

            if(j == former_k) then
                do k = 1, row_glo(j)
                    read(file_unit, *) 
                end do

                do k = 1, gross
                    read(file_unit, *) &
                    temp, temp1, cs_former(k, 1), cs_former(k, 2)
                end do

                do k = 1, row_glo(j + 1)
                    read(next_file_unit, *)  
                end do

                do k = 1, gross
                    read(next_file_unit, *) & 
                    temp, temp1, cs_latter(k, 1), cs_latter(k, 2)
                end do
            end if

            if(j > former_k) then
                do k = 1, row_glo(j)
                    read(next_file_unit, *)  
                end do

                do k = 1, gross
                    read(next_file_unit, *) & 
                    temp, temp1, cs_former(k, 1), cs_former(k, 2)
                end do

                do k = 1, row_glo(j + 1) - row_glo(j) - gross
                    read(next_file_unit, *) 
                end do

                do k = 1, gross
                    read(next_file_unit, *) &
                    temp, temp1, cs_latter(k, 1), cs_latter(k, 2)
                end do
            end if
            !--end get the c and s in two successive sampling points-!

            !----------------interpolation, interval = 30s-----------!
            total_time = 3600 * interval / 30 
            do l = 1, 8
                read(next_cts_unit, *, iostat = ierr)
                if(ierr /= 0) exit
            end do
            do h = 0, total_time - 1
                if((j == sample_num - 1) .and. (h >= total_time / 3)) then   
                    read(next_cts_unit, *, iostat = ierr) d
                    read(next_cts_unit, *, iostat = ierr) t
                    read(next_cts_unit, *, iostat = ierr) x
                    read(next_cts_unit, *, iostat = ierr) y
                    read(next_cts_unit, *, iostat = ierr) z          
                    do k = 1, gross
                        gradient_c = (cs_latter(k, 1) - cs_former(k, 1))&
                         / 3600.0 * interval / 30.0
                        gradient_s = (cs_latter(k, 2) - cs_former(k, 2))&
                         / 3600.0 * interval / 30.0
                        c(k) = gradient_c * h + cs_former(k, 1)
                        s(k) = gradient_s * h + cs_former(k, 2)
                    end do
                    call CalculateT(&
                                    x, y, z, degree, gross,&
                                    c, s, d, t, output_unit1&
                                    ) 
                    call AccXyz(&
                                 x, y, z, degree, gross,&
                                 c, s, d, t, output_unit2&
                                )               
                    read(next_cts_unit, *, iostat = ierr)
                    read(next_cts_unit, *, iostat = ierr)
                    read(next_cts_unit, *, iostat = ierr)
                else  
                    read(cts_unit, *, iostat = ierr) x
                    if (ierr /= 0) exit
                    read(cts_unit, *, iostat = ierr) y
                    if (ierr /= 0) exit
                    read(cts_unit, *, iostat = ierr) z
                    do k = 1, gross
                        gradient_c = (cs_latter(k, 1) - cs_former(k, 1))&
                         / 3600.0 * interval / 30.0
                        gradient_s = (cs_latter(k, 2) - cs_former(k, 2))&
                         / 3600.0 * interval / 30.0
                        c(k) = gradient_c * h + cs_former(k, 1)
                        s(k) = gradient_s * h + cs_former(k, 2)
                    end do
                    if(h == 0 .and. j == 1) then
                        d = d1
                        t = epoch12
                    end if
                    call CalculateT(&
                                    x, y, z, degree, gross,&
                                    c, s, d, t, output_unit1&
                                    ) 
                    call AccXyz(&
                                 x, y, z, degree, gross,&
                                 c, s, d, t, output_unit2&
                                )
                    if (ierr /= 0) exit                
                    read(cts_unit, *, iostat = ierr)
                    if (ierr /= 0) exit
                    read(cts_unit, *, iostat = ierr)
                    if (ierr /= 0) exit
                    read(cts_unit, *, iostat = ierr)
                    if (ierr /= 0) exit
                    read(cts_unit, *, iostat = ierr) d
                    if (ierr /= 0) exit
                    read(cts_unit, *, iostat = ierr) t
                    if (ierr /= 0) exit
                end if
            end do
            !------------------end interpolation---------------------!
            rewind(file_unit)
            rewind(next_file_unit)
        end do

        close(file_unit)
        close(next_file_unit)
        close(output_unit1)
        close(output_unit2)
        close(cts_unit)
        close(next_cts_unit)
    end do
end subroutine interpolation

subroutine creat_output_files1(days, results_dayBYday, aod_file)
    implicit none
    !--------------------varaibles--------------------!
    integer(kind = 8)         :: days, year, month, date
    integer(kind = 8)         :: i
    character(len = 100)      :: results_dayBYday(days)
    character(len = 100)      :: aod_file(days)
    character(len = 100)      :: cyear, cmonth, cdate


    do i = 1, days
        results_dayBYday(i) = &
        "AOD_B1_POTENTIAL_AUGMENTATION_" //       &
        aod_file(i)((len_trim(aod_file(i)) - 18)  &
        : (len_trim(aod_file(i)) - 15)) // "_" // &
        aod_file(i)((len_trim(aod_file(i)) - 13)  &
        : (len_trim(aod_file(i)) - 12)) // "_" // &
        aod_file(i)((len_trim(aod_file(i)) - 10)  &
        : (len_trim(aod_file(i)) - 9)) // ".txt"
    end do
end subroutine creat_output_files1

subroutine creat_output_files2(days, results_dayBYday, aod_file)
    implicit none
    !--------------------varaibles--------------------!
    integer(kind = 8)         :: days, year, month, date
    integer(kind = 8)         :: i
    character(len = 100)      :: results_dayBYday(days)
    character(len = 100)      :: aod_file(days)
    character(len = 100)      :: cyear, cmonth, cdate


    do i = 1, days
        results_dayBYday(i) = &
        "AOD_B1_ACCELERATION_AUGMENTATION_" //       &
        aod_file(i)((len_trim(aod_file(i)) - 18)  &
        : (len_trim(aod_file(i)) - 15)) // "_" // &
        aod_file(i)((len_trim(aod_file(i)) - 13)  &
        : (len_trim(aod_file(i)) - 12)) // "_" // &
        aod_file(i)((len_trim(aod_file(i)) - 10)  &
        : (len_trim(aod_file(i)) - 9)) // ".txt"
    end do
end subroutine creat_output_files2

subroutine CalculateT(x, y, z, degree, gross, c, s, d, t, output_unit1)
    implicit none
    !----------------variables--------------------!
    real(kind = 16)         :: V, MU
    real(kind = 16)         :: REarth, d, t
    real(kind = 16)         :: x, y, z, R
    integer(kind = 8)       :: i, j, degree, gross
    integer(kind = 8)       :: flag, output_unit1
    real(kind = 16)         :: c(gross), s(gross)
    real(kind = 16)         :: e(0: degree, 0: degree)
    real(kind = 16)         :: f(0: degree, 0: degree)
    
    R = sqrt(x**2.0 + y**2.0 + z**2.0)
        REarth = 6.3781363E+6
        MU = 0.3986004415E+15

    call CalculateEF(x, y, z, REarth, degree, E, F)

    V = 0
    flag = 1
    do i = 0, degree
        do j = 0, i
            V = V + E(i, j) * C(flag) + F(i, j) * S(flag)
            flag = flag + 1
        end do
    end do

    V = V * MU / REarth
    write(output_unit1, 200) d, t, v
    200 format(f10.1, 2f)
end subroutine

subroutine CalculateEF(x, y, z, REarth, n, E, F)
    implicit none
    !-----------------variables---------------------------!
    integer(kind = 8)       :: n
    real(kind = 16)         :: x, y, z, REarth
    real(kind = 16)         :: E(0: n, 0: n), F(0: n, 0: n)
    integer(kind = 8)       :: i,j
    real(kind = 16)         :: r
    real(kind = 16)         :: coe1,coe2

    r = sqrt(x**2 + y**2 + z**2)
    E(0, 0) = REarth / r
    E(1, 0) = sqrt(3.0) * z * (REarth**2) / (r**3)
    E(1, 1) = sqrt(3.0) * x * (REarth**2) / (r**3)
    F(0, 0) = 0
    F(1, 0) = 0
    F(1, 1) = sqrt(3.0) * y * (REarth**2) / (r**3)

    do i = 2, n
        do j = 0, i
            if(i == j) then
                coe1 = sqrt(real(2 * j + 1) / (2 * j))

                E(i, j) = coe1 * (x * REarth * E(i - 1, j - 1) &
                / (r**2) - y * REarth * F(i - 1, j - 1) / (r**2))

                F(i, j) = coe1 * (x * REarth * F(i - 1, j - 1) &
                / (r**2) + y * REarth * E(i - 1, j - 1) / (r**2))
            else
                coe1 = sqrt(real(2 * i + 1) * (2 * i - 1) &
                / ((i - j) * (i + j)))

                coe2 = sqrt((real(2 * i + 1) * (i - j - 1) &
                * (i + j - 1)) / ((i - j) * (i + j) * (2 * i - 3)))

                E(i, j) = coe1 * z * REarth * E(i - 1, j) & 
                / (r**2) - coe2 * (REarth**2) * E(i - 2, j) / (r**2)

                F(i, j) = coe1 * z * REarth * F(i - 1, j) &
                / (r**2) - coe2 * (REarth**2) * F(i - 2, j) / (r**2)
            end if
        end do
    end do
end subroutine

subroutine header_p(output_unit1, degree)
    implicit none

    integer(kind = 8)  :: output_unit1, degree
    character(len = 10):: date1, time1, pretty_time1

    call date_and_time(date1, time1) 
    pretty_time1 = time1(1: 2) // ':' // time1(3: 4) // ':' // time1(5: 10) 
    write(output_unit1, *) "Programmed by:  ", "Haosi Li"
    write(output_unit1, *) "Product time :  ", date1, pretty_time1
    write(output_unit1, *) "Max degree   :  ", degree
    write(output_unit1, *) "First row    :  ", "Day(Julian day)"
    write(output_unit1, *) "Second row   :  ", "Second(Julian day) "
    write(output_unit1, *) "Third row    :  ", "Augmentation of potential"
    write(output_unit1, *) "END OF HEADER"

end subroutine header_p

subroutine header_a(output_unit2, degree)
    implicit none

    integer(kind = 8)  :: output_unit2, degree
    character(len = 10):: date1, time1, pretty_time1

    call date_and_time(date1, time1) 
    pretty_time1 = time1(1: 2) // ':' // time1(3: 4) // ':' // time1(5: 10) 
    write(output_unit2, *) "Programmed by:  ", "Haosi Li"
    write(output_unit2, *) "Product time :  ", date1, pretty_time1
    write(output_unit2, *) "Max degree   :  ", degree
    write(output_unit2, *) "First row    :  ", "Day(Julian day)"
    write(output_unit2, *) "Second row   :  ", "Second(Julian day) "
    write(output_unit2, *) "Third row    :  ", "Augmentation of acceleration"
    write(output_unit2, *) "END OF HEADER"

end subroutine header_a

subroutine AccXyz(x, y, z, degree, gross, c, s, d, t, output_unit2) 
    implicit None 
    
    real(kind = 16)          :: AccXx, AccYy, AccZz, AccXS, AccYS, AccZS
    integer(kind = 8)        :: i, j, flag
    real(kind = 16)          :: x, y, z
    real(kind = 16)          :: MU, REarth
    real(kind = 16), external:: CalBi
    integer(kind = 8)        :: degree, gross, output_unit2
    real(kind = 16)          :: d, t
    real(kind = 16)          :: c(gross), s(gross)
    real(kind = 16)          :: e(0: degree + 1, 0: degree + 1)
    real(kind = 16)          :: f(0: degree + 1, 0: degree + 1)

    REarth = 6.3781363E+6
    MU = 0.3986004415E+15

    E = 0
    F = 0
    flag = 1

    call CalculateEF(x, y, z, REarth, degree, E, F)

    AccXS = 0
    AccXx = 0
    AccYS = 0
    AccYy = 0
    AccZz = 0
    AccZS = 0
    do i = 0, degree
        do j = 0, i
            if(j == 0) then
                AccXx = MU / REarth**2 * (-CalBi(i, j, 1)) * E(i + 1, 1) * C(flag)
                AccYy = MU / REarth**2 * (-CalBi(i, j, 1)) * F(i + 1, 1) * C(flag)
            else if(j > 0) then
                AccXx = MU / (2D0 * REarth**2)        & 
                * (CalBi(i, j, 2) * (-E(i + 1, j + 1) &
                * C(flag) - F(i + 1, j + 1) * S(flag))&
                + CalBi(i, j, 3) * (E(i + 1, j - 1)   & 
                * C(flag) + F(i + 1, j - 1) * S(flag)))
                AccYy = MU / 2.0 / REarth**2          &
                * (CalBi(i, j, 2) * (-F(i + 1, j + 1) &
                * C(flag) + E(i + 1, j + 1) * S(flag))&
                + CalBi(i, j, 3) * (-F(i + 1, j - 1)  &
                * c(flag) + E(i + 1, j - 1) * S(flag)))
            end if
            AccZz = MU / REarth**2 * (CalBi(i, j, 4) *&
             (-E(i + 1, j) * C(flag) - F(i + 1, j) * S(flag)))
            AccXS = AccXS + AccXx
            AccYS = AccYS + AccYy
            AccZS = AccZS + AccZz
            flag = flag + 1
        end do
    end do
    write(output_unit2, 300) d, t, sqrt(AccXS**2 + AccYS**2 + AccZS**2)
    300 format(f10.1, 2f)
end subroutine

real(kind = 16) function CalBi(i, j, m)
    implicit none
    integer(kind = 8):: i, j, m
    real(kind = 16)  :: fac
    if(m == 1) then
        fac = real((i + 1) * (i + 2) * (2 * i + 1))
        CalBi = sqrt(fac / 2.0 / (2 * i + 3))
    else if(m == 2) then
        fac = real((i + j + 1) * (i + j + 2) * (2 * i + 1))
        CalBi = sqrt(fac / (2 * i + 3))
    else if(m == 3) then
        fac = real((i - j + 1) * (i - j + 2) * (2 * i + 1))
        CalBi=sqrt(fac/(2*i+3))
    else if(m == 4) then
        fac = real((i - j + 1) * (i + j + 1) * (2 * i + 1))
        CalBi = sqrt(fac / (2 * i + 3))
    else
        write(*,*) 'mֵ is not correct: ', m
    end if

end function CalBi
