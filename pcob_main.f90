program pcob_main
    ! 天体の視位置を求める

    implicit none
    
    character(7), parameter :: CELESTIA(6) = (/'Moon   ', 'Sun    ', 'Venus  ', 'Mars   ', 'Jupiter', 'Saturn '/)

    double precision, dimension(6) :: pos
    double precision :: year, month, day, hour, minute, second
    double precision :: rs, ds, ss
    integer :: rh, rm, dh, dm, sm
    character(1) :: sign
    character(132) :: sfmt

    double precision :: dy = 0.0d0
    double precision :: dt = 0.0d0
    integer :: planet_number = 0

    write(*, *)
    write(*, fmt='(a)', advance='no') 'DATE AND TIME(JST) ? '
    read(*, *) dy, dt
    write(*, fmt='(a)', advance='no') '(1: Moon, 2: Sun, 3:Venus, 4: Mars, 5: Jupiter, 6: Saturn) ? '
    read(*, *) planet_number

    if ((planet_number < 1) .or. (planet_number > 6)) then
        stop 'Bad Planet Number!'
    end if

    ! 年月日および時刻を抽出する
    year = int(dy / 10000.0d0)
    month = mod(int(dy / 100.0d0), 100)
    day = mod(dy, 100.0d0)
    hour = int(dt / 10000.0d0)
    minute = mod(int(dt / 100.0d0), 100)
    second = mod(dt, 100.0d0)

    call getPlanetaryPosition(planet_number, year, month, day, hour, minute, second, pos)

    rh = int(pos(1))
    rs = 60.0d0 * (pos(1) - rh)
    rm = int(rs)
    rs = 60.0d0 * (rs - rm)

    sign = '+'
    if (pos(2) < 0) then
        sign = '-'
    end if

    dh = abs(int(pos(2)))
    ds = 60.0d0 * (abs(pos(2)) - dh)
    dm = int(ds)
    ds = 60.0d0 * (ds - dm)

    write(*, '(i5, "年 ", i2, "月 ", i2, "日 ", i2, "時 ", i2, "分 ", i2, "秒")') &
    int(year), int(month), int(day), int(hour), int(minute), int(second)
    write(*, *)
    write(*, '(a, "の視位置")') trim(CELESTIA(planet_number))
    write(*, *)
    if (planet_number == 1) then
        ! 月の場合は黄道傾斜角を表示しない
        sm = int(pos(4))
        ss = 60.0d0 * (pos(4) - sm)    
        sfmt = '(i2, " ", i2, " ", f6.3, "    ", a1, i2, " ", i2, " ", f5.2, "   ", f9.7, "    ", i3, " ", f5.2)'
        write(*, *) '    赤経            赤緯         H.P.     視半径'
        write(*, *) ' h  m  s          。 ,                        ,  ,,'
        write(*, sfmt) rh, rm, rs, sign, dh, dm, ds, pos(3), sm, ss       
    else
        sm = int(pos(4))
        ss = 60.0d0 * (pos(4) - sm)    
        sfmt = '(i2, " ", i2, " ", f6.3, "    ", a1, i2, " ", i2, " ", f5.2, "   ", f9.7, "    ", i2, " ", f5.2, "     ", f10.7)'
        write(*, '("")')
        write(*, *) '    赤経            赤緯       地心距離     視半径      黄道傾斜角'
        write(*, *) ' h  m  s          。 ,               AU      ,  ,,        。'
        write(*, sfmt) rh, rm, rs, sign, dh, dm, ds, pos(3), sm, ss, pos(6)
    end if
    write(*, *)
    
    stop
end program pcob_main
