subroutine getPlanetaryPosition(planet_number, year, month, day, hour, minute, second, pos)
! 視赤経・視赤緯・地心距離および視半径の計算
! 海上保安庁 海洋情報部による略算式(2022版)
! https:!www1.kaiho.mlit.go.jp/KOHO/index.html
!
! 年度が替わる場合は、x()およびr()の値を変更する。
! 太陽以外の天体の位置を計算する場合は、x()およびSDの値を変更する。
!
! 引　数:  観測年月日＋時分秒(日本標準時)
!          year        : 年
!          month       : 月
!          day         : 日
!          hour        : 時
!          minute      : 分
!          second      : 秒
! 戻り値:  視赤経・視赤緯・地心距離・視半径を返す
!          pos(1): 視赤経
!          pos(2): 視赤緯
!          pos(3): 地心距離
!          pos(4): 視半径
!          pos(5): R
!          pos(6): 平均黄道傾斜角
!
! 計算精度について:
! 略算式とはいえ、精度は国立天文台天文情報センター暦計算室の値と較べても
! 非常に高い一致をみる。ただ、使用にあたっては、係数の桁数の関係で視赤経
! および視赤緯、地心距離は小数点以下5-6桁までにとどめる。
    implicit none

    double precision, parameter :: PI = 3.141592653589793d0
    double precision, parameter :: RAD = 180.0d0 / PI
    double precision, parameter :: DELTAT = 70.0d0
    double precision, dimension(6), parameter :: SO = (/15.05d0, 16.02d0, 8.3d0, 4.7d0, 92.1d0, 73.8d0/) ! ベースとなる太陽の視半径
    character(7), parameter :: CELESTIA(6) = (/'moon   ', 'sun    ', 'venus  ', 'mars   ', 'jupiter', 'saturn '/)
    
    double precision, intent(in) :: year, month, day
    double precision, intent(in) :: hour, minute, second
    double precision, intent(out) :: pos(6)
    integer, intent(in) :: planet_number

    double precision, dimension(12, 2) :: check
    double precision, dimension(0:31, 1:3) :: x
    double precision, dimension(0:10, 1:2) :: r
    double precision :: f, tp, theta, rr
    double precision :: ra ! 赤経
    double precision :: dc ! 赤緯
    double precision :: ds ! 地心距離
    double precision :: ec ! 黄道傾斜角
    integer :: i, n, a, b, p, q, y, s, t
    character(20) :: path
    character(19) :: filename, rfilename
    character(2) :: filenumber
    character(4) :: syear

    ! 各種引数の計算
    p = int(month) - 1
    q = int((month + 7) / 10)
    y = int((year / 4) - int(year / 4) + 0.77)
    s = int(p * 0.55 - 0.33)
    t = int(30 * p + q * (s - y) + p * (1 - q) + day)
    f = (hour - 9.0d0) / 24.0d0 + minute / 1440.0d0 + second / 86400.0d0
    tp = t + f + DELTAT / 86400.0d0

    ! 惑星番号によって読み込むファイル名を構成する
    write(syear, '(i4)') int(year)
    path = './data/'//syear//'/'//trim(CELESTIA(planet_number))//'/'

    select case(planet_number)
    case(1) ! 月
        n = 30
        check( 1, 1:2) = (/  0.0d0,  32.0d0/)
        check( 2, 1:2) = (/ 31.0d0,  60.0d0/)
        check( 3, 1:2) = (/ 59.0d0,  91.0d0/)
        check( 4, 1:2) = (/ 90.0d0, 121.0d0/)
        check( 5, 1:2) = (/120.0d0, 152.0d0/)
        check( 6, 1:2) = (/151.0d0, 182.0d0/)
        check( 7, 1:2) = (/181.0d0, 213.0d0/)
        check( 8, 1:2) = (/212.0d0, 244.0d0/)
        check( 9, 1:2) = (/243.0d0, 274.0d0/)
        check(10, 1:2) = (/273.0d0, 305.0d0/)
        check(11, 1:2) = (/304.0d0, 335.0d0/)
        check(12, 1:2) = (/334.0d0, 366.0d0/)
        do i = 1, 12
            if ((check(i, 1) <= tp) .and. (tp <= check(i, 2))) then
                a = int(check(i,1))
                b = int(check(i, 2))
                write(filenumber, '(i2.2)') i
                filename = trim(CELESTIA(planet_number))//'-data-'//filenumber//'.csv'
                exit
            end if
        end do
    case(2:6) ! 太陽・金星・火星・木星・土星
        n = 18
        check(1, 1:2) = (/  0.0d0, 121.0d0/)
        check(2, 1:2) = (/120.0d0, 244.0d0/)
        check(3, 1:2) = (/243.0d0, 366.0d0/)
        do i = 1, 3
            if ((check(i, 1) <= tp) .and. (tp <= check(i, 2))) then
                a = int(check(i,1))
                b = int(check(i, 2))
                write(filenumber, '(i2.2)') i
                filename = trim(CELESTIA(planet_number))//'-data-'//filenumber//'.csv'
                rfilename = 'r-data-'//filenumber//'.csv'
                exit
            end if
        end do
    end select

    ! 構成されたファイル名から係数データを読み込む
    write(*, *) 
    ! write(*, '("Planet: ", a)') CELESTIA(planet_number)
    ! write(*, '("File Name: ", a)') filename
    ! write(*, '("a: ", i3)') a
    ! write(*, '("b: ", i3)') b
    ! write(*, *) '    RA         DC         Dis'
    open(10, file=trim(path)//trim(filename), status="old")
    do i = 0, n
        read(10, *) x(i, 1:3)
        ! write(*, '(f10.6, " ", f10.6, " ", f10.6)') x(i, 1), x(i, 2), x(i, 3)
    end do
    close(10)
    write(*, *)

    ! Rおよび黄道傾斜角の係数を読み込む(太陽・金星・火星・木星・土星)
    if ((2 <= planet_number) .and. (planet_number <= 6)) then
        open(20, file='./data/'//syear//'/R/'//trim(rfilename), status="old")
        do i = 0, 8
            read(20, *) r(i, 1:2)
            ! write(*, '(f10.6, " ", f10.6)') r(i, 1), r(i, 2)
        end do
        close(20)
    end if
    write(*, *)

    ! 視赤経・視赤緯および地心距離の計算
    theta = acos((2.0d0 * tp - (a + b)) / (b - a)) * RAD
    ra = 0.0d0
    dc = 0.0d0
    ds = 0.0d0
    do i = 0, 17
        ra = ra + (x(i + 1, 1) * cos(mod((theta * i), 360.0d0) / RAD))
        dc = dc + (x(i + 1, 2) * cos(mod((theta * i), 360.0d0) / RAD))
        ds = ds + (x(i + 1, 3) * cos(mod((theta * i), 360.0d0) / RAD))
    end do

    if (int(ra) >= 24) then
        ra = ra - 24
    else
        if (ra < 0) then
            ra = ra + 24
        end if
    end if

    pos(1) = ra
    pos(2) = dc
    pos(3) = ds
    pos(4) = SO(planet_number) / ds

    ! Rと黄道傾斜角の計算(太陽・金星・火星・木星・土星)
    if ((2 <= planet_number) .and. (planet_number <= 6)) then
        theta = acos((2.0d0 * (t + f) - (a + b)) / (b - a)) * RAD
        rr = 0.0d0
        ec = 0.0d0
        do i = 0, 7
            rr = rr + (r(i + 1, 1) * cos(mod((theta * i), 360.0d0) / RAD))        
            ec = ec + (r(i + 1, 2) * cos(mod((theta * i), 360.0d0) / RAD))        
        end do

        ra = rr - ra

        pos(5) = ra ! Rを考慮した赤経
        pos(6) = ec ! 黄道傾斜角
    else
        ! 月のときは０が入る
        pos(5) = 0.0d0
        pos(6) = 0.0d0 
    end if

    return
end subroutine getPlanetaryPosition
