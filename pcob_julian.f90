subroutine getJulian(year, month, day, hour, minute, second, julian)
! ユリウス日を求める
! 引　数:  観測年月日＋時分秒(日本標準時)
!          year        : 年
!          month       : 月
!          day         : 日
!          hour        : 時
!          minute      : 分
!          second      : 秒
! 戻り値:  ユリウス日を返す
!          julianday: ユリウス日(世界時)
!
! 計算式について
! 本ルーチンで使用している計算式は、紀元前や1582年10月15日より
! 前のユリウス日の計算には使えないので注意が必要である。

    implicit none

    double precision, intent(in) :: year, month, day, hour, minute, second
    double precision, intent(out) :: julian
    double precision :: mm, yy

    mm = month
    yy = year

    if (mm < 3.0d0) then
        mm = mm + 12.0d0
        yy = yy - 1.0d0
    end if

    julian = int(yy * 365.25) + int(yy / 400.0) - int(yy / 100.0) + int(30.59 * (mm - 2)) + day + 1721088.5

    julian = julian + (hour / 24.0 + minute / 1440.0 + second / 86400.0)
    julian = julian - 0.375

    return
end subroutine getJulian
    
subroutine getdaytime(julian, year, month, day)
! ユリウス日から年月日を求める
! 天文計算入門　63頁 (16.6)より
! 引　数:  ユリウス日
!          julian      : 時
! 戻り値:  年月日を返す。日は小数点
!          year        : 年
!          month       : 月
!          day         : 日

    implicit none

    ! 各月の日数
    integer, dimension(12) :: T = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)

    double precision, intent(in) :: julian
    double precision :: day
    integer :: year, month
    integer :: a, b, c, e, f, g, h

    a = int(julian + 68569.5)
    b = int(a / 36524.25)
    c = a - int(36524.25 * b + 0.75)
    e = int((c + 1) / 365.25025)
    f = c - int(365.25 * e) + 31
    g = int(f / 30.59)
    day = f - int(30.59 * g) + (julian + 0.5) - int(julian + 0.5)  ! 日
    h = int(g / 11)
    month = g - 12 * h + 2  ! 月
    year = 100 * (b - 49) + e + h  ! 年

    ! うるう年の判定
    if ((mod(year, 4) == 0 .and. mod(year, 100) /= 0) .or. mod(year, 400) == 0) then
        T(2) = 29
    end if

    ! 12月32日になったときの処理
    if (month == 12) then
        if (day > T(month)) then
            year = year + 1
            month = (month + 1) - 12
            day = 1
        end if
    else
        ! 12月以外の月における処理
        if (day > T(month)) then
            month = month + 1
            day = 1
        end if
    end if

    T(2) = 28

    return
end subroutine getdaytime
    