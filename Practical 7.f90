! Gareth Daniel Enoch, 213 552 759
PROGRAM PRAC7
IMPLICIT NONE


REAL, DIMENSION(:,:), ALLOCATABLE :: Disp
integer :: i
real :: no, maximum
REAL :: TSTEP, TMAX, T !WHERE TSTEP IS THE TIME INCRIMINT AND TMAX IS MAXIMUM TIME AND T IS TIME, ALL 3 ARE
IN SECONDS
REAL :: Y !DISPLACEMENT IN METERS
PRINT*,'PLEASE INPUT VALUES FOR TSTEP AND TMAX RESPECTIVELY'
READ*, TSTEP, TMAX

Y = X(T)

no = (tmax/tstep)+1

ALLOCATE (Disp(Int(no),2)) !ALLOCATE NUMBERS PUT IN THAT ARE WORKED OUT


T=0

OPEN (10, FILE = 'DataLinearDrag.txt', ACTION='write')
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO WHILE (T<=TMAX)

 Y = X(T)
 WRITE (10,*),t,y

 ! PRINT*, t, y

  i = 1

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   do while (i<=Int(no))

     Disp( i,1)= t
     Disp( i,2)= y
    print*, disp(i,1),disp(i,2)
     i=i+1
    EXIT
   end do

 ! do while (i<=Int(no))

 !  maximum = 0

 !   if (maxval(Disp,2)>maximum) then
 !     maximum = maxval(Disp,2)
 !     print*, maximum

 !   end if

 ! end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  T = T + TSTEP

 END DO

 CLOSE(10)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 CONTAINS
   REAL FUNCTION X(T)
   REAL :: T, X0, VT, TOR, V0

    X0=2.0!METERS
    VT=1.0!MS^-1
    TOR=1.0!SECONDS
    V0=1.0

    X = X0 - VT*T+(V0+VT)*TOR*(1-EXP(-1*(T/TOR)))

   END FUNCTION

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END PROGRAM PRAC7