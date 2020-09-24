! Gareth Daniel Enoch, 213 552 759
PROGRAM PRAC8
IMPLICIT NONE
 REAL :: PI, X, z
 REAL :: R
 REAL :: VERT, HMAX
 INTEGER:: COUNTER
 INTEGER :: THETA, I
 
 !OPEN (40, FILE = 'proj1.txt')
!!!!! OPEN (50, FILE = 'hmax 10tau.txt')
!!!!!OPEN (60, FILE = 'R 100tau.txt')
 Pi =4.0*ATAN(1.0) !constant to help convert degrees to radians
 THETA = 1

DO WHILE (THETA<=89)

 R = THETA * (Pi/180) !convert theta to radians
 X = 0 !start again at x= 0 (origin)
 HMAX = 0
 COUNTER = 0

    DO WHILE (VERT>=0)

      VERT = Y(R, X) !aquires results from function

      COUNTER = COUNTER + 1 !FINDING THE AMOUNT OF VERT VALUES CALCULATED

      CALL MaxHeight(VERT,HMAX,COUNTER)

      X = X + 0.01 !incriminting x value

    END DO
   !!!!! writE (50,*),hmax, theta
   !!!!!writE (60,*),x, theta
 Print*,hmax
 hmax = 0
 theta = theta + 1 !incriminting theta value
 VERT = 0 !start again at "y"= 0 (origin)

END DO

!!!!!CLOSE(50)
!!!!!CLOSE(60)
CONTAINS

SUBROUTINE MaxHeight(VERT, HMAX, COUNTER)
 REAL, INTENT(IN) :: VERT
 REAL, INTENT(OUT) :: HMAX
 INTEGER, INTENT(IN) :: COUNTER
 DO I = 0, COUNTER

  IF (VERT > HMAX) THEN
   
   HMAX = VERT ! REPLACING OLD HMAX VALUE WITH NEW LARGER VALUE

  END IF
 
 END DO

END SUBROUTINE

REAL FUNCTION Y(fi, eX)
 REAL :: G !MS^-2 (GRAVITATIONAL ACCELLERATION)
 REAL :: U !MS^-1
 REAL :: TAU !SECONDS
 REAL :: eX
 real :: fi
  U = 100.0
  TAU = 100.0
  G = 9.8

  Y = G*(TAU**2)*LOG(1-(eX/(TAU*U*COS(fi))))+eX*(TAN(fi)+((G*TAU)/(U*COS(fi))))
END FUNCTION

END PROGRAM PRAC8