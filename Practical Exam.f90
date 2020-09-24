! Gareth Daniel Enoch, 213 552 759
PROGRAM Aliens
IMPLICIT NONE
REAL(8) :: X, Y,X1, Y1
REAL(8) :: X_0, Y_0 !INITIAL CARTESIAN POSITIONS
REAL(8) :: VX_0, VY_0 !INITIAL CARTESIAN VELOCITIES
REAL(8) :: VX, VY,VX1, VY1
REAL(8) :: h, hSTEP
REAL(8) :: G, GX, GY !GRAVITATION ACCELERATIONS AND ITS CARTESIAN COMPONENTS
REAL(8) :: G_CONST
REAL(8) :: G_NEW
real(8) :: t
REAL(8) :: B !QUADRATIC DRAG
REAL(8) :: M !MASS OF ICBM
REAL(8) :: RADS
REAL(8) :: R_EARTH !RADIUS OF EARTH
REAL(8) :: M_EARTH ! MASS OF EARTH
REAL(8) :: R_NEW !DISTANCE FROM GROUND ANT THEREFORE NEW RADIUS FOR EQUATION
REAL(8) :: theta
REAL(8) :: G_X, G_Y, G_X1, G_Y1
REAL(8) :: V_Y, V_X
real(8) :: i
real(8) :: maxh
REAL(8) :: K1X,K1Y,K1VX,K1VY
REAL(8) :: K2X,K2Y,K2VX,K2VY
logical(8) :: L1,L2

!OPEN (40, FILE = 'TRAJECT1.txt')
!OPEN (50, FILE = 'TRAJECT2.txt')
!OPEN (60, FILE = 'TRAJECT3.txt')

t = 0
X_0 = 0.0
Y_0 = 6.431E6
VX_0 = 7000.0
VY_0 = 3500.0
h = 0.001
G_CONST = 6.67384E-11
M_EARTH = 5.972E24
R_EARTH = 6.371E6
G = ( G_CONST * M_EARTH ) / ( ( R_EARTH )**2 )
Y1 = 6.431E6
X1 = 0
VY1 = 3500.0
VX1 = 7000.0
R_NEW = SQRT( (X1**2) + (Y1**2) )
theta = ASIN(Y1/R_NEW)
M = 96.75E3
maxH=0
l1 = (r_new>=(R_EARTH+100))
L2 = (R_NEW<=(R_EARTH+100))
i = 0.001

DO WHILE (R_NEW>=R_EARTH)

 if (l1) then
  B = 0.0
 end if

 IF (L2) THEN
  B = 0.05
 END IF

 G_X= GRAVX(R_NEW,theta)

 G_Y= GRAVY(R_NEW,theta)

 K1X = H * VX1
 K1Y = H * VY1

 K1VX = H * (-SIGN(((B/M)*(VX1**2)), VX1)-SIGN(G_X1,X1))
 K1VY = H * (-SIGN(((B/M)*(VY1**2)), VY1)-SIGN(G_Y1,Y1))

 K2X = H * (VX1 + 0.5*K1VX)
 K2Y = H * (VY1 + 0.5*K1VY)

 R_NEW = SQRT( ((X1 + 0.5*K1X)**2) + ((Y1 + 0.5*K1Y)**2) )
 theta = ASIN((Y1 + 0.5*K1Y)/R_NEW)
 !writE (50,*),r_new, t
 print*, r_new, t
 G_X1=GRAVX(R_NEW,theta)
 G_Y1= GRAVY(R_NEW,theta)

 K2VX = H * (-SIGN(((B/M)*(VX1+0.5*K1VX)**2),VX1+0.5*K1VX) - SIGN(G_X1,X1 + 0.5*K1X))
 K2VY = H * (-SIGN(((B/M)*(VY1+0.5*K1VY)**2),VY1+0.5*K1VY) - SIGN(G_Y1,Y1+ 0.5*K1Y))


 X1 = X1 + K2X
 Y1 = Y1 + K2Y
 VX1 = VX1 + K2VX
 VY1 = VY1 + K2VY

 CALL hmax(r_new,maxh)

 t = t + h
 writE (60,*),x1,y1
 PRINT*,x1,y1
 ! writE (40,*),x1,y1
 !PRINT*,r_new

END DO
!CLOSE (50)
!CLOSE (40)
CLOSE (60)
Print*,maxH
maxH = 0
 CONTAINS

 SUBROUTINE hmax(r_new,maxh)
  real (8), intent(in) ::r_new
  real (8), intent(inout) :: maxh
   IF (r_new > MAXh) THEN

    MAXh = r_new ! REPLACING OLD MAXh VALUE WITH NEW LARGER VALUE
   
   END IF

 END SUBROUTINE

 REAL(8) FUNCTION GRAVX(R_NEW,theta)
  REAL(8) :: G_NEW
  REAL(8) :: THETA
  REAL(8) :: G_CONST
  REAL(8) :: M_EARTH
  REAL(8) :: R_NEW
  M_EARTH = 5.972E24
  G_CONST = 6.67384E-11
  G_NEW = ( G_CONST * M_EARTH ) / ( ( R_NEW )**2 )
  GRAVX = G_NEW * COS(THETA)

 END FUNCTION

 REAL(8) FUNCTION GRAVY(R_NEW,theta)
  REAL(8) :: G_NEW
  REAL(8) :: THETA
  REAL(8) :: G_CONST
  REAL(8) :: M_EARTH
  REAL(8) :: R_NEW
  M_EARTH = 5.972E24
  G_CONST = 6.67384E-11
  G_NEW = ( G_CONST * M_EARTH ) / ( ( R_NEW )**2 )
  GRAVY = G_NEW * SIN(THETA)

 END FUNCTION

END PROGRAM Aliens