! Gareth Daniel Enoch, 213 552 759
PROGRAM prac9

REAL :: B,A !UPPER AND LOWER LIMITS
REAL :: N !NUMBER OF EQUISPACED LENGTHS
REAL :: PI !constant pi
REAL :: H,H2,H3 !MAGNITUDE OF EQUISPACED LENGTHS
REAL :: Y,Y2,Y3,Y4,Y5 !FUNCTION
INTEGER :: I
real :: AREA,AREA2,AREA3,AREA4,AREA5 !sum of the then equispaced areas under the graph

PI = 4.0*ATAN(1.0)
A=0
B=2*PI
N=10 ! step size
H=(B-A)/N ! formula to calculate equispaced lengths
area=0 !starting off area at zero (for every other areas as well, area2, area3 etc.)

DO I = 0, 9
! 0 to 9 is 10 values which allows us to get 10 equispaced values of x and there the
! corresponding heights at those x positions. This method will be used for area2 to area4 as well
 Y = H * (SIN(H*I))**2
 area=area+y
END DO

PRINT*,AREA

AREA2=0

DO I = 0,9

 Y2= H*0.5*(((SIN(H*I))**2)+((SIN(H*(I+1)))**2))
!"I+1" in the equation allows us to get the y value for x=10 where I=9. (applies to area4 as well)
 AREA2=AREA2+Y2

END DO

PRINT*,AREA2

h2=5.0/10.0

AREA3=0

DO I=0,9

 Y3 = H2*(((H2*I)**2)+(2*H2*I)-8)
 AREA3=AREA3+Y3

END DO

PRINT*, AREA3

area4 = 0

DO I = 0,9

 Y4 = H2*0.5*((((H2*I)**2)+(2*H2*I)-8)+(((H2*(I+1))**2)+(2*H2*(I+1))-8))
 AREA4 = AREA4 + Y4

END DO

PRINT*, AREA4

!!!!!!!!!!! THE TRAPEZOIDAL RULE IS THE MOST ACCURATE OUT OF THE TWO SINCE IT GIVES VALUES
!!!!!!!!!!! WHICH ARE MUCH CLOSER TO ANALYTICAL ANSWERS.
!!!!!!!!!!! THE TWO VALUES GIVE THE SAME ANSWERS IN PART I) BECAUSE OF THE SYMMETRY OF THE GRAPH

H3 = (5.0-(-5.0))/10.0
AREA5 = 0

DO I = 0,4
!As a result of the symmetry of the problem, one can take the areas on either the left or right
!side of the y axis and multiply it by a factor of two. 0 to 4 gives 5 values starting at x = 0
!to x = 4. "I+1" in the equation allows us to get the y value for x=5 where I=4.
 Y5 = H3*0.5*((EXP(-2*(H3*I)**2))+(EXP(-2*(H3*(I+1))**2)))
 AREA5 = AREA5 +Y5

END DO
area5=2*area5
PRINT*, AREA5

END PROGRAM prac9