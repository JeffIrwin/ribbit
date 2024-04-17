clear
clc
close(winsid())

// Points a, b, and c form the triangle
a = [1, 0, 0]'
b = [0, 2, 0]'
c = [0, 0, 3]'

// plot
xt(:,1) = [a(1), b(1), c(1), a(1)]
xt(:,2) = [a(2), b(2), c(2), a(2)]
xt(:,3) = [a(3), b(3), c(3), a(3)]
param3d(xt(:,1), xt(:,2), xt(:,3))

// Points e and f form the line segment
e = [0.2, 0.1, 0.3]'

f = [0.5, 1.5, 1  ]' // valid intersection
//f = -[0.5, 1.5, 1  ]' // outside of line segment
//f = [1, -1, 3]'    // outside of triangle
// TODO: test a case where line and triangle are parallel

// plot
xl(:,1) = [e(1), f(1)]
xl(:,2) = [e(2), f(2)]
xl(:,3) = [e(3), f(3)]
param3d(xl(:,1), xl(:,2), xl(:,3), 'b')

// Vectors along the triangle's edges
u = b - a
v = c - a

// Vector along line segment
ef = f - e

// Represent intersection point p in the triangle:
//
//     p = a + bu * u + bv * v

// Represent the point p on the line segment:
//
//     p = e + t * ef

//     p - bu * u - bv * v = a
//     p - t * ef          = e


mat = [1, 0, 0,    0, -u(1), -v(1) 
       0, 1, 0,    0, -u(2), -v(2) 
       0, 0, 1,    0, -u(3), -v(3) 
       1, 0, 0, -ef(1),   0,   0 
       0, 1, 0, -ef(2),   0,   0 
       0, 0, 1, -ef(3),   0,   0 
      ]

rhs = [a(1)
       a(2)
       a(3)
       e(1)
       e(2)
       e(3)
      ]

// Do the linear algebra
vars = mat \ rhs

// Unpack the answers from vars
p  = vars(1:3)
t  = vars(4)
bu = vars(5)
bv = vars(6)

bw = 1 - bu - bv

scatter3d(p(1), p(2), p(3), 'o')

// All of these parametric coordinates need to be in the range [0, 1] for the intersection to be valid
mprintf("t  = %f\n", t)
mprintf("bu = %f\n", bu)
mprintf("bv = %f\n", bv)
mprintf("bw = %f\n", bw)

//
mprintf("p = [%f, %f, %f]\n", p(1), p(2), p(3))


