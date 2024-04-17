
p - bu * u - bv * v = a
p - t * ef          = e

// 6 unknowns: p1, p2, p3, t, bu, bv

// 6 equations:
p1 - bu * u1 - bv * v1 = a1
p2 - bu * u2 - bv * v2 = a2
p3 - bu * u3 - bv * v3 = a3
p1 - t * ef1 = e1
p2 - t * ef2 = e2
p3 - t * ef3 = e3

p1 -  u1 * bu - v1 * bv = a1
p2 -  u2 * bu - v2 * bv = a2
p3 -  u3 * bu - v3 * bv = a3
p1 - ef1 * t            = e1
p2 - ef2 * t            = e2
p3 - ef3 * t            = e3

p1                     -u1 * bu  -v1 * bv = a1
     p2                -u2 * bu  -v2 * bv = a2
          p3           -u3 * bu  -v3 * bv = a3
p1           -ef1 * t                     = e1
     p2      -ef2 * t                     = e2
          p3 -ef3 * t                     = e3

[1, 0, 0,    0, -u1, -v1]   [p1]   [a1]
[0, 1, 0,    0, -u2, -v2]   [p2]   [a2]
[0, 0, 1,    0, -u3, -v3] * [p3] = [a3]
[1, 0, 0, -ef1,   0,   0]   [ t]   [e1]
[0, 1, 0, -ef2,   0,   0]   [bu]   [e2]
[0, 0, 1, -ef3,   0,   0]   [bv]   [e3]

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
       e(3)]

