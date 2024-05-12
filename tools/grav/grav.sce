clear
clc
close(winsid())

// Initial conditions

//// Sun and earth
//pos = [
//    0        0 0
//    150.88e9 0 0
//]
//vel = [
//    0     -0.1   0
//    0 29784.8 0 // actual
////    0 20784.8 0  // elliptical
//]
//mass = [
//    1.989e30
//    5.972e24
//]

//// 2 bodies
//pos = [
//    0        0 0
//    150.88e9 0 0
//]
//vel = [
//    0   -250   0
//    0 1000 0
//]
//mass = [
//    2e28
//    5e27
//]

// 3 bodies
pos = [
    0        0  0
    150.88e9 0  0
    0 155.88e9 0
]
vel = [
    500 -500 0
    0  1000 0
   -1000 0  0
]
mass = [
    1e28
    5e27
    5e27
]

g = 6.6743015e-11

t_start = 0
t_end   = 1.8e8
dt      = 2e6

min_order = 3
max_order = 4

//==================================================================

// Symplectic integrator constants
//
// Ref:
//
//     https://en.wikipedia.org/wiki/Symplectic_integrator#Examples

c14 = 1 / (2 * (2 - 2 ** (1/3)))
c24 = (1 - 2 ** (1/3)) / (2 * (2 - 2 ** (1/3)))
d14 = 1 / (2 - 2 ** (1/3))
d24 = -2 ** (1/3) / (2 - 2 ** (1/3))

// Only the lower-left triangle is used
cs = [
    1,  0  , 0  , 0  // Symplectic Euler (1st order)
    0,  1  , 0  , 0  // Verlet method (2nd order)
    1, -2/3, 2/3, 0  // Ruth 1983 (3rd order)
    c14, c24, c24, c14,
]

ds = [
     1   , 0  , 0   , 0
     1/2 , 1/2, 0   , 0
    -1/24, 3/4, 7/24, 0
    d14  , d24, d14 , 0
]
        
// Number of bodies
nb = size(mass, "*")

styles = [
//    "r-",
//    "g-",
//    "b-",
//    "k-",
    "r",
    "g",
    "b",
    "k",
]

body_styles = [
    "o"
    "x"
    "s"
]

//==================================================================

function [acc] = get_acc(pos, vel, mass)
    // vel arg is unused (for now)
    
    acc = zeros(nb,3)
    
    // Loop through unique pairs of bodies
    for ia = 1: nb
    for ib = 1: ia - 1
        
        // 1st order
        apos = pos(ia,:)
        bpos = pos(ib,:)
        
        ma = mass(ia)
        mb = mass(ib)
        
        r = bpos - apos
        f = g * ma * mb / (r * r') * (r / norm(r))
        
        acc(ia,:) = acc(ia,:) + f / mass(ia)
        acc(ib,:) = acc(ib,:) - f / mass(ib)
        
    end
    end

endfunction

//==================================================================

function solve_grav_symp(iorder)
    // Integrate the equations of motion for Newtonian gravity using a symplectic integrator of order `iorder`
    
    c = cs(iorder,:)
    d = ds(iorder,:)
    
    it = 0
    for t = t_start: dt: t_end
        it = it + 1
        //mprintf("%e\n", t)
        
        for ic = 1: iorder
            pos = pos + c(ic) * vel * dt
            acc = get_acc(pos, vel, mass)
            vel = vel + d(ic) * acc * dt
        end
        
        // Save state for plotting
        data(:,:,it) = pos
        
    end
    nt = size(data,3)
    
    // Plot it
    for ib = 1: nb
        x = matrix(data(ib,1,:), nt)
        y = matrix(data(ib,2,:), nt)
        style = styles(iorder) + body_styles(ib)
        plot(x, y, style)
    end

endfunction

//==================================================================

function solve_grav_rk4()
    // Integrate the equations of motion for Newtonian gravity using Runge-Kutta 4
    
    it = 0
    for t = t_start: dt: t_end
        it = it + 1
        
        k1v = get_acc(pos, vel, mass)
        k1r = vel
        
        k2v = get_acc(pos + 0.5 * dt * k1r, vel, mass)
        k2r = vel + 0.5 * dt * k1v
        
        k3v = get_acc(pos + 0.5 * dt * k2r, vel, mass)
        k3r = vel + 0.5 * dt * k2v
        
        k4v = get_acc(pos +       dt * k3r, vel, mass)
        k4r = vel +       dt * k3v
        
        vel = vel + dt/6 * (k1v + 2*k2v + 2*k3v + k4v)
        pos = pos + dt/6 * (k1r + 2*k2r + 2*k3r + k4r)
        
        // Save state for plotting
        data(:,:,it) = pos
        
    end
    nt = size(data,3)
    
    // Plot it
    for ib = 1: nb
        x = matrix(data(ib,1,:), nt)
        y = matrix(data(ib,2,:), nt)
        style = "g" + body_styles(ib)
        plot(x, y, style)
    end

endfunction

//==================================================================

scf()

for iorder = min_order: max_order
    solve_grav_symp(iorder)
end

solve_grav_rk4()

//legend("Symp 3", "Symp 4", "RK4") // plot is in body-major order so this is wrong unless you only plot 1 body

