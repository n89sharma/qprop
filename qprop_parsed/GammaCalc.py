import numpy as np
import scipy.optimize as opt
import math

def gammaFunc ( x0, V, omega, c, beta, r, R, B, a, dclda, cl0, clmax, clmin):
    va, vt = x0[0], x0[1]

    Wt  = omega*r - vt
    Wa  = V + va
    W   = math.sqrt(Wa*Wa + Wt*Wt)

    alpha   = beta - math.atan(Wa/Wt)
    Ma      = W/a
    cl      = (dclda*alpha + cl0)/math.sqrt(1. - Ma)
    if cl > clmax:
        cl  = clmax*math.cos(alpha - cl0/dclda)
    elif cl < clmin:
        cl  = clmin*math.cos(alpha - cl0/dclda)
    gamma1  = 0.5*W*c*cl

    advanceRatio    = r*Wa/R/Wt
    f               = 0.5*B*(1 - r/R)/advanceRatio
    F               = 2*math.acos(math.exp(-f))/math.pi
    gamma2          = vt*4.*math.pi*r*F*math.sqrt(1 + math.pow( (4.*advanceRatio*R/math.pi/B/r), 2))/B
    return np.asarray([Wt/Wa - va/vt, gamma1 - gamma2])

def getGamma(omega, V, r, R, B, vt):
    Wt  = omega*r - vt
    Wa  = V + va

    advanceRatio    = r*Wa/R/Wt
    f               = 0.5*B*(1 - r/R)/advanceRatio
    F               = 2*math.acos(math.exp(-f))/math.pi
    return vt*4.*math.pi*r*F*math.sqrt(1 + math.pow( (4.*advanceRatio*R/math.pi/B/r), 2))/B


V       = 20.
omega   = 3000.*2*math.pi/3600
B       = 2
R       = 3.
n       = 7
a       = 340.      #m/s
rho     = 1.225     #kg/m^3
mu      = 1.78E-5   #kg/m/s
r       = np.linspace(0.75, R, n)
r       = np.asarray([0.75, 1., 1.5, 2., 2.5, 2.875, 3.])
c       = np.asarray([0.66, 0.69, 0.63, 0.55, 0.44, 0.30, 0.19])
beta    = np.asarray([27.5, 22., 15.2, 10.2, 6.5, 4.6, 4.2])
va      = r*0.
vt      = r*0.
gamma   = r*0.
clmin   = -0.3
clmax   = 1.2
cl0     = 0.5
dclda   = 2.*math.pi

for _r, _c, _beta in zip(r, c, beta):
    optRes = opt.fsolve(
        gammaFunc,
        [1,1],
        args=(V, omega, _c, _beta, _r, R, B, a, dclda, cl0, clmax, clmin))
    print optRes
    # print optRes
# gamma = getGamma(omega, V, r, R, B, vt)
# va, vt, gamma = gammaFunc(va, vt, gamma)
