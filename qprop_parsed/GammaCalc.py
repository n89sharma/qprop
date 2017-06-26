import numpy as np
import scipy.optimize as opt
import math

def gammaFunc ( x0, V, omega, c, beta, r, R, B, a, dclda, cl0, clmax, clmin):
    va, vt = x0[0], x0[1]
    Wa  = V + va
    Wt  = omega*r - vt
    W   = math.sqrt(Wa*Wa + Wt*Wt)
    alpha   = beta - math.atan(Wa/Wt)
    Ma      = W/a
    Ma      = 0.9 if Ma > .9 else Ma
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

def getGamma(omega, V, r, R, B, va, vt):
    Wt  = omega*r - vt
    Wa  = V + va
    W   = math.sqrt(Wt*Wt + Wa*Wa)
    advanceRatio    = r*Wa/R/Wt
    f               = 0.5*B*(1 - r/R)/advanceRatio
    F               = 2*math.acos(math.exp(-f))/math.pi
    gamma           = vt*4.*math.pi*r*F*math.sqrt(1 + math.pow( (4.*advanceRatio*R/math.pi/B/r), 2))/B
    return Wt, Wa, W, gamma, va, vt

def getDragCoefficient(cl, Re, Ma, clcd0, cd0, cd2, ReRef, ReExp, mcrit):
    cdmf = 10.
    iexp = 3.
    fac = (Re/ReRef)**ReExp

    clb = cl - clcd0
    cd = (cd0 + cd2*clb**2)*fac

    if Ma > mcrit:
        cd = cd + cdmf*(Ma-mcrit)**iexp

    return cd

def getTorque(rho, B, W, c, dr):
    return 0
# r       = np.linspace(0.75, R, 7)
a       = 340.      #m/s
rho     = 1.225     #kg/m^3
mu      = 1.78E-5   #kg/m/s
B       = 2
R       = 3         #m
V       = 20        #m/s 72 km/hr
omega   = 1500.*math.pi/30. #rad/s
r       = np.asarray([0.75, 1., 1.5, 2., 2.5, 2.875, 3.])
c       = np.asarray([0.66, 0.69, 0.63, 0.55, 0.44, 0.30, 0.19])
betaDeg = np.asarray([27.5, 22., 15.2, 10.2, 6.5, 4.6, 4.2])
beta    = betaDeg*math.pi/180.
va      = r*0.
vt      = r*0.
gamma   = r*0.
clmin   = -0.3
clmax   = 1.2
cl0     = 0.5
dclda   = 2.*math.pi


res = []
for _r, _c, _beta in zip(r, c, beta):
    optRes = opt.root(
        gammaFunc,
        [1.,1.],
        args=(V, omega, _c, _beta, _r, R, B, a, dclda, cl0, clmax, clmin))
    res.append(optRes.x)

res = np.asarray([getGamma(omega, V, _r, R, B, va, vt) for (va, vt), _r in zip(res, r)])
#Wt, Wa, W, gamma, va, vt
print [res_[-2] for res_ in res]
