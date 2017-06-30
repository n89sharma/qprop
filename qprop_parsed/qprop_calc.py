import numpy as np
import scipy.optimize as opt
import scipy.interpolate as inter
import math

def gammaFunc ( x0, V, omega, c, beta, r, R, B, dclda, cl0, clmax, clmin):
    va, vt = x0[0], x0[1]
    Wt  = omega*r - vt
    Wa  = V + va
    gamma1 = getCirculationAndLiftRelation(Wt, Wa, omega, V, r, va, vt, c, beta, dclda, cl0)
    gamma2 = getCirculationAndTangentialVelocityRelation(Wt, Wa, omega, V, r, R, B, va, vt)
    return np.asarray([gamma1 - gamma2, Wt/Wa - va/vt])

def getCirculationAndLiftRelation(Wt, Wa, omega, V, r, va, vt, c, beta, dclda, cl0):
    W       = math.sqrt(Wa*Wa + Wt*Wt)
    alpha   = beta - math.atan(Wa/Wt)
    cl      = getLiftCoefficient(W, alpha, dclda, cl0)
    return 0.5*W*c*cl

def getCirculationAndTangentialVelocityRelation(Wt, Wa, omega, V, r, R, B, va, vt):
    advanceRatio    = r*Wa/R/Wt
    f   = 0.5*B*(1 - r/R)/advanceRatio
    F   = 2*math.acos(math.exp(-f))/math.pi
    return vt*4.*math.pi*r*F*math.sqrt(1 + math.pow( (4.*advanceRatio*R/math.pi/B/r), 2))/B

def getLiftCoefficient (W, alpha, dclda, cl0):
    a       = 340.      #m/s
    Ma      = W/a
    Ma      = 0.9 if Ma > .9 else Ma
    cl      = (dclda*alpha + cl0)/math.sqrt(1. - Ma)
    if cl > clmax:
        cl  = clmax*math.cos(alpha - cl0/dclda)
    elif cl < clmin:
        cl  = clmin*math.cos(alpha - cl0/dclda)
    return cl

def getDragCoefficient(cl, Re, Ma, clcd0, cd0, cd2, reRef, reExp, mcrit):
    cdmf = 10.
    iexp = 3.
    fac = (Re/reRef)**reExp

    clb = cl - clcd0
    cd = (cd0 + cd2*clb**2)*fac

    if Ma > mcrit:
        cd = cd + cdmf*(Ma-mcrit)**iexp

    return cd

def getThrust(rho, B, W, Wa, Wt, c, dr, cl, cd):
    phi = math.atan(Wa/Wt)
    return 0.5*B*rho*W*W*(cl*math.cos(phi) - cd*math.sin(phi))*c*dr

def getTorque(rho, B, W, Wa, Wt, c, r, dr, cl, cd):
    phi = math.atan(Wa/Wt)
    return 0.5*B*rho*W*W*(cl*math.sin(phi) + cd*math.cos(phi))*c*r*dr

def linearMotorTorque(v, omega, r, Io, kv):
    kq = kv
    I = (v - omega/kv)/r
    Q = (I - Io)/kq
    return Q

def nonLinearMotorTorque(v, omega, r0, r2, Io0, Io1, Io2, kv, kq, tau):
    vm          = (1 + omega*tau)*omega/kv
    iResidual   = lambda I, v, vm, r0, r2: vm + I*(r0 + r2*I*I) - v
    iGuess      = (v - vm)/r0
    iResult     = opt.root(iResidual, iGuess, args=[v, vm, r0, r2])
    I           = iResult.x
    Io          = Io0 + Io1*omega + Io2*omega*omega
    Q           = (I - Io)/kq
    return Q

def linearMotorVoltage(Q, omega, r, Io, kv):
    qResidual   = lambda v, Q, omega, r, Io, kv: Q - linearMotorTorque(v, omega, r, Io, kv)
    vGuess      = 1.
    vResult     = opt.root(qResidual, vGuess, args=[Q, omega, r, Io, kv])
    v           = vResult.x
    return v

def nonLinearMotorVoltage(Q, omega, r0, r2, Io0, Io1, Io2, kv, kq, tau):
    qResidual   = lambda    v, Q, omega, r0, r2, Io0, Io1, Io2, kv, kq, tau:\
                            Q - nonlinearMotorTorque(\
                                v, omega, r0, r2, Io0, Io1, Io2, kv, kq, tau)
    vGuess      = 1.
    vResult     =   opt.root(\
                        qResidual,\
                        vGuess,\
                        args=[Q, omega, r0, r2, Io0, Io1, Io2, kv, kq, tau])
    v           = vResult.x
    return v
# r       = np.linspace(0.75, R, 7)
rho     = 1.225     #kg/m^3
mu      = 1.78E-5   #kg/m/s
B       = 2
R       = 3         #m
V       = 0        #m/s 72 km/hr
omega   = 14000.*math.pi/30. #rad/s
r_coarse        = 0.0254*np.asarray([0.75, 1., 1.5, 2., 2.5, 2.875, 3.])
c_coarse        = 0.0254*np.asarray([0.66, 0.69, 0.63, 0.55, 0.44, 0.30, 0.19])
betaDeg         = np.asarray([27.5, 22., 15.2, 10.2, 6.5, 4.6, 4.2])
beta_coarse     = betaDeg*math.pi/180.
c           = inter.interp1d(r_coarse, c_coarse, kind='cubic')
beta        = inter.interp1d(r_coarse, beta_coarse, kind='cubic')
r_fine      = 0.0254*np.linspace(0.75, 3., 25)
c_fine      = c(r_fine)
beta_fine   = beta(r_fine)
dr          = [r2 - r1 for r1, r2 in zip(r_fine, r_fine[1:])]
dr.insert(0, r_fine[0])
va          = r_fine*0.
vt          = r_fine*0.
gamma       = r_fine*0.

clmin   = -0.3
clmax   = 1.2
cl0     = 0.5
dclda   = 5.8#2.*math.pi

cd0     = 0.0280
cd2u    = 0.05
cd2l    = 0.05
clcd0   = 0.5
reRef   = 70000
reExp   = -0.7
mcrit   = 0.9

res = []
T   = []
Q   = []

for _r, _c, _beta, _dr in zip(r_fine, c_fine, beta_fine, dr):
    optRes = opt.root(
        gammaFunc,
        [1.,1.],
        args=(V, omega, _c, _beta, _r, R, B, dclda, cl0, clmax, clmin))
    res.append(optRes.x)
    va, vt = optRes.x[0], optRes.x[1]
    Wt  = omega*_r - vt
    Wa  = V + va
    W = math.sqrt(Wa*Wa + Wt*Wt)
    alpha = _beta - math.atan(Wa/Wt)
    cl = getLiftCoefficient(W, alpha, dclda, cl0)
    Re = rho*W*_c/mu #kg/m^3  * m/s  * ,m = kg/m/s
    Ma = W/340.
    cd = getDragCoefficient(cl, Re, Ma, clcd0, cd0, cd2u, reRef, reExp, mcrit)
    T.append(getThrust(rho, B, W, Wa, Wt, _c, _dr, cl, cd))
    Q.append(getTorque(rho, B, W, Wa, Wt, _c, _r, _dr, cl, cd))

    print _r, Wa

print sum(T), sum(Q)

# Rpm
# Volt
# Thrust
# Torque
# Amp
