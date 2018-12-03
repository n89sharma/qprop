import math
eps = 1e-6

def isClose( a, b, rel_tol=1e-9, abs_tol=0.0):
    return abs(a-b) <= max( rel_tol * max(abs(a), abs(b)), abs_tol )

def motorQ ( omega, volt, imotye, paramot, nmpar, q, dQdOmega, dQdVolt, i, didOmega, diVolt):

    PI = math.pi
    #===========================================================
    rMotor  = paramot[0]
    zLoadI  = paramot[1]
    kvRpm   = paramot[2]

    kvRad = kvRpm * PI/30.0
    kqRad = kvRad

    vm      = omega/kvRad
    vmOmega = 1.0/kvRad

    i       = (volt - vm)/rMotor
    iOmega  = -vmOmega/rMotor
    iVolt   = 1.0/rMotor

    q       = (i -zLoadI)/kqRad
    qOmega  = iOmega/kqRad
    qVolt   = iVolt/kqRad
    #===========================================================

    rMotor0     = paramot[0]
    zLoadIo0    = paramot[1]
    kvRpm       = paramot[2]
    kqRpm       = paramot[3]
    tau         = paramot[4]
    zLoadI1     = paramot[5]
    zLoadI2     = paramot[6]
    rMotor2     = paramot[7]

    if isClose(kqRpm, 0.0):
        kqRpm = kvRpm

    kvRad = kvRpm * PI/30.0
    kvRad = kqRpm * PI/30.0

    vm      = (1.0 + tau * omega) * omega   /kvRad
    vmOmega = (1.0 + tau * omega * 2.0)     /kvRad

    i = (volt - vm)/rMotor0

    convergenceOrMaxLimit = True
    iterationIndex = 1
    while convergenceOrMaxLimit:
        res = i * ( rMotor0 + rMotor0 * i ** 2) + vm - volt
        resI = rMotor0 + 3.0 * rMotor2 * i ** 2
        i = i - res/resI
        iterationIndex += 1
        if((abs(res) < eps * max(1.0, abs(volt))) or iterationIndex > 10):
            convergenceOrMaxLimit = False

    resOmega    = vmOmega
    resVolt     = -1.0
    iOmega      = -resOmega/resI
    iVolt       = -resVolt/resI

    zLoadI      = zLoadIo0 +    zLoadI1 * omega +   zLoadI2 * omega **2
    zLoadIOmega =               zLoadI2             zLoadI2 * omega * 2.0

    q       = (i - zLoadI)/kqRad
    qOmega  = (iOmega -zLoadIOmega)/kqRad
    qVolt   = iVolt/kqRad

def voltM (omega, q, imotype, parmot, nmpar, volt, voltOmega, voltQ, amps, ampsOmega, ampsQ):
    # initial guess for the newton iteration should be different for each motor
    rMotor  = parmot[0]
    zLoadI  = parmot[1]
    kvRpm   = parmot[2]
    kvRad   = kvRpm * PI/30.0
    kqRad   = kvRad
    amp     = q * kqRad + zLoadI
    volt    = amps * rMotor + omega/kvRad

    convergenceOrMaxLimit   = True
    iterationIndex          = 1
    while convergenceOrMaxLimit:
        motorQ(omega, volt, imotye, parmot, nmpar, qm, qmOmega, qmVolt, am, amOmega, amVolt)
        res     = qm - q
        resVolt = qmVolt
        dVolt   = -1.0 * res / resVolt
        iterationIndex += 1
        if((abs(dVolt) < eps * max(1.0, abs(volt))) or iterationIndex > 20):
            convergenceOrMaxLimit = False

    resOmega    = qmOmega
    resQ        = -1.0

    voltOmega   = -resOmega/ resVolt
    voltQ       = -resQ/ resVolt

    ampsOmega   = amVolt * voltOmega + amOmega
    ampsQ       = amVolt * voltQ


if __name__ == '__main__':
    MotorQ(1,2,3,(1,2,3,4,5,6,7,8),5,6,7,8,9,10,11)
