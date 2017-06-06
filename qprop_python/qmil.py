# C***********************************************************************
# C    Module:  qmil.f
# C
# C    Copyright (C) 2005 Mark Drela
# C
# C    This program is free software; you can redistribute it and/or modify
# C    it under the terms of the GNU General Public License as published by
# C    the Free Software Foundation; either version 2 of the License, or
# C    (at your option) any later version.
# C
# C    This program is distributed in the hope that it will be useful,
# C    but WITHOUT ANY WARRANTY; without even the implied warranty of
# C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# C    GNU General Public License for more details.
# C
# C    You should have received a copy of the GNU General Public License
# C    along with this program; if not, write to the Free Software
# C    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
# C***********************************************************************
def QMIL ():
# C------------------------------------------------------------------------
# C     Program implementing a modified Larrabee method for calculation
# C     of Minimum Induced Loss propeller or windmill geometry.
# C     Outputs a prop definition file to be used for subsequent
# C     off-design analysis in QPROP.
# C
# C     The modification from Larrabee consists of corrections to account
# C     for heavy disk loading.  This also makes QPROP analysis at the
# C     design point exactly match the design parameters.
# C     (The standard Larrabee design and analysis methods are
# C      consistent only to first order in the disk loading).
# C
# C   Ref:  Larrabee, E.E.
# C         "Five Years Experience with Minimum Induced Loss
# C         Propellers -- Part I: Theory"
# C         SAE Technical Paper Series  No. 840026.
# C
# C   Usage on Unix:
# C
# C       % edit mil.f     (optional, set design inputs below, or use inputfile)
# C       % make mil
# C       % mil [ inputfile outputpropfile ]
# C
# C   If inputfile isn't specified or doesn't exist,
# C   the hardwired inputs below are used.
# C
# C   If outputfile isn't specified, the output will go
# C   to the screen instead, after the usual printout.
# C
# C     Version 1.13   10 Nov 2007
# C
# C     Mark Drela      MIT Aero-Astro
# C------------------------------------------------------------------------
#   IMPLICIT REAL (A-H,M,O-Z)
#   CHARACTER*48 PNAME
#   CHARACTER*80 ARGP1, ARGP2, FILNAM
#   CHARACTER*128 LINE

    PARAMETER (IDIM=50)
    REAL G(IDIM), GAM(IDIM), CH(IDIM), XI(IDIM),
          BETA(IDIM), R(IDIM), DR(IDIM)
    REAL CL(IDIM), CD(IDIM), CLDESI(IDIM)
    REAL WORK(IDIM)
    LOGICAL DEST, DESP, LOUTF, ERROR

    REAL CL0(IDIM), DCLDA(IDIM), CLMIN(IDIM), CLMAX(IDIM)
    REAL CD0(IDIM), CD2U(IDIM), CD2L(IDIM), CLCD0(IDIM)
    REAL REREF(IDIM), REEXP(IDIM), MCRIT(IDIM)

    REAL VA(IDIM), VT(IDIM)
    LOGICAL STALL(IDIM), LCONV, LCONVE

    REAL TP_CH(IDIM), TP_BE(IDIM),
          QP_CH(IDIM), QP_BE(IDIM)
    REAL ZERO(IDIM), NEGINF(IDIM), POSINF(IDIM)

    REAL A(2,2,IDIM), B(2,2,IDIM), C(2,2,IDIM), D(2,2,IDIM)
    REAL DCH(IDIM), DBETA(IDIM)

    PARAMETER (JDIM=100)
    REAL XIDES(JDIM), CLDES(JDIM), CLDESX(JDIM)

    REAL RVAL(JDIM)
    INTEGER IVAL(JDIM)

    INCLUDE 'QDEF.INC'

    PI = math.pi
    SQRT = math.sqrt
    EXP = math.exp
    ATAN2 = math.atan2
    MAX = math.max
    COS = math.cos
    SIN = math.sin
    ASIN = math.asin
    ACOS = math.acos
    ATAN = math.atan
    LOG = math.log
    ABS = abs
    MAX = max
    MIN = min

    EPS = 1.0e-6
    VERSION = 1.13

    # C---- get Unix command-line arguments, if any
     GETARG0(1,ARGP1)
     GETARG0(2,ARGP2)

    WRITE(*,1100) VERSION
    1100 FORMAT(/' QMIL Version', F5.2)

    # C---- set default design option flag
    LDES = 0
    FQDES = 0.0     #  0 = true max,  0..1 sub-maximum factor
    NITERG = 20

# C=========================================================================
# C---- set default input parameters (in SI or any consistent set of units)

    PNAME = 'Default prop'
    BLDS = 2.0     # number of blades
    N = 20         # number of spanwise integration points

    for I in range(1, 21):
        CL0[I] =  0.4     # blade airfoil zero-alpha  CL
        DCLDA[I] = 2.0*PI # blade airfoil dCL/dalpha   (1/rad)
        CLMIN[I] = -0.8   # stall limits
        CLMAX[I] =  1.2   #

        CD0[I] =  0.020   # design blade airfoil CD parameters
        CD2U[I] =  0.030  #
        CD2L[I] =  0.030  #
        CLCD0[I] = 0.3    #  CD = [ CD0 + CD2*(CL-CLCD0)^2 ] * (Re/REREF)^REEXP
        REREF[I] = 1.0E5  #
        REEXP[I] = -0.5   #

        ZERO[I] = 0.
        NEGINF[I] = -1.0E9
        POSINF[I] =  1.0E9

        MCRIT[I] = 0.7

    NDES = 3

    XIDES[1] = 0.0   # locations where design cl is specified
    XIDES[2] = 0.5   #
    XIDES[3] = 1.0   #

    CLDES[1] = 0.6   # design cl at r/R = XIDES1
    CLDES[2] = 0.4   # design cl at r/R = XIDES2
    CLDES[3] = 0.3   # design cl at r/R = XIDES3

    RHUB = 0.05    # prop hub radius  (m)
    RAD = 0.65     # prop tip radius  (m)
    VEL = 20.0     # speed            (m/s)
    RPM = 3000.0   # prop rpm

    TDES = 0.0        # thrust (N)   (set to 0.0 if power  specified)
    PDES = 10000.0    # power  (W)   (set to 0.0 if thrust specified)

# C---- default fluid properties from QDEF.INC
    RHO = RHO1   # density
    RMU = RMU1   # viscosity
    VSO = VSO1   # speed of sound
    # c      RHO = 1000.0   # fluid density   (kg/m^3)
    # c      RMU = 1.15E-3  # dyn. viscosity  (kg/m-s)
    # c      VSO = 1500.0   # speed of sound  (m/s)
    QCGET(RHO,RMU,VSO)

    # C=========================================================================
    # C---- read prop design data file
    # TODO GO TO 5 has to be taken out
    if ( ARGP1 == ' ' ) GO TO 5

    FILNAM = ARGP1
    LU = 1
    OPEN(LU,FILE=FILNAM,STATUS='OLD',ERR=5)
    ILINE = 0

    # C---- prop name
    FREAD(LU,LINE,ILINE,IERR,PNAME)
    # if(isClose(IERR,+1)) GO TO 900
    # if(isClose(IERR,-1)) GO TO 950

    # C---- extract parameters on data lines
    NV = 1
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    BLDS = RVAL[1]

    NV = 2
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    CL0B   = RVAL[1]
    DCLDAB = RVAL[2]

    NV = 2
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    CLMINB = RVAL[1]
    CLMAXB = RVAL[2]

    NV = 4
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    CD0B   = RVAL[1]
    CD2UB  = RVAL[2]
    CD2LB  = RVAL[3]
    CLCD0B = RVAL[4]

    NV = 2
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    REREFB = RVAL[1]
    REEXPB = RVAL[2]

    NV = 1
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    NDES1 = JDIM
    GETFLT(LINE,RVAL,NDES1,ERROR)
    # IF(ERROR) GO TO 900
    for J in range( 1, NDES1):
        XIDES(J) = RVAL(J)

    NV = 1
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    NDES2 = JDIM
    GETFLT(LINE,RVAL,NDES2,ERROR)
    # if(ERROR) GO TO 900
    for J in range( 1, NDES2):
        CLDES(J) = RVAL(J)


    NDES = MIN( NDES1 , NDES2 )
    if(isClose(NDES, 1)):
        # C----- set up for constant-cl case
        XIDES[1] = 0.
        XIDES[2] = 1.0
        CLDES[2] = CLDES[1]
        NDES = 2


    NV = 1
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    RHUB = RVAL[1]

    NV = 1
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    RAD = RVAL[1]

    NV = 1
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    VEL = RVAL[1]

    NV = 1
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950

    if(RVAL[1] > 0.0):
        RPM = RVAL[1]
        ADV = VEL/(RAD*RPM*PI/30.0)
    else:
        ADV = -RVAL[1]
        RPM = (30.0/PI) * (VEL/RAD) / ADV



    NV = 1
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    TDES = RVAL[1]

    NV = 1
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    PDES = RVAL[1]

    NV = 2
    RREAD(LU,LINE,ILINE,IERR,NV,RVAL)
    # IF(IERR.EQ.+1) GO TO 900
    # IF(IERR.EQ.-1) GO TO 950
    LDES = INT( RVAL[1] + 0.01 )
    if(NV >= 2):
        FQDES = RVAL[2]
    else:
        FQDES = 0.0


    NV = 1
    IREAD(LU,LINE,ILINE,IERR,NV,IVAL)
    # IF(IERR.EQ.+1) GO TO 900
    if(isClose(IERR, -1)):
        N = MAX( 2 , MIN( IDIM , IVAL[1] ) )


    for I in range( 1, N):
        CL0[I]   = CL0B
        DCLDA[I] = DCLDAB
        CLMIN[I] = CLMINB
        CLMAX[I] = CLMAXB
        CD0[I]   = CD0B
        CD2U[I]  = CD2UB
        CD2L[I]  = CD2LB
        CLCD0[I] = CLCD0B
        REREF[I] = REREFB
        REEXP[I] = REEXPB
        MCRIT[I] = 0.7

        ZERO[I] = 0.
        NEGINF[I] = -1.0E9
        POSINF[I] =  1.0E9


    CLOSE(LU)
# C=========================================================================

# 5    CONTINUE

    SEGSPL(CLDES,CLDESX,XIDES,NDES)


    # c      do 50 irad = 10, 20
    # ccc      do irad = 15, 15
    # c        rad = 0.001*float(irad)
    # c        write(irad,4444)
    # c 4444   format(
    # c     &'#   rpm           R               Q              P         ',
    # c     &  '    V/wR')
    # c        do 40 irpm = 4, 14
    # ccc        do irpm = 10, 10
    # c          rpm = 1000.0*float(irpm) * float(12)/float(irad)
    # c          ADV = VEL/(RAD*RPM*PI/30.0)
    # c          pdes = -0.5*rho*vel**3 * pi*rad**2 * 0.2

    # C---- set radial CL,CD distributions
    XI0 = RHUB/RAD
    XI1 = 1.0

    DXI = (XI1 - XI0)/FLOAT(N)
    for I in range(1, N + 1):
        XI[I] = XI0 + DXI*(FLOAT[I]-0.5)
        R[I]  = RAD*XI[I]
        DR[I] = RAD*DXI

        CL[I] = SEVAL(XI[I],CLDES,CLDESX,XIDES,NDES)
        if(CL[I] > CLCD0[I]):
            CD[I] = CD0[I] + CD2U[I]*(CL[I]-CLCD0[I])**2
        else:
            CD[I] = CD0[I] + CD2L[I]*(CL[I]-CLCD0[I])**2


        # C------ save design CL for Newton iteration below
        CLDESI[I] = MAX( CLMIN[I]+EPS , MIN( CLMAX[I]-EPS , CL[I] ) )



    OMG = RPM*PI/30.0

    # C---- initial geometry guess using modified Larrabee method
    if(isClose(LDES, 2)):
        TDES = 0.
        PDES = 0.


# c      write(*,*) ldes, tdes, pdes

   TPDES(TDES,PDES, RHO,VEL,OMG, RAD,BLDS,
            N, R,DR,CLDESI,CD, CL0,DCLDA,
            GAM, CH,BETA, ADW)
    DBE = 0.0

    # cC---- set Re, and improved cd
    # c      DO I = 1, N
    # c        WSQ = VEL**2 + (OMG*R[I])**2
    # c        RE = CH[I]*SQRT(WSQ)*RHO/RMU
    # c        CDFAC = (RE/REREF[I])**REEXP[I]
    # cC
    # c        CL[I] = CLDESI[I]
    # c        IF(CL[I].GT.CLCD0[I]) THEN
    # c         CD[I] = (CD0[I] + CD2U[I]*(CL[I]-CLCD0[I])**2)*CDFAC
    # c        ELSE
    # c         CD[I] = (CD0[I] + CD2L[I]*(CL[I]-CLCD0[I])**2)*CDFAC
    # c        ENDIF
    # c      ENDDO
    # cC
    # c       TPDES(TDES,PDES, RHO,VEL,OMG, RAD,BLDS,
    # c     &           N, R,DR,CLDESI,CD, CL0,DCLDA,
    # c     &           GAM, CH,BETA, ADW)
    # c      DBE = 0.0

    # ccc      go to 105  #c@@@


    # C---- initialize EFF global variable
    if(isClose(LDES, 0)):
    # C----- compute T and Q with zero cd to get induced efficiency
        TQCALC(N,CH,BETA,R,DR,
                  VA,VT,CL,CD,STALL,
                  BLDS,RAD,VEL,OMG,DBE,
                   RHO,RMU,VSO,
                   CL0,DCLDA,NEGINF,POSINF,MCRIT,
                   ZERO,ZERO,ZERO,CLCD0,REREF,REEXP,
                   TP, TP_VEL, TP_OMG, TP_DBE, TP_CH, TP_BE,
                   QP, QP_VEL, QP_OMG, QP_DBE, QP_CH, QP_BE )
    else:
    # C----- compute T and Q with actual cd to get total efficiency
        TQCALC(N,CH,BETA,R,DR,
               VA,VT,CL,CD,STALL,
               BLDS,RAD,VEL,OMG,DBE,
               RHO,RMU,VSO,
               CL0,DCLDA,NEGINF,POSINF,MCRIT,
               CD0,CD2U,CD2L,CLCD0,REREF,REEXP,
               TP, TP_VEL, TP_OMG, TP_DBE, TP_CH, TP_BE,
               QP, QP_VEL, QP_OMG, QP_DBE, QP_CH, QP_BE )

    EFF = TP*VEL / (QP*OMG)
    TOP = TP     / (QP*OMG)


    print '-----------------------------------------------------'
    print ' Nonlinear design iteration...'

    # C---- iterate using QPROP's local induced velocity formulation
    for ITERG in range(1, NITERG + 1):
        for I in range(1, N+1):
            # C-------- perturbation imposed axial and tangential velocities
            U0A = 0.
            U0T = 0.

            # C-------- total imposed axial and tangential velocities
            UA     = VEL   + U0A
            UA_VEL = 1.0
            UA_U0A =         1.0

            UT     = OMG*R[I] - U0T
            UT_OMG =     R[I]
            UT_U0T =          - 1.0

            GVCALC(CH[I],BETA[I],R[I],
                BLDS,RAD,VEL,OMG,VSO,
                CL0[I],DCLDA[I],NEGINF[I],POSINF[I],MCRIT[I],
                GAM[I],GAM_VEL,GAM_OMG,GAM_CH,GAM_BET,
                VA[I] , VA_VEL, VA_OMG, VA_CH, VA_BET,
                VT[I] , VT_VEL, VT_OMG, VT_CH, VT_BET,
                CL[I] , CL_VEL, CL_OMG, CL_CH, CL_BET, STALL[I], LCONV)

            WA = UA + VA[I]
            WT = UT - VT[I]

            WA_BET = VA_BET
            WA_CH  = VA_CH

            WT_BET = -VT_BET
            WT_CH  = -VT_CH

            # C-------- total velocity
            WSQ = WA**2 + WT**2
            WSQ_BET = 2.0*WA*WA_BET + 2.0*WT*WT_BET
            WSQ_CH  = 2.0*WA*WA_CH  + 2.0*WT*WT_CH

            W = SQRT(WSQ)
            W_BET = 0.5*WSQ_BET/W
            W_CH  = 0.5*WSQ_CH /W

            # C-------- chord Reynolds number
            RE     = RHO*CH[I]*W    /RMU
            RE_BET = RHO*CH[I]*W_BET/RMU
            RE_CH  = RHO*CH[I]*W_CH /RMU + RHO      *W    /RMU


            MA = 0.

            if(isClose(LDES, 0)):
                CD[I]  = 0.0
                CD_BET = 0.
                CD_CH  = 0.

            else (isClose(LDES, 1) or isClose(LDES, 2) or isClose(LDES, -2)):


                if(CL[I] >= CLCD0[I]):
                    CDFUN(CL[I],RE,MA,
                    CLCD0[I],CD0[I],CD2U[I],REREF,REEXP[I],MCRIT[I],
                    CD[I],CD_CL,CD_RE,CD_MA)
                else:
                    CDFUN(CL[I],RE,MA,
                    CLCD0[I],CD0[I],CD2L[I],REREF,REEXP[I],MCRIT[I],
                    CD[I],CD_CL,CD_RE,CD_MA)


                CD_BET = CD_CL*CL_BET + CD_RE*RE_BET
                CD_CH  = CD_CL*CL_CH  + CD_RE*RE_CH



# c         ADWI = (WA/WT) * XI[I]
# c         EFFI = (ADV/XI[I]) * (WT/WA) = ADV/ADWI
# c         EFFP = (CL[I] - CD[I]*WA/WT)
# c     &        / (CL[I] + CD[I]*WT/WA)
# c         EFF  = (CL[I]*WT - CD[I]*WA)
# c     &        / (CL[I]*WA + CD[I]*WT)
# c     &        * (ADV/XI[I])

            # C-------- residual 1: impose design CL
            D(1,1,I) = CL[I] - CLDESI[I]
            D(1,2,I) = 0.
            A(1,1,I) = CL_BET
            A(1,2,I) = CL_CH

# C-------- residual 2: impose constant local efficiency
            if(isClose(LDES, 0) or isClose(LDES, 1)):
                D(2,1,I) = TOP*UT*( CL[I] *WA     + CD[I] *WT    )#*CH[I]
                -     ( CL[I] *WT     - CD[I] *WA    )#*CH[I]

                D(2,2,I) =     UT*( CL[I] *WA     + CD[I] *WT    )#*CH[I]

                A(2,1,I) = TOP*UT*( CL_BET*WA     + CD_BET*WT
                         + CL[I] *WA_BET + CD[I] *WT_BET)#*CH[I]
                   -     ( CL_BET*WT     - CD_BET*WA
                         + CL[I] *WT_BET - CD[I] *WA_BET)#*CH[I]

                A(2,2,I) = TOP*UT*( CL_CH *WA     + CD_CH *WT
                         + CL[I] *WA_CH  + CD[I] *WT_CH )#*CH[I]
                   -     ( CL_CH *WT     - CD_CH *WA
                         + CL[I] *WT_CH  - CD[I] *WA_CH )#*CH[I]
# c     &              + EFF*UT*( CL[I] *WA     + CD[I] *WT    )
# c     &                 - VEL*( CL[I] *WT     - CD[I] *WA    )
            elif(isClose(LDES, 2)):
                T1     = (WA     - 0.5*UA   ) / (WT - UT)
                T1_BET = (WA_BET - T1*WT_BET) / (WT - UT)
                T1_CH  = (WA_CH  - T1*WT_CH ) / (WT - UT)

                T2     = (  CL[I] *(WT    - 0.5*UT)
                    - CD[I] *(WA    - 0.5*UA) )/ (  CL[I] * WA    + CD[I] * WT )
                T2_BET = (  CL[I] * WT_BET
                         - CD[I] * WA_BET
                         + CL_BET*(WT    - 0.5*UT)
                         - CD_BET*(WA    - 0.5*UA)
                         - T2
                          *(  CL[I]* WA_BET + CD[I]* WT_BET
                            + CL_BET*WA     + CD_BET*WT    ) )
                      / (  CL[I] * WA    + CD[I] * WT )
                T2_CH  = (  CL[I] * WT_CH
                         - CD[I] * WA_CH
                         + CL_CH *(WT    - 0.5*UT)
                         - CD_CH *(WA    - 0.5*UA)
                         - T2
                          *(  CL[I]* WA_CH  + CD[I]* WT_CH
                            + CL_CH *WA     + CD_CH *WT    ) )
                      / (  CL[I] * WA    + CD[I] * WT )

# c           T3     = (- CL[I] *(WA    - 0.5*UA)
# c     &               - CD[I] *(WT    - 0.5*UT) )
# c     &            / (  CL[I] * WT    - CD[I] * WA )
# c           T3_BET = (- CL[I] * WA_BET
# c     &               - CD[I] * WT_BET
# c     &               - CL_BET*(WA    - 0.5*UA)
# c     &               - CD_BET*(WT    - 0.5*UT)
# c     &               - T3
# c     &                *(  CL[I]* WT_BET - CD[I]* WA_BET
# c     &                  + CL_BET*WT     - CD_BET*WA    ) )
# c     &            / (  CL[I] * WT    - CD[I] * WA )
# c           T3_CH  = (- CL[I] * WA_CH
# c     &               - CD[I] * WT_CH
# c     &               - CL_CH *(WA    - 0.5*UA)
# c     &               - CD_CH *(WT    - 0.5*UT)
# c     &               - T3
# c     &                *(  CL[I]* WT_CH  - CD[I]* WA_CH
# c     &                  + CL_CH *WT     - CD_CH *WA    ) )
# c     &            / (  CL[I] * WT    - CD[I] * WA )

                T4     = (WA     - UA       )/(WT    - 0.5*UT)
                T4_BET = (WA_BET - T4*WT_BET)/(WT    - 0.5*UT)
                T4_CH  = (WA_CH  - T4*WT_CH )/(WT    - 0.5*UT)

# c           D(2,1,I) = (T2     - T1    )  -  FQDES*(T3     - T1    )
# c           D(2,2,I) = 0.
# cC
# c           A(2,1,I) = (T2_BET - T1_BET)  -  FQDES*(T3_BET - T1_BET)
# c           A(2,2,I) = (T2_CH  - T1_CH )  -  FQDES*(T3_CH  - T1_CH )


                D(2,1,I) = (T2     - T1    )*T4  -  FQDES
                D(2,2,I) = 0.

                A(2,1,I) = (T2_BET - T1_BET)*T4 + (T2 - T1)*T4_BET
                A(2,2,I) = (T2_CH  - T1_CH )*T4 + (T2 - T1)*T4_CH

            elif(isClose(LDES, -2)):
                D(2,1,I) =
                (  CL[I] * WA
                + CD[I] * WT         )*(WA - 0.5*UA)
                - (  CL[I] *(WT -0.5*UT)
                - CD[I] *(WA -0.5*UA))*(WT - UT    )
                D(2,2,I) = 0.

                A(2,1,I) =
                (  CL[I] * WA_BET
                + CD[I] * WT_BET
                + CL_BET*WA
                + CD_BET*WT          )*(WA - 0.5*UA)
                - (  CL[I]* WT_BET
                - CD[I]* WA_BET
                + CL_BET*(WT- 0.5*UT)
                - CD_BET*(WA- 0.5*UA))*(WT - UT    )
                + (  CL[I]* WA
                + CD[I]* WT          )* WA_BET
                - (  CL[I]*(WT - 0.5*UT)
                - CD[I]*(WA - 0.5*UA))* WT_BET

                A(2,2,I) =
                (  CL[I] * WA_CH
                + CD[I] * WT_CH
                + CL_CH *WA
                + CD_CH *WT          )*(WA - 0.5*UA)
                - (  CL[I]* WT_CH
                - CD[I]* WA_CH
                + CL_CH*(WT - 0.5*UT)
                - CD_CH*(WA - 0.5*UA))*(WT - UT    )
                + (  CL[I]* WA
                + CD[I]* WT          )* WA_CH
                - (  CL[I]*(WT - 0.5*UT)
                - CD[I]*(WA - 0.5*UA))* WT_CH


            B(1,1,I) = 0.
            B(1,2,I) = 0.
            B(2,1,I) = 0.
            B(2,2,I) = 0.

            C(1,1,I) = 0.
            C(1,2,I) = 0.
            C(2,1,I) = 0.
            C(2,2,I) = 0.

        #ENDDO

        BNSOLV(A,B,C,D,2,N,2,2)

        TQCALC(N,CH,BETA,R,DR,
                VA,VT,CL,CD,STALL,
                BLDS,RAD,VEL,OMG,DBE,
                RHO,RMU,VSO,
                CL0,DCLDA,NEGINF,POSINF,MCRIT,
                CD0,CD2U,CD2L,CLCD0,REREF,REEXP,
                TP, TP_VEL, TP_OMG, TP_DBE, TP_CH, TP_BE,
                QP, QP_VEL, QP_OMG, QP_DBE, QP_CH, QP_BE )


# c        write(*,*) 'P =', qp*omg
# c        write(*,*) 'Q =', qp
# c        write(*,*) 'T =', tp

# C----------------------------------------------------
        if(isClose(LDES, 0) or isClose(LDES, 1)):
# C------- MIL or MTL case...  solve for T/P Newton change

            DSUM = 0.
            ESUM = 0.

            if(!isClose(TDES, 0.0)):
                RES = TP - TDES

# ccc         write(*,*) 'T, Tdes', tp, tdes

                for I in range(1, N+1)
                    DSUM = DSUM + TP_BE[I]*D(1,1,I)
                                + TP_CH[I]*D(2,1,I)
                    ESUM = ESUM + TP_BE[I]*D(1,2,I)
                                + TP_CH[I]*D(2,2,I)

            else:
                RES = QP - PDES/OMG

# cc         write(*,*) 'Q, Qdes', qp, qdes/omg

                for I in range(1, N+1):
                    DSUM = DSUM + QP_BE[I]*D(1,1,I)
                                + QP_CH[I]*D(2,1,I)
                    ESUM = ESUM + QP_BE[I]*D(1,2,I)
                                + QP_CH[I]*D(2,2,I)
#
            #ENDIF
#
            DTOP = (RES - DSUM)/ESUM

# C----------------------------------------------------



        elif(isClose(LDES,2)):
# C------- MTP case... T/P is not a variable
            DTOP = 0.
#
        #ENDIF
# C----------------------------------------------------
# cc        write(*,*) 'R, dsum, esum, dtop', res, dsum, esum, dtop
# cc        pause
#
        DBMAX = 0.
        DCMAX = 0.
        IBMAX = 0
        ICMAX = 0
        RLX = 1.0
        for I in range(1, N):
            DBETA[I] = -D(1,1,I) - DTOP*D(1,2,I)
            DCH[I]   = -D(2,1,I) - DTOP*D(2,2,I)

            if(ABS(DBETA[I]) > ABS(DBMAX)):
                DBMAX = DBETA[I]
                IBMAX = I

            if(ABS(DCH[I]  ) > ABS(DCMAX)):
                DCMAX = DCH[I]
                ICMAX = I

# c          IF(RLX*DBETA[I] .LT. -0.03     ) RLX = -0.03/DBETA[I]
# c          IF(RLX*DBETA[I] .GT.  0.03     ) RLX =  0.03/DBETA[I]

            if(RLX*DBETA[I] < -0.05     ):
                RLX = -0.05/DBETA[I]
            if(RLX*DBETA[I] >  0.05     ):
                RLX =  0.05/DBETA[I]

# c          IF(RLX*DBETA[I] .LT. -0.10     ) RLX = -0.10/DBETA[I]
# c          IF(RLX*DBETA[I] .GT.  0.10     ) RLX =  0.10/DBETA[I]

            if(RLX*DCH[I] < -0.8*CH[I]):
                RLX = -0.8*CH[I]/DCH[I]
            if(RLX*DCH[I] >  3.0*CH[I]):
                RLX =  3.0*CH[I]/DCH[I]
        #ENDDO

        DTOPV = DTOP * SQRT(VEL**2 + (OMG*RAD)**2)
        print ITERG, DCMAX/RAD, ICMAX,DBMAX*180.0/PI, IBMAX, DTOPV, RLX




        for I in range(1, N+1):
            BETA[I] = BETA[I] + RLX*DBETA[I]
            CH[I]   = CH[I]   + RLX*DCH[I]


        TOP = TOP + RLX*DTOP

        LCONVE = ABS(DCMAX)/RAD < EPS and ABS(DBMAX) < EPS and ABS(DTOPV) < EPS
        if(LCONVE):
            break

    if(!LCONVE):
        print 'c,beta convergence failed'





    # C---- compute T and Q with actual cd to get total efficiency
    TQCALC(N,CH,BETA,R,DR,
               VA,VT,CL,CD,STALL,
               BLDS,RAD,VEL,OMG,DBE,
               RHO,RMU,VSO,
               CL0,DCLDA,NEGINF,POSINF,MCRIT,
               CD0,CD2U,CD2L,CLCD0,REREF,REEXP,
               TP, TP_VEL, TP_OMG, TP_DBE, TP_CH, TP_BE,
               QP, QP_VEL, QP_OMG, QP_DBE, QP_CH, QP_BE )
    EFF = TP*VEL / (QP*OMG)


    # c      write(irad,5566) rpm, rad, qp, qp*omg, adv
    # c 5566 format(1x,8g15.6)
    # c 40   continue
    # c 50   continue


    # C-------------------------------------------------------------------
    NBLDS = INT(BLDS+0.01)
    RPM = OMG*30.0/PI
    PP = QP*OMG
    print NBLDS, RAD, VEL, OMG, RPM, TP, QP, PP, EFF, RHO, RMU, VSO
# 1260 FORMAT(/'   B =', I4,
        #  /'   R =', G12.4,' m'
        #  /'   V =', G12.4,' m/s'
        #  /'   w =', G12.4,' rad/s'
        #  /'   w =', G12.4,' rpm'
        # //'   T =', G12.4,' N'
        #  /'   Q =', G12.4,' N-m'
        #  /'   P =', G12.4,' W'
        #  /' eff =', G12.4,
        # //'   rho =', G12.4,' kg/m^3'
        #  /'   mu  =', G12.4,' kg/m-s'
        #  /'   a   =', G12.4,' m/s' )

#
   #  WRITE(*,*)
   #  WRITE(*,1280)
   # '   r/R    phi     c/R    beta  ',
   # '   CL     CD    Mach     Re     adw_local  effi    effp    eff',
   # '    T_c      Q_c  '
# CCC     radius   total  flow    blade   blade
# CCC               vel.  angle   chord   angle
# 1280 FORMAT(1X,A,A,A)
    for I in range(1, N+1):
        # C------ perturbation imposed axial and tangential velocities
        U0A = 0.
        U0T = 0.

        # C------ total imposed axial and tangential velocities
        UA = VEL      + U0A
        UT = OMG*R[I] - U0T

        # C------ total velocity
        WA = UA + VA[I]
        WT = UT - VT[I]

        WSQ = WA**2 + WT**2
        W = SQRT(WSQ)

        PHI = ATAN2( WA , WT )

        if(!isClose(WA, 0.0) and !isClose(WT, 0.0)):
            ADWI = (WA/WT) * XI[I]
            EFFI = (ADV/XI[I]) * (WT/WA)
            EFFP = (CL[I] - CD[I]*WA/WT)
               / (CL[I] + CD[I]*WT/WA)
            EFF  = (CL[I]*WT - CD[I]*WA)
               / (CL[I]*WA + CD[I]*WT)
               * (ADV/XI[I])
#
        else:
            ADWI = 0.
            EFFI = 0.
            EFFP = 0.
            EFF = 0.


        EFFI = MAX( -99.0 , MIN( 99.0 , EFFI ) )
        EFFP = MAX( -99.0 , MIN( 99.0 , EFFP ) )
        EFF  = MAX( -99.0 , MIN( 99.0 , EFF  ) )

        REC = W * CH[I] * RHO/RMU

        PDEG = PHI    *180.0/PI
        BDEG = BETA[I]*180.0/PI

        COR = CH[I]/RAD

        T_C = TP_CH[I] * RAD/TP
        Q_C = QP_CH[I] * RAD/QP

        print XI[I], PDEG, COR,  BDEG , CL[I], CD[I],
              W/VSO, REC , ADWI, EFFI, EFFP, EFF,
              T_C, Q_C
# 1300   FORMAT(2X,F5.3 , F7.2, F8.4, F7.2, F7.3, F9.5,
            #   F7.3 , F10.0,F8.4, F8.4, F8.4, F8.4,
            #  2F9.5)
    #ENDDO


    # WRITE(*,*)
    print 'Outputting prop definition in QPROP format...'
    # WRITE(*,*)

    LOUTF = ARGP2 != ' '
    if(LOUTF):
        FILNAM = ARGP2
        LU2 = 2
        OPEN(LU2,FILE=FILNAM,STATUS='UNKNOWN')
        REWIND(LU2)
    else:
        LU2 = 6


    RFAC = 1.0
    CFAC = 1.0
    BFAC = 1.0
    RADD = 0.
    CADD = 0.
    BADD = 0.
    I = 1
    # WRITE(LU2,*)
    print PNAME
    print INT(BLDS+0.01)
    print CL0[I], DCLDA[I], CLMIN[I], CLMAX[I]
    print CD0[I], CD2U[I], CD2L[I], CLCD0[I], REREF[I], REEXP[I]
    print RFAC, CFAC, BFAC, RADD, CADD, BADD

# 1000 FORMAT(A)

# 2100 FORMAT(/1X,I3,'       # Nblades')
# 2200 FORMAT(/1X,2F8.4, '    # CL0    CL_a '
        #  /1X,2F8.4, '    # CLmin  CLmax' )
# 2300 FORMAT(/1X,3F9.5,F8.4,  '    # CD0    CD2u  CD2l  CLCD0'
        #  /1X,F10.1,F8.3,8X,'    # REref  REexp' )
# 2400 FORMAT(/1X,3F8.4, '   #  Rfac   Cfac   Bfac'
        #  /1X,3F8.4, '   #  Radd   Cadd   Badd' )

# C---- extrapolate geometry to root and tip radii
    XI0 = MAX( 2.0*XI[1] - XI[2] , 0.0 )
    SPLINE(CH[1],WORK[1],XI[1],3)
    CH0 = SEVAL(XI0,CH[1],WORK[1],XI[1],3)

    SPLINE(BETA[1],WORK[1],XI[1],3)
    BE0 = SEVAL(XI0,BETA[1],WORK[1],XI[1],3)

    XI1 = 1.0
    SPLINE(CH(N-2),WORK(N-2),XI(N-2),3)
    CH1 = SEVAL(XI1,CH(N-2),WORK(N-2),XI(N-2),3)

    SPLINE(BETA(N-2),WORK(N-2),XI(N-2),3)
    BE1 = SEVAL(XI1,BETA(N-2),WORK(N-2),XI(N-2),3)

    CH0 = MAX( CH0 , 0.0 )
    CH1 = MAX( CH1 , 0.0 )

    # WRITE(LU2,*)
    print '#        r            c        beta'
# ccc      WRITE(LU2,2500) RAD*XI0, CH0, BE0*180.0/PI
    for I in range(1, N+1):
        print RAD*XI[I], CH[I], BETA[I]*180.0/PI

    print RAD*XI1, CH1, BE1*180.0/PI
# 2500 FORMAT(1X,2G13.5,F9.4)

    if(LOUTF):
        # CLOSE(LU2)
        # WRITE(*,*)
        print 'QPROP propfile written: ', FILNAM
