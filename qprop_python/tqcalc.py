import math
def TQCALC ( N,C,B,R,DR,VA,VT,CL,CD,STALL,BLDS,RAD,VEL,OMG,DBE,RHO,RMU,VSO,
    CL0,DCLDA,CLMIN,CLMAX,MCRIT,CD0,CD2U,CD2L,CLCD0,REREF,REEXP, TP, TP_VEL,
    TP_OMG, TP_DBE, TP_C, TP_B, QP, QP_VEL, QP_OMG, QP_DBE, QP_C, QP_B ):

    msqMax = 0.9

      TP     = 0.
      TP_VEL = 0.
      TP_OMG = 0.
      TP_DBE = 0.

      QP     = 0.
      QP_VEL = 0.
      QP_OMG = 0.
      QP_DBE = 0.


    for I in range(1, N + 1):
        U0A = 0.
        U0T = 0.
        GAM_U0A = 0.
        GAM_U0T = 0.
        VA_U0A = 0.
        VA_U0T = 0.
        VT_U0A = 0.
        VT_U0T = 0.
        CL_U0A = 0.
        CL_U0T = 0.

        #------ total imposed axial and tangential velocities
        UA     = VEL   + U0A
        UA_VEL = 1.0
        UA_U0A = 1.0

        UT     = OMG * R[I] - U0T
        UT_OMG =       R[I]
        UT_U0T =          - 1.0

        #------ geometric velocity
        WZ      = math.sqrt(UA ** 2 + UT ** 2)
        WZ_VEL  = (UA/WZ)*UA_VEL
        WZ_OMG  = (UT/WZ)*UT_OMG

        #------ total axial and tangential velocities
        WA     = UA     + VA[I]
        WA_VEL = UA_VEL + VA_VEL
        WA_OMG =          VA_OMG
        WA_B   =          VA_B
        WA_C   =          VA_C
        WA_U0A = UA_U0A + VA_U0A
        WA_U0T =          VA_U0T
        C
        WT     = UT     - VT[I]
        WT_VEL =        - VT_VEL
        WT_OMG = UT_OMG - VT_OMG
        WT_B   =        - VT_B
        WT_C   =        - VT_C
        WT_U0A =        - VT_U0A
        WT_U0T = UT_U0T - VT_U0T

        #------ total velocity^2
        WSQ = WA**2 + WT**2
        WSQ_VEL = 2.0*WA*WA_VEL + 2.0*WT*WT_VEL
        WSQ_OMG = 2.0*WA*WA_OMG + 2.0*WT*WT_OMG
        WSQ_B   = 2.0*WA*WA_B   + 2.0*WT*WT_B
        WSQ_C   = 2.0*WA*WA_C   + 2.0*WT*WT_C
        WSQ_U0A = 2.0*WA*WA_U0A + 2.0*WT*WT_U0A
        WSQ_U0T = 2.0*WA*WA_U0T + 2.0*WT*WT_U0T

        #------ total velocity
        W = math.sqrt(WSQ)
        W_VEL = 0.5*WSQ_VEL/W
        W_OMG = 0.5*WSQ_OMG/W
        W_B   = 0.5*WSQ_B  /W
        W_C   = 0.5*WSQ_C  /W
        W_U0A = 0.5*WSQ_U0A/W
        W_U0T = 0.5*WSQ_U0T/W

        #------ chord Reynolds number
        RE     = RHO*C[I]*W    /RMU
        RE_VEL = RHO*C[I]*W_VEL/RMU
        RE_OMG = RHO*C[I]*W_OMG/RMU
        RE_B   = RHO*C[I]*W_B  /RMU
        RE_C   = RHO*C[I]*W_C  /RMU + RHO*W/RMU
        RE_U0A = RHO*C[I]*W_U0A/RMU
        RE_U0T = RHO*C[I]*W_U0T/RMU

        #------ local Mach and PG factor
        MSQ     = WSQ     / VSO**2
        MSQ_VEL = WSQ_VEL / VSO**2
        MSQ_OMG = WSQ_OMG / VSO**2
        MSQ_B   = WSQ_B   / VSO**2
        MSQ_C   = WSQ_C   / VSO**2
        MSQ_U0A = WSQ_U0A / VSO**2
        MSQ_U0T = WSQ_U0T / VSO**2

        if(MSQ > MSQMAX):
            MSQ = MSQMAX
            MSQ_VEL = 0.
            MSQ_OMG = 0.
            MSQ_B   = 0.
            MSQ_C   = 0.
            MSQ_U0A = 0.
            MSQ_U0T = 0.

            PG      = 1.0 / math.sqrt(1.0 - MSQ)
            PG_MSQ  = 0.5*PG / (1.0 - MSQ)

            PG_VEL = PG_MSQ*MSQ_VEL
            PG_OMG = PG_MSQ*MSQ_OMG
            PG_B   = PG_MSQ*MSQ_B
            PG_C   = PG_MSQ*MSQ_C
            PG_U0A = PG_MSQ*MSQ_U0A
            PG_U0T = PG_MSQ*MSQ_U0T

            MA      = math.sqrt(MSQ)
            MA      = max( MA , 1.0E-8 )
            MA_VEL  = (0.5/MA)*MSQ_VEL
            MA_OMG  = (0.5/MA)*MSQ_OMG
            MA_B    = (0.5/MA)*MSQ_B
            MA_C    = (0.5/MA)*MSQ_C
            MA_U0A  = (0.5/MA)*MSQ_U0A
            MA_U0T  = (0.5/MA)*MSQ_U0T

        #------ set unstalled CD
        if(CL[I] > CLCD0[I]):
            CDFUN(CL[I],RE,MA,CLCD0[I],CD0[I],CD2U[I],REREF,REEXP[I],MCRIT,CD[I],CD_CL,CD_RE,CD_MA)
        else:
            CDFUN(CL[I],RE,MA,CLCD0[I],CD0[I],CD2L[I],REREF,REEXP[I],MCRIT,CD[I],CD_CL,CD_RE,CD_MA)

        CD_VEL = CD_CL*CL_VEL + CD_RE*RE_VEL + CD_MA*MA_VEL
        CD_OMG = CD_CL*CL_OMG + CD_RE*RE_OMG + CD_MA*MA_OMG
        CD_B   = CD_CL*CL_B   + CD_RE*RE_B   + CD_MA*MA_B
        CD_C   = CD_CL*CL_C   + CD_RE*RE_C   + CD_MA*MA_C
        CD_U0A = CD_CL*CL_U0A + CD_RE*RE_U0A + CD_MA*MA_U0A
        CD_U0T = CD_CL*CL_U0T + CD_RE*RE_U0T + CD_MA*MA_U0T

        if(STALL[I]):
        #------- additional CD with normal-force stall model
            DCDFUN(BTOT,WA,WT, CLCD0[I],CL0[I],DCLDA[I],DCD,DCD_B,DCD_WA,DCD_WT)

            CD[I]  = CD[I]  + DCD
            CD_VEL = CD_VEL + DCD_WA*WA_VEL + DCD_WT*WT_VEL
            CD_OMG = CD_OMG + DCD_WA*WA_OMG + DCD_WT*WT_OMG
            CD_B   = CD_B   + DCD_WA*WA_B   + DCD_WT*WT_B   + DCD_B
            CD_C   = CD_C   + DCD_WA*WA_C   + DCD_WT*WT_C
            CD_U0A = CD_U0A + DCD_WA*WA_U0A + DCD_WT*WT_U0A
            CD_U0T = CD_U0T + DCD_WA*WA_U0T + DCD_WT*WT_U0T

        #------ axial and tangential load/span
        HRC = 0.5*RHO*C[I]

        FA     = HRC*W    *(  CL[I] *WT     - CD[I] *WA    )

        FA_VEL = HRC*W_VEL*(  CL[I] *WT     - CD[I] *WA    )
                 + HRC*W    *(  CL_VEL*WT     - CD_VEL*WA
                              + CL[I] *WT_VEL - CD[I] *WA_VEL)

        FA_OMG = HRC*W_OMG*(  CL[I] *WT     - CD[I] *WA    )
                 + HRC*W    *(  CL_OMG*WT     - CD_OMG*WA
                              + CL[I] *WT_OMG - CD[I] *WA_OMG)

        FA_B   = HRC*W_B  *(  CL[I] *WT     - CD[I] *WA    )
                 + HRC*W    *(  CL_B  *WT     - CD_B  *WA
                              + CL[I] *WT_B   - CD[I] *WA_B  )

        FA_C   = HRC*W_C  *(  CL[I] *WT     - CD[I] *WA    )
                 + HRC*W    *(  CL_C  *WT     - CD_C  *WA
                              + CL[I] *WT_C   - CD[I] *WA_C  )
             + 0.5*RHO*W    *(  CL[I] *WT     - CD[I] *WA    )

        FT     = HRC*W    *(  CL[I] *WA     + CD[I] *WT    )

        FT_VEL = HRC*W_VEL*(  CL[I] *WA     + CD[I] *WT    )
                 + HRC*W    *(  CL_VEL*WA     + CD_VEL*WT
                              + CL[I] *WA_VEL + CD[I] *WT_VEL)

        FT_OMG = HRC*W_OMG*(  CL[I] *WA     + CD[I] *WT    )
                 + HRC*W    *(  CL_OMG*WA     + CD_OMG*WT
                              + CL[I] *WA_OMG + CD[I] *WT_OMG)

        FT_B   = HRC*W_B  *(  CL[I] *WA     + CD[I] *WT    )
                 + HRC*W    *(  CL_B  *WA     + CD_B  *WT
                              + CL[I] *WA_B   + CD[I] *WT_B  )

        FT_C   = HRC*W_C  *(  CL[I] *WA     + CD[I] *WT    )
                 + HRC*W    *(  CL_C  *WA     + CD_C  *WT
                              + CL[I] *WA_C   + CD[I] *WT_C  )
             + 0.5*RHO*W    *(  CL[I] *WA     + CD[I] *WT    )

        #------ sum thrust and torque
        TP     = TP     + BLDS*FA    *DR[I]
        TP_VEL = TP_VEL + BLDS*FA_VEL*DR[I]
        TP_OMG = TP_OMG + BLDS*FA_OMG*DR[I]
        TP_DBE = TP_DBE + BLDS*FA_B  *DR[I]
        TP_B[I] =         BLDS*FA_B  *DR[I]
        TP_C[I] =         BLDS*FA_C  *DR[I]

        QP     = QP     + BLDS*FT    *DR[I]*R[I]
        QP_VEL = QP_VEL + BLDS*FT_VEL*DR[I]*R[I]
        QP_OMG = QP_OMG + BLDS*FT_OMG*DR[I]*R[I]
        QP_DBE = QP_DBE + BLDS*FT_B  *DR[I]*R[I]
        QP_B[I] =         BLDS*FT_B  *DR[I]*R[I]
        QP_C[I] =         BLDS*FT_C  *DR[I]*R[I]


if __name__ == '__main__':
    TQCALC(10)
