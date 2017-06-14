#***********************************************************************
#    Module:  tqcalc.f
#
#    Copyright (C) 2003 Mark Drela
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#***********************************************************************

def TQCALC (N,C,B,R,DR,VA,VT,CL,CD,STALL,BLDS,RAD,VEL,OMG,DBE,RHO,RMU,VSO,CL0,DCLDA,CLMIN,CLMAX,MCRIT,CD0,CD2U,CD2L,CLCD0,REREF,REEXP,TP, TP_VEL, TP_OMG, TP_DBE, TP_C, TP_B,QP, QP_VEL, QP_OMG, QP_DBE, QP_C, QP_B ):
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	IMPLICIT REAL(A-H,M,O-Z)
	#, B, R, DR = [0.0]*N ,[0.0]*N ,[0.0]*N ,[0.0]*N
	CL0, DCLDA, CLMIN, CLMAX, MCRIT = [0.0]*N ,[0.0]*N ,[0.0]*N ,[0.0]*N ,[0.0]*N
	CD0, CD2U, CD2L = [0.0]*N ,[0.0]*N ,[0.0]*N
	CLCD0 = [0.0]*N
	REREF, REEXP = [0.0]*N ,[0.0]*N
	#
	VA, VT, CL, CD = [0.0]*N ,[0.0]*N ,[0.0]*N ,[0.0]*N
	TP_C, TP_B,QP_C, QP_B = [0.0]*N ,[0.0]*N ,[0.0]*N ,[0.0]*N
	# part of multi line command.
	STALL = [False]*N
	#
	#-----------------------------------------------------
	#     Computes propeller thrust and torque
	#     by integrating forces along radius.
	#
	#
	#  Input   N      number of radial stations  i = 1..N
	#          C[i - 1]   chords
	#          B[i - 1]   angles (radians)
	#          R[i - 1]   radii
	#          DR[i - 1]  radius interval for i station
	#
	#          BLDS    number of blades
	#          RAD     tip radius  (for Prandtl's factor)
	#          VEL     forward flight speed
	#          OMG     rotational speed  (radians/time)
	#          DBE     delta(B) to be added to all B[i - 1] angles
	#
	#          RHO     fluid density
	#          RMU     fluid viscosity
	#
	#          CL0[i - 1]     constants for CL[alpha,M - 1] function
	#          DCLDA[i - 1]
	#          CLMIN[i - 1]
	#          CLMAX[i - 1]
	#          MCRIT[i - 1]
	#
	#          CD0[i - 1]     constant  coefficient for CD[CL - 1] function
	#          CD2(i)     quadratic coefficient for CD[CL - 1] function
	#          CLCD0[i - 1]   CL at minimum drag point (where CD=CD0)
	#          REREF[i - 1]   reference Reynolds number where CD[CL - 1] applies
	#          REEXP[i - 1]   Re-scaling exponent:   CD ~ (Re/REREF)^REEXP
	#
	#
	#  Output  VA[i - 1]    axial induced velocity
	#          VT[i - 1]    tangential induced velocity
	#          CL[i - 1]    section lift coefficient
	#          CD[i - 1]    section drag coefficient
	#          STALL[i - 1] T if alpha is outside stall limits
	#
	#          TP       prop thrust
	#          QP       prop torque
	#          ()_VEL   derivatives  d()/dVEL
	#          ()_OMG   derivatives  d()/dOMG
	#          ()_DBE   derivatives  d()/dDBE
	#          ()_C(i)  derivatives  d()/dC(i)
	#          ()_B(i)  derivatives  d()/dB(i)
	#
	#-----------------------------------------------------
	LCONV = False
	#
	MSQMAX =  0.9    
	#
	#---- clear for accumulation
	TP     = 0.
	TP_VEL = 0.
	TP_OMG = 0.
	TP_DBE = 0.
	#
	QP     = 0.
	QP_VEL = 0.
	QP_OMG = 0.
	QP_DBE = 0.
	#
	#---- go over radial stations
	for I in fortranRangeTwoParam( 1, N ):
		BTOT = B[I - 1] + DBE
		GVCALC (C(I),BTOT,R(I),BLDS,RAD,VEL,OMG,VSO,CL0(I),DCLDA(I),CLMIN(I),CLMAX(I),MCRIT(I),GAM  ,GAM_VEL,GAM_OMG,GAM_C,GAM_B,VA(I), VA_VEL, VA_OMG, VA_C, VA_B,VT(I), VT_VEL, VT_OMG, VT_C, VT_B,CL(I), CL_VEL, CL_OMG, CL_C, CL_B, STALL[I - 1], LCONV)
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		#
		#------ perturbation imposed axial and tangential velocities
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
		#
		#------ total imposed axial and tangential velocities
		UA     = VEL   + U0A
		UA_VEL = 1.0
		UA_U0A =         1.0
		#
		UT     = OMG*R(I) - U0T
		UT_OMG =     R[I - 1]
		UT_U0T =          - 1.0
		#
		#------ geometric velocity
		WZ = SQRT(UA**2 + UT**2)
		WZ_VEL = (UA/WZ)*UA_VEL
		WZ_OMG = (UT/WZ)*UT_OMG
		#
		#------ total axial and tangential velocities
		WA     = UA     + VA[I - 1]
		WA_VEL = UA_VEL + VA_VEL
		WA_OMG =          VA_OMG
		WA_B   =          VA_B
		WA_C   =          VA_C
		WA_U0A = UA_U0A + VA_U0A
		WA_U0T =          VA_U0T
		#
		WT     = UT     - VT[I - 1]
		WT_VEL =        - VT_VEL
		WT_OMG = UT_OMG - VT_OMG
		WT_B   =        - VT_B
		WT_C   =        - VT_C
		WT_U0A =        - VT_U0A
		WT_U0T = UT_U0T - VT_U0T
		#
		#------ total velocity^2
		WSQ = WA**2 + WT**2
		WSQ_VEL = 2.0*WA*WA_VEL + 2.0*WT*WT_VEL
		WSQ_OMG = 2.0*WA*WA_OMG + 2.0*WT*WT_OMG
		WSQ_B   = 2.0*WA*WA_B   + 2.0*WT*WT_B
		WSQ_C   = 2.0*WA*WA_C   + 2.0*WT*WT_C
		WSQ_U0A = 2.0*WA*WA_U0A + 2.0*WT*WT_U0A
		WSQ_U0T = 2.0*WA*WA_U0T + 2.0*WT*WT_U0T
		#
		#------ total velocity
		W = SQRT(WSQ)
		W_VEL = 0.5*WSQ_VEL/W
		W_OMG = 0.5*WSQ_OMG/W
		W_B   = 0.5*WSQ_B  /W
		W_C   = 0.5*WSQ_C  /W
		W_U0A = 0.5*WSQ_U0A/W
		W_U0T = 0.5*WSQ_U0T/W
		#
		#------ chord Reynolds number
		RE     = RHO*C(I)*W    /RMU
		RE_VEL = RHO*C(I)*W_VEL/RMU
		RE_OMG = RHO*C(I)*W_OMG/RMU
		RE_B   = RHO*C(I)*W_B  /RMU
		RE_C   = RHO*C(I)*W_C  /RMU+ RHO     *W    /RMU
		# part of multi line command.
		RE_U0A = RHO*C(I)*W_U0A/RMU
		RE_U0T = RHO*C(I)*W_U0T/RMU
		#
		#------ local Mach and PG factor
		MSQ     = WSQ     / VSO**2
		MSQ_VEL = WSQ_VEL / VSO**2
		MSQ_OMG = WSQ_OMG / VSO**2
		MSQ_B   = WSQ_B   / VSO**2
		MSQ_C   = WSQ_C   / VSO**2
		MSQ_U0A = WSQ_U0A / VSO**2
		MSQ_U0T = WSQ_U0T / VSO**2
		if (MSQ > MSQMAX):
			MSQ = MSQMAX
			MSQ_VEL = 0.
			MSQ_OMG = 0.
			MSQ_B   = 0.
			MSQ_C   = 0.
			MSQ_U0A = 0.
			MSQ_U0T = 0.
		#ENDIF
		#
		PG = 1.0 / SQRT(1.0 - MSQ)
		PG_MSQ = 0.5*PG / (1.0 - MSQ)
		#
		PG_VEL = PG_MSQ*MSQ_VEL
		PG_OMG = PG_MSQ*MSQ_OMG
		PG_B   = PG_MSQ*MSQ_B
		PG_C   = PG_MSQ*MSQ_C
		PG_U0A = PG_MSQ*MSQ_U0A
		PG_U0T = PG_MSQ*MSQ_U0T
		#
		MA = SQRT(MSQ)
		MA = MAX( MA , 1.0E-8 )
		MA_VEL = (0.5/MA)*MSQ_VEL
		MA_OMG = (0.5/MA)*MSQ_OMG
		MA_B   = (0.5/MA)*MSQ_B
		MA_C   = (0.5/MA)*MSQ_C
		MA_U0A = (0.5/MA)*MSQ_U0A
		MA_U0T = (0.5/MA)*MSQ_U0T
		#
		#------ set unstalled CD
		if (CL(I)>CLCD0(I)):
			CDFUN (CL(I),RE,MA,CLCD0(I),CD0(I),CD2U(I),REREF,REEXP(I),MCRIT,CD(I),CD_CL,CD_RE,CD_MA)
			# part of multi line command.
			# part of multi line command.
		else:
			CDFUN (CL(I),RE,MA,CLCD0(I),CD0(I),CD2L(I),REREF,REEXP(I),MCRIT,CD(I),CD_CL,CD_RE,CD_MA)
			# part of multi line command.
			# part of multi line command.
		#ENDIF
		CD_VEL = CD_CL*CL_VEL + CD_RE*RE_VEL + CD_MA*MA_VEL
		CD_OMG = CD_CL*CL_OMG + CD_RE*RE_OMG + CD_MA*MA_OMG
		CD_B   = CD_CL*CL_B   + CD_RE*RE_B   + CD_MA*MA_B
		CD_C   = CD_CL*CL_C   + CD_RE*RE_C   + CD_MA*MA_C
		CD_U0A = CD_CL*CL_U0A + CD_RE*RE_U0A + CD_MA*MA_U0A
		CD_U0T = CD_CL*CL_U0T + CD_RE*RE_U0T + CD_MA*MA_U0T
		#
		if (STALL(I)):
			#------- additional CD with normal-force stall model
			DCDFUN (BTOT,WA,WT, CLCD0[I - 1],CL0(I),DCLDA(I),DCD,DCD_B,DCD_WA,DCD_WT)
			# part of multi line command.
			#
			CD(I)  = CD[I - 1]  + DCD
			CD_VEL = CD_VEL + DCD_WA*WA_VEL + DCD_WT*WT_VEL
			CD_OMG = CD_OMG + DCD_WA*WA_OMG + DCD_WT*WT_OMG
			CD_B   = CD_B   + DCD_WA*WA_B   + DCD_WT*WT_B   + DCD_B
			CD_C   = CD_C   + DCD_WA*WA_C   + DCD_WT*WT_C
			CD_U0A = CD_U0A + DCD_WA*WA_U0A + DCD_WT*WT_U0A
			CD_U0T = CD_U0T + DCD_WA*WA_U0T + DCD_WT*WT_U0T
			#
		#ENDIF
		#
		#------ axial and tangential load/span
		HRC = 0.5*RHO*C(I)
		FA     = HRC*W    *(  CL[I - 1] *WT     - CD[I - 1] *WA    )
		FA_VEL = HRC*W_VEL*(  CL[I - 1] *WT     - CD[I - 1] *WA    )+ HRC*W    *(  CL_VEL*WT     - CD_VEL*WA+ CL[I - 1] *WT_VEL - CD[I - 1] *WA_VEL)
		# part of multi line command.
		# part of multi line command.
		FA_OMG = HRC*W_OMG*(  CL[I - 1] *WT     - CD[I - 1] *WA    )+ HRC*W    *(  CL_OMG*WT     - CD_OMG*WA+ CL[I - 1] *WT_OMG - CD[I - 1] *WA_OMG)
		# part of multi line command.
		# part of multi line command.
		FA_B   = HRC*W_B  *(  CL[I - 1] *WT     - CD[I - 1] *WA    )+ HRC*W    *(  CL_B  *WT     - CD_B  *WA+ CL[I - 1] *WT_B   - CD[I - 1] *WA_B  )
		# part of multi line command.
		# part of multi line command.
		FA_C   = HRC*W_C  *(  CL[I - 1] *WT     - CD[I - 1] *WA    )+ HRC*W    *(  CL_C  *WT     - CD_C  *WA+ CL[I - 1] *WT_C   - CD[I - 1] *WA_C  )+ 0.5*RHO*W    *(  CL[I - 1] *WT     - CD[I - 1] *WA    )
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		#
		FT     = HRC*W    *(  CL[I - 1] *WA     + CD[I - 1] *WT    )
		FT_VEL = HRC*W_VEL*(  CL[I - 1] *WA     + CD[I - 1] *WT    )+ HRC*W    *(  CL_VEL*WA     + CD_VEL*WT+ CL[I - 1] *WA_VEL + CD[I - 1] *WT_VEL)
		# part of multi line command.
		# part of multi line command.
		FT_OMG = HRC*W_OMG*(  CL[I - 1] *WA     + CD[I - 1] *WT    )+ HRC*W    *(  CL_OMG*WA     + CD_OMG*WT+ CL[I - 1] *WA_OMG + CD[I - 1] *WT_OMG)
		# part of multi line command.
		# part of multi line command.
		FT_B   = HRC*W_B  *(  CL[I - 1] *WA     + CD[I - 1] *WT    )+ HRC*W    *(  CL_B  *WA     + CD_B  *WT+ CL[I - 1] *WA_B   + CD[I - 1] *WT_B  )
		# part of multi line command.
		# part of multi line command.
		FT_C   = HRC*W_C  *(  CL[I - 1] *WA     + CD[I - 1] *WT    )+ HRC*W    *(  CL_C  *WA     + CD_C  *WT+ CL[I - 1] *WA_C   + CD[I - 1] *WT_C  )+ 0.5*RHO*W    *(  CL[I - 1] *WA     + CD[I - 1] *WT    )
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		#
		#------ sum thrust and torque
		TP     = TP     + BLDS*FA    *DR(I)
		TP_VEL = TP_VEL + BLDS*FA_VEL*DR(I)
		TP_OMG = TP_OMG + BLDS*FA_OMG*DR(I)
		TP_DBE = TP_DBE + BLDS*FA_B  *DR(I)
		TP_B(I) =         BLDS*FA_B  *DR(I)
		TP_C(I) =         BLDS*FA_C  *DR(I)
		#
		QP     = QP     + BLDS*FT    *DR(I)*R(I)
		QP_VEL = QP_VEL + BLDS*FT_VEL*DR(I)*R(I)
		QP_OMG = QP_OMG + BLDS*FT_OMG*DR(I)*R(I)
		QP_DBE = QP_DBE + BLDS*FT_B  *DR(I)*R(I)
		QP_B(I) =         BLDS*FT_B  *DR(I)*R(I)
		QP_C[I - 1] =         BLDS*FT_C  *DR(I)*R(I)
		
		#       write(*,*) i
		#       write(*,*) tp, qp
		#       write(*,*) tp_vel, qp_vel
		#       write(*,*) tp_omg, qp_omg
		#       write(*,*) tp_b(i),qp_b(i)
		#       write(*,*) tp_c(i),qp_c(i)
		#       pause
		#
		#
	#ENDDO
	#
	return
#END
