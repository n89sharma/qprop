#***********************************************************************
#    Module:  qprop.f
#
#    Copyright (C) 2005 Mark Drela
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

def QPROP ():
	#--------------------------------------------------------
	#     Propeller/motor performance program
	#     Version 1.20   12 Mar 06
	#
	#     Usage:
	#
	# % qprop propfile motorfile Vel Rpm Volt dBeta    (single-point)
	#
	# % qprop propfile motorfile Vel1,Vel2,dVel Rpm Volt dBeta  (1-param multi-point)
	#
	# % qprop propfile motorfile Vel1,Vel2/NVel Rpm Volt dBeta  (1-param multi-point)
	#
	# % qprop propfile motorfile Vel1,Vel2/NVel Rpm Volt1,Volt2,dVolt dBeta
	#                                                           (2-param multi-point)
	#
	# % qprop propfile motorfile runfile               (multi-point)
	#
	#
	#--------------------------------------------------------
	IMPLICIT REAL (A-H,M,O-Z)
	#
	#---- input radial quantities (from propfile)
	IRDIM = 81
	WORK = [0.0]*IRDIM
	RB, CB, BB = [0.0]*IRDIM ,[0.0]*IRDIM ,[0.0]*IRDIM
	CL0B, DCLDAB, CLMINB, CLMAXB = [0.0]*IRDIM ,[0.0]*IRDIM ,[0.0]*IRDIM ,[0.0]*IRDIM
	CD0B, CD2UB, CD2LB, CLCD0B = [0.0]*IRDIM ,[0.0]*IRDIM ,[0.0]*IRDIM ,[0.0]*IRDIM
	REREFB, REEXPB, MCRITB = [0.0]*IRDIM ,[0.0]*IRDIM ,[0.0]*IRDIM
	#
	#---- radial quantities interpolated to computational stations
	IDIM = 25
	R, C, B, DR = [0.0]*IDIM ,[0.0]*IDIM ,[0.0]*IDIM ,[0.0]*IDIM
	CL0, DCLDA, CLMIN, CLMAX = [0.0]*IDIM ,[0.0]*IDIM ,[0.0]*IDIM ,[0.0]*IDIM
	CD0, CD2U, CD2L, CLCD0 = [0.0]*IDIM ,[0.0]*IDIM ,[0.0]*IDIM ,[0.0]*IDIM
	REREF, REEXP, MCRIT = [0.0]*IDIM ,[0.0]*IDIM ,[0.0]*IDIM
	VA, VT, CL, CD = [0.0]*IDIM ,[0.0]*IDIM ,[0.0]*IDIM ,[0.0]*IDIM
	STALL = [False]*IDIM
	#
	TP_C, TP_B,QP_C, QP_B = [0.0]*IDIM ,[0.0]*IDIM ,[0.0]*IDIM ,[0.0]*IDIM
	# part of multi line command.
	#
	#---- motor parameters
	NMPDIM = 10
	PARMOT = [0.0]*NMPDIM
	PMLAB = ['']*NMPDIM
	#
	#---- various character variables
	CHARF, ANS = '',''
	PNAME, MNAME = '',''
	ARGP1, ARGP2, ARGP3, ARGP4, ARGP5,ARGP6, ARGP7, ARGP8, ARGP9, ARGP10 = '','','','','','','','','',''
	# part of multi line command.
	FILNAM = ''
	LINE = ''
	#
	LRDUMP = False
	LRPMSET,LVOLTSET,LTHRUSET,LTORQSET,LAMPSSET,LPELESET = False,False,False,False,False,False
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	ERROR = False
	#
	INCLUDE 'QDEF.INC'
	#
	#---- input receiving arrays
	RVAL = [0.0]*15
	IVAL = [0]*15
	#
	PI =  3.14159265 
	#      DATA EPS / 1.0E-6 /
	EPS =  1.0E-8 
	#
	VERSION =  1.22 
	
	#---- default Mcrit
	MCRIT0 = 0.70
	#
	#---- get Unix command-line arguments, if any
	GETARG0 (1,ARGP1)
	GETARG0 (2,ARGP2)
	GETARG0 (3,ARGP3)
	GETARG0 (4,ARGP4)
	GETARG0 (5,ARGP5)
	GETARG0 (6,ARGP6)
	GETARG0 (7,ARGP7)
	GETARG0 (8,ARGP8)
	GETARG0 (9,ARGP9)
	GETARG0 (10,ARGP10)
	#
	if ( ARGP1 == ' '): 
		print '\nQPROPusage:\n\n%qproppropfilemotorfileVelRpm[VoltdBetaThrustTorque{}mpsPele](single-point)\n\n%qproppropfilemotorfileVel1Vel2dVelRpm["](multi-point1-parametersweepoverVelRpmset)\n\n%qproppropfilemotorfileVel1Vel2dVel0Volt["](multi-point1-parametersweepoverVelVoltset)\n\n%qproppropfilemotorfileVel1Vel2dVelRpm1Rpm2dRpm["](multi-point2-parametersweepoverVelandRpm)\n\n%qproppropfilemotorfilerunfile(multi-pointviafilespecification)'.format()
		# 1005  FORMAT(/' QPROP usage:'//' % qprop propfile motorfile Vel Rpm ','[ Volt dBeta Thrust Torque Amps Pele ]','   (single-point)'//' % qprop propfile motorfile Vel1,Vel2,dVel Rpm ["]           ','   (multi-point 1-parameter sweep over Vel, Rpm set)'//' % qprop propfile motorfile Vel1,Vel2,dVel 0 Volt ["]        ','   (multi-point 1-parameter sweep over Vel, Volt set)'//' % qprop propfile motorfile Vel1,Vel2,dVel Rpm1,Rpm2,dRpm ["]','   (multi-point 2-parameter sweep over Vel and Rpm)'//' % qprop propfile motorfile runfile                          ','   (multi-point, via file specification)')
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		print 
		print  'Run with default inputs?  Y'
		READ(*,1000) ANS
		if (INDEX('Nn',ANS) != 0): raise SystemExit
		print 
	#ENDIF
	#
	#---- default fluid properties from QDEF.INC
	RHO = RHO1   # density
	RMU = RMU1   # viscosity
	VSO = VSO1   # speed of sound
	#
	QCGET (RHO,RMU,VSO)
	#
	#==========================================================
	#---- set default prop
	PNAME = 'Graupner CAM 6x3 folder'
	BLDS = 2.0
	#
	#---- number of radial stations
	NR = 7
	#
	#---- linear CL[alpha - 1] function
	#     CL  =  CL0 + DCLCD*alpha  ,  clipped if outside range  CLMIN..CLMAX
	for IR in fortranRangeTwoParam( 1, NR ):
		CL0B[IR - 1] = 0.5
		DCLDAB[IR - 1] = 5.8
		CLMINB[IR - 1] = -0.4
		CLMAXB[IR - 1] = 1.2
	#ENDDO
	#
	#---- quadratic CD[CL,Re - 1] function
	#     CD  =  [ CD0 + CD2*(CL-CLCD0)**2 ] * [Re/REREF]^REEXP
	for IR in fortranRangeTwoParam( 1, NR ):
		CD0B[IR - 1] = 0.028
		CD2UB[IR - 1] = 0.050
		CD2LB[IR - 1] = 0.050
		CLCD0B[IR - 1] = 0.5
		REREFB[IR - 1] = 70000.0
		REEXPB[IR - 1] = -0.7
		MCRITB[IR - 1] = MCRIT0
	#ENDDO
	#
	#---- radii
	RFAC = 0.0254
	RADD = 0.
	RB[1 - 1] = 0.75
	RB[2 - 1] = 1.00
	RB[3 - 1] = 1.50
	RB[4 - 1] = 2.00
	RB[5 - 1] = 2.50
	RB[6 - 1] = 2.875
	RB[7 - 1] = 3.00
	#
	#---- chords
	CFAC = 0.0254
	CADD = 0.
	CB[1 - 1] = 0.66
	CB[2 - 1] = 0.69
	CB[3 - 1] = 0.63
	CB[4 - 1] = 0.55
	CB[5 - 1] = 0.44
	CB[6 - 1] = 0.30
	CB[7 - 1] = 0.19
	#
	#---- blade angles
	BFAC = 1.0
	BADD = 0.
	BB[1 - 1] = 27.5
	BB[2 - 1] = 22.0
	BB[3 - 1] = 15.2
	BB[4 - 1] = 10.2
	BB[5 - 1] =  6.5
	BB[6 - 1] =  4.6
	BB[7 - 1] =  4.2
	#
	RAD = RB[NR - 1]
	#
	#----------------------------------------------------
	#---- default motor/gear combo
	MNAME = "Speed-400 3321 (6V) direct drive"
	IMOTYPE = 1
	PARMOT[1 - 1] = 0.31    # Rmotor  (Ohms)
	PARMOT[2 - 1] = 0.77    # Io      (Amps)
	PARMOT[3 - 1] = 2760.0  # Kv      (rpm/Volt)
	PMLAB[1 - 1] = 'R  (Ohm)'
	PMLAB[2 - 1] = 'Io (Amp)'
	PMLAB[3 - 1] = 'Kv (rpm/Volt)'
	NMPAR = 3
	#
	#----------------------------------------------------
	#---- default parameter sweeps
	VEL1 =  0.0
	VEL2 = 10.0
	NVEL = 6
	#
	RPM1 = 10000.0
	RPM2 = 16000.0
	NRPM = 7
	#
	VOLT1 = 6.0
	VOLT2 = 9.0
	NVOLT = 4
	#
	DBET1 = -2.0
	DBET2 =  2.0
	NDBET = 5
	#
	THRU1 = 0.
	THRU2 = 0.
	NTHRU = 1
	#
	AMPS1 = 0.
	AMPS2 = 0.
	NAMPS = 1
	#
	PELE1 = 0.
	PELE2 = 0.
	NPELE = 1
	#
	#---- do not dump radial distributions
	LRDUMP = False
	#
	#==========================================================
	# 1000 FORMAT(A)
	#
	#----------------------------------------------------
	#---- read prop data file
	FILNAM = ARGP1
	IF( FILNAM == ' ') GO TO 18 
	#
	LU = 1
	OPEN(LU,FILE=FILNAM,STATUS='OLD',ERR=18)
	#
	ILINE = 0
	#
	#---- prop name
	FREAD (LU,LINE,ILINE,IERR,PNAME)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	#
	#---- extract parameters on data lines
	NVAL = 2
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	IF(NVAL< 1) GO TO 980
	BLDS = RVAL[1 - 1]
	if (NVAL>=2):
		RAD = RVAL[2 - 1]
	else:
		RAD = 0.
	#ENDIF
	#
	NVAL = 2
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	IF(NVAL< 2) GO TO 980
	for IR in fortranRangeTwoParam( 1, IRDIM ):
		CL0B[IR - 1]   = RVAL[1 - 1]
		DCLDAB[IR - 1] = RVAL[2 - 1]
	#ENDDO
	#
	NVAL = 2
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	IF(NVAL< 2) GO TO 980
	for IR in fortranRangeTwoParam( 1, IRDIM ):
		CLMINB[IR - 1] = RVAL[1 - 1]
		CLMAXB[IR - 1] = RVAL[2 - 1]
	#ENDDO
	#
	NVAL = 4
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	IF(NVAL< 3) GO TO 980
	for IR in fortranRangeTwoParam( 1, IRDIM ):
		CD0B[IR - 1]   = RVAL[1 - 1]
		CD2UB[IR - 1]  = RVAL[2 - 1]
		CD2LB[IR - 1]  = RVAL[3 - 1]
		CLCD0B[IR - 1] = RVAL[4 - 1]
	#ENDDO
	#
	NVAL = 2
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	IF(NVAL< 2) GO TO 980
	for IR in fortranRangeTwoParam( 1, IRDIM ):
		REREFB[IR - 1] = RVAL[1 - 1]
		REEXPB[IR - 1] = RVAL[2 - 1]
	#ENDDO
	#
	#
	NVAL = 3
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	IF(NVAL< 3) GO TO 980
	RFAC = RVAL[1 - 1]
	CFAC = RVAL[2 - 1]
	BFAC = RVAL[3 - 1]
	#
	NVAL = 3
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	IF(NVAL< 3) GO TO 980
	RADD = RVAL[1 - 1]
	CADD = RVAL[2 - 1]
	BADD = RVAL[3 - 1]
	#
	KR = 0
	#
	#14 CONTINUE
	#
	NVAL = 13
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 16
	IF(NVAL< 3) GO TO 980
	#
	KR = KR + 1
	IR = MIN( KR , IRDIM )
	RB[IR - 1] = RVAL[1 - 1]
	CB[IR - 1] = RVAL[2 - 1]
	BB[IR - 1] = RVAL[3 - 1]
	#
	MCRITB[IR - 1] = MCRIT0
	#
	if (NVAL>= 4): CL0B[IR - 1]   = RVAL[ 4 - 1]
	if (NVAL>= 5): DCLDAB[IR - 1] = RVAL[ 5 - 1]
	if (NVAL>= 6): CLMINB[IR - 1] = RVAL[ 6 - 1]
	if (NVAL>= 7): CLMAXB[IR - 1] = RVAL[ 7 - 1]
	if (NVAL>= 8): CD0B[IR - 1]   = RVAL[ 8 - 1]
	if (NVAL>= 9): CD2UB[IR - 1]  = RVAL[ 9 - 1]
	if (NVAL>=10): CD2LB[IR - 1]  = RVAL[10 - 1]
	if (NVAL>=11): CLCD0B[IR - 1] = RVAL[11 - 1]
	if (NVAL>=12): REREFB[IR - 1] = RVAL[12 - 1]
	if (NVAL>=13): REEXPB[IR - 1] = RVAL[13 - 1]
	#
	GO TO 14
	#
	#16 CONTINUE
	CLOSE(LU)
	#
	if (KR>0):
		NR = IR
		if (KR>NR):
			print  'Array overflow.  Increase IRDIM to', KR
			raise SystemExit
		#ENDIF
	#ENDIF
	GO TO 19
	#
	#18 CONTINUE
	print 
	print  'Prop file not found:  ', FILNAM[1:48 - 1]
	print  'Default prop used  :  ', PNAME
	#
	#
	#19 CONTINUE
	#
	if (NR<=1):
		print 
		print  '*** Must define at least two radial stations'
		raise SystemExit
	#ENDIF
	#
	#---- apply scaling factors
	for IR in fortranRangeTwoParam( 1, NR ):
		RB[IR - 1] =  RB[IR - 1]*RFAC + RADD
		CB[IR - 1] =  CB[IR - 1]*CFAC + CADD
		BB[IR - 1] = (BB[IR - 1]*BFAC + BADD)* PI / 180.0
	#ENDDO
	#
	if ( isClose(RAD, 0.0) ):
		RAD = RB[NR - 1]
	#ENDIF
	#
	for IR in fortranRangeTwoParam( 1, NR ):
		if (CB[IR - 1] <= 0.0): raise SystemExit('Chords must be positive')
		# part of multi line command.
		if (RB[IR - 1] < 0.0): raise SystemExit('Radii must be nonnegative')
		# part of multi line command.
		if (RB[IR - 1] >= RB[IR+1 - 1]): raise SystemExit('Radii must increase monotonically')
		# part of multi line command.
	#ENDDO
	#
	if (RAD < RB[NR - 1]):
		print '\nGivenonline2:R={:12.4f}\nLastrstation:r={:12.4f}\n\nMusthaveR>r\n'.format( RAD, RB[NR - 1])
		# 1050  FORMAT(/' Given on line 2:  R =', G12.4,/' Last r station :  r =', G12.4,//' Must have  R > r' / )
		# part of multi line command.
		# part of multi line command.
		raise SystemExit
	#ENDIF
	#
	#==========================================================
	#---- read motor data file
	FILNAM = ARGP2
	IF( FILNAM == ' ') GO TO 28 
	#
	LU = 2
	OPEN(LU,FILE=FILNAM,STATUS='OLD',ERR=28)
	#
	#---- clear motor data in case it's not all in the file
	for IMPAR in fortranRangeTwoParam( 1, NMPDIM ):
		PARMOT[IMPAR - 1] = 0.0
		PMLAB[IMPAR - 1] = ' '
	#ENDDO
	#
	ILINE = 0
	#
	#---- motor name
	FREAD (LU,LINE,ILINE,IERR,MNAME)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	#
	#---- motor model index
	NVAL = 1
	IREAD (LU,LINE,ILINE,IERR,NVAL,IVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	IF(NVAL< 1) GO TO 980
	IMOTYPE = IVAL[1 - 1]
	#
	#---- extract parameters on data lines
	for IMPAR in fortranRangeTwoParam( 1, NMPDIM ):
		NVAL = 1
		RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
		IF( IERR == +1 ) GO TO 900
		IF( IERR == -1 ) GO TO 25
		IF(NVAL< 1) GO TO 980
		if ( IMPAR == NMPDIM+1): 
			print  '* Motor parameter array overflow. Increase NMPDIM'
			raise SystemExit
		#ENDIF
		PARMOT[IMPAR - 1] = RVAL[1 - 1]
		KEX = INDEX(LINE,'#')
		if (KEX>=1):
			PMLAB[IMPAR - 1] = LINE[KEX+1:80 - 1]
		else:
			PMLAB[IMPAR - 1] = ' '
		#ENDIF
	#ENDDO
	#
	#25 CONTINUE
	NMPAR = IMPAR-1
	#
	CLOSE(LU)
	GO TO 29
	#
	#28 CONTINUE
	print 
	print  'Motor file not found:  ', FILNAM[1:48 - 1]
	print  'Default motor used  :  ', MNAME
	#
	#29 CONTINUE
	#
	#==========================================================
	#---- operating parameter data file, or single-point parameters
	FILNAM = ARGP3
	IF( FILNAM == ' ') GO TO 80 
	#
	#---- first assume that 3rd Unix argument is parameter data filename
	LU = 4
	OPEN(LU,FILE=FILNAM,STATUS='OLD',ERR=31)
	#
	#---- file open successful... read parameter data
	ILINE = 0
	#
	#---- extract parameters on data lines
	NVAL = 3
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	IF(NVAL< 3) GO TO 980
	VEL1 = RVAL[1 - 1]
	VEL2 = RVAL[2 - 1]
	NVEL = INT( RVAL[3 - 1] + 0.01 )
	#
	#---- extract parameters on data lines
	NVAL = 3
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	IF(NVAL< 3) GO TO 980
	RPM1 = RVAL[1 - 1]
	RPM2 = RVAL[2 - 1]
	NRPM = INT( RVAL[3 - 1] + 0.01 )
	#
	#---- extract parameters on data lines
	NVAL = 3
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	IF( IERR == -1 ) GO TO 950
	IF(NVAL< 3) GO TO 980
	VOLT1 = RVAL[1 - 1]
	VOLT2 = RVAL[2 - 1]
	NVOLT = INT( RVAL[3 - 1] + 0.01 )
	#
	#---- extract parameters on data lines
	NVAL = 3
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	if ( IERR == -1  or  NVAL< 3):
		DBET1 = 0.0
		DBET2 = 0.0
		NDBET = 0
	else:
		DBET1 = RVAL[1 - 1]
		DBET2 = RVAL[2 - 1]
		NDBET = INT( RVAL[3 - 1] + 0.01 )
	#ENDIF
	NDBET = MAX( 1 , NDBET )
	#
	NVAL = 3
	RREAD (LU,LINE,ILINE,IERR,NVAL,RVAL)
	IF( IERR == +1 ) GO TO 900
	if ( IERR == -1  or  NVAL< 3):
		THRU1 = 0.0
		THRU2 = 0.0
		NTHRU = 0
	else:
		THRU1 = RVAL[1 - 1]
		THRU2 = RVAL[2 - 1]
		NTHRU = INT( RVAL[3 - 1] + 0.01 )
	#ENDIF
	NTHRU = MAX( 1 , NTHRU )
	#
	CLOSE(LU)
	GO TO 82
	#
	#-------------------------------------------------------
	#---- pick up here if 3rd Unix argument is not a filename
	#31 CONTINUE
	#
	#---- try reading velocity 3rd Unix argument
	PPARSE (ARGP3,VEL1,VEL2,NVEL,IERR)
	IF( IERR == +1 ) GO TO 80
	IF( IERR == -1 ) GO TO 80
	#
	#---- set new default single-point RPM or VOLT
	RPM1 = 0.
	RPM2 = 0.
	NRPM = 0
	#
	VOLT1 = 0.
	VOLT2 = 0.
	NVOLT = 0
	#
	DBET1 = 0.
	DBET2 = 0.
	NDBET = 1
	#
	THRU1 = 0.
	THRU2 = 0.
	NTHRU = 1
	#
	TORQ1 = 0.
	TORQ2 = 0.
	NTORQ = 1
	#
	AMPS1 = 0.
	AMPS2 = 0.
	NAMPS = 1
	#
	PELE1 = 0.
	PELE2 = 0.
	NPELE = 1
	#
	#---- try reading Rpm from 4th Unix argument
	PPARSE (ARGP4,RPM1,RPM2,NRPM,IERR)
	if ( IERR == +1  or  IERR == -1 ):
		# part of multi line command.
		RPM = 0.0
		NRPM = 0
	#ENDIF
	#
	#---- try reading Voltage from 5th Unix argument
	PPARSE (ARGP5,VOLT1,VOLT2,NVOLT,IERR)
	if ( IERR == +1  or  IERR == -1 ):
		# part of multi line command.
		VOLT = 0.0
		NVOLT = 0
	#ENDIF
	#
	#---- try reading pitch change from 6th Unix argument
	PPARSE (ARGP6,DBET1,DBET2,NDBET,IERR)
	if ( IERR == +1  or  IERR == -1 ):
		# part of multi line command.
		DBET = 0.0
		NDBET = 1
	#ENDIF
	#
	#---- try reading thrust from 7th Unix argument
	PPARSE (ARGP7,THRU1,THRU2,NTHRU,IERR)
	if ( IERR == +1  or  IERR == -1 ):
		# part of multi line command.
		THRU = 0.0
		NTHRU = 1
	#ENDIF
	#
	#---- try reading torque from 8th Unix argument
	PPARSE (ARGP8,TORQ1,TORQ2,NTORQ,IERR)
	if ( IERR == +1  or  IERR == -1 ):
		# part of multi line command.
		TORQ = 0.0
		NTHRU = 1
	#ENDIF
	#
	#---- try reading current from 9th Unix argument
	PPARSE (ARGP9,AMPS1,AMPS2,NAMPS,IERR)
	if ( IERR == +1  or  IERR == -1 ):
		# part of multi line command.
		AMPS = 0.0
		NAMPS = 1
	#ENDIF
	#
	#---- try reading Pele from 10th Unix argument
	PPARSE (ARGP10,PELE1,PELE2,NPELE,IERR)
	if ( IERR == +1  or  IERR == -1 ):
		# part of multi line command.
		PELE = 0.0
		NPELE = 1
	#ENDIF
	#
	#---- if this is a single-point case... will dump radial distributions
	LRDUMP = NVEL <=1 and  NRPM <=1 and  NVOLT<=1 and  NDBET<=1 and  NTHRU<=1 and  NTORQ<=1 and  NAMPS<=1 and  NPELE<=1
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	#
	GO TO 82
	#
	#
	#80 CONTINUE
	print 
	print  'Run parameter file not found: ', FILNAM[1:48 - 1]
	print  'Default velocities, voltages, pitch used'
	#
	#82 CONTINUE
	#
	if ( NRPM == 0  and  NVOLT == 0  and  NTHRU == 0  and  NAMPS == 0  and  NPELE == 0 ):
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		print 'Must specify either Rpm or Volts or Thrust or Amps or Pele'
		# part of multi line command.
		raise SystemExit
	#ENDIF
	#
	LRPMSET  = NRPM  > 0  and  ( not isClose(RPM1, 0.0)  or  not isClose(RPM2, 0.0) )
	LVOLTSET = NVOLT > 0  and  ( not isClose(VOLT1, 0.0)  or  not isClose(VOLT2, 0.0) )
	LTHRUSET = NTHRU > 0  and  ( not isClose(THRU1, 0.0)  or  not isClose(THRU2, 0.0) )
	LTORQSET = NTORQ > 0  and  ( not isClose(TORQ1, 0.0)  or  not isClose(TORQ2, 0.0) )
	LAMPSSET = NAMPS > 0  and  ( not isClose(AMPS1, 0.0)  or  not isClose(AMPS2, 0.0) )
	LPELESET = NPELE > 0  and  ( not isClose(PELE1, 0.0)  or  not isClose(PELE2, 0.0) )
	#
	#    write(*,*)
	#   &  lrpmset, lvoltset, lthruset, ltorqset, lampsset, lpeleset
	
	#==========================================================
	#
	#---- set up finely-spaced radial arrays
	R0 = RB[1 - 1]
	R1 = RB[NR - 1]
	#
	N = IDIM
	for I in fortranRangeTwoParam( 1, N ):
		FRAC = (FLOAT(I)-0.5)/FLOAT(N)
		R[I - 1] = R0*(1.0-FRAC) + R1*FRAC
		DR[I - 1] = (R1-R0)/FLOAT(N)
	#ENDDO
	#
	SPLINE (CB,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		C[I - 1] = SEVAL(R[I - 1],CB,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (BB,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		B[I - 1] = SEVAL(R[I - 1],BB,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (CL0B,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		CL0[I - 1] = SEVAL(R[I - 1],CL0B,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (DCLDAB,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		DCLDA[I - 1] = SEVAL(R[I - 1],DCLDAB,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (CLMINB,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		CLMIN[I - 1] = SEVAL(R[I - 1],CLMINB,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (CLMAXB,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		CLMAX[I - 1] = SEVAL(R[I - 1],CLMAXB,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (CD0B,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		CD0[I - 1] = SEVAL(R[I - 1],CD0B,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (CD2UB,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		CD2U[I - 1] = SEVAL(R[I - 1],CD2UB,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (CD2LB,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		CD2L[I - 1] = SEVAL(R[I - 1],CD2LB,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (CLCD0B,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		CLCD0[I - 1] = SEVAL(R[I - 1],CLCD0B,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (REREFB,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		REREF[I - 1] = SEVAL(R[I - 1],REREFB,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (REEXPB,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		REEXP[I - 1] = SEVAL(R[I - 1],REEXPB,WORK,RB,NR)
	#ENDDO
	#
	SPLINE (MCRITB,WORK,RB,NR)
	for I in fortranRangeTwoParam( 1, N ):
		MCRIT[I - 1] = SEVAL(R[I - 1],MCRITB,WORK,RB,NR)
	#ENDDO
	#
	#---- reality checks
	ERROR = False
	for I in fortranRangeTwoParam( 1, N ):
		if (C[I - 1] <= 0.0):
			print  'Negative chord at i =', I
			ERROR = True
		#ENDIF
		if (REREF[I - 1] <= 0.0):
			print  'Negative Re_ref at i =', I
			ERROR = True
		#ENDIF
		if (MCRIT[I - 1] <= 0.0):
			print  'Negative Mcrit at i =', I
			ERROR = True
		#ENDIF
	#ENDDO
	#
	if (ERROR):
		print 
		print '#{}{}{}{}'.format(' i   radius   chord     beta    Re_ref')
		# part of multi line command.
		for I in fortranRangeTwoParam( 1, N ):
			IRE = INT( REREF[I - 1] )
			print ' {:3d}{:9.4f}{:9.4f}{:9.3f}{:9d}'.format( I, R[I - 1], C[I - 1], B[I - 1]*180.0/PI, IRE)
			# 1070     FORMAT(1X,I3, F9.4, F9.4, F9.3, I9)
		#ENDDO
		print 
		raise SystemExit
	#ENDIF
	
	#
	#----------------------------------------------------
	#---- perform calculations and dump output
	#
	LU = 6
	print 
	#
	# 1105 FORMAT('# QPROP Version', F5.2)
	# 1100 FORMAT('# ', A,A,A,A)
	# 1110 FORMAT('#  ', G12.5, 1X, A)
	# 1120 FORMAT('#   rho =', G12.5,' kg/m^3'/'#   mu  =', G12.5,' kg/m-s'/'#   a   =', G12.5,' m/s   ' )
	# part of multi line command.
	# part of multi line command.
	#
	LU.write('#QPROPVersion{:5.2f}'.format( VERSION))
	LU.write('#{}{}{}{}'.format())
	LU.write('#{}{}{}{}'.format( PNAME))
	LU.write('#{}{}{}{}'.format())
	LU.write('#{}{}{}{}'.format( MNAME))
	for IMPAR in fortranRangeTwoParam( 1, NMPAR ):
		LU.write('#{:12.5f} {}'.format( PARMOT[IMPAR - 1], PMLAB[IMPAR - 1]))
	#ENDDO
	LU.write('#{}{}{}{}'.format())
	LU.write('#rho={:12.5f}kg\nm^3\n#mu={:12.5f}kg\nm-s\n#a={:12.5f}m\ns'.format( RHO, RMU, VSO))
	LU.write('#{}{}{}{}'.format())
	LU.write('#{}{}{}{}'.format(' 1         2        3          4          5        ',' 6            7         8       9        10        11    ','    12          13        14        15      16          17   ','        18      19'))
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	LU.write('#{}{}{}{}'.format())
	LU.write('#{}{}{}{}'.format(' V(m/s)    rpm      Dbeta      T(N)       Q(N-m)   ',' Pshaft(W)    Volts     Amps    effmot   effprop   adv   ','    CT          CP        DV(m/s)   eff     Pelec       Pprop','        cl_avg  cd_avg'))
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	#
	if (LRDUMP):
		CHARF = '#'
	else:
		CHARF = ' '
	#ENDIF
	#
	NVELM = MAX( 1 , NVEL-1 )
	DVEL = (VEL2-VEL1)/FLOAT(NVELM)
	#
	NDBETM = MAX( 1 , NDBET-1 )
	DDBET = (DBET2-DBET1)/FLOAT(NDBETM)
	#
	if (LRPMSET):
		NRPMM = MAX( 1 , NRPM-1 )
		PAR1 = RPM1
		PAR2 = RPM2
		DPAR = (RPM2-RPM1)/FLOAT(NRPMM)
		NPAR = NRPM
	elif (LVOLTSET):
		NVOLTM = MAX( 1 , NVOLT-1 )
		PAR1 = VOLT1
		PAR2 = VOLT2
		DPAR = (VOLT2-VOLT1)/FLOAT(NVOLTM)
		NPAR = NVOLT
	elif (LTHRUSET):
		NTHRUM = MAX( 1 , NTHRU-1 )
		PAR1 = THRU1
		PAR2 = THRU2
		DPAR = (THRU2-THRU1)/FLOAT(NTHRUM)
		NPAR = NTHRU
	elif (LTORQSET):
		NTORQM = MAX( 1 , NTORQ-1 )
		PAR1 = TORQ1
		PAR2 = TORQ2
		DPAR = (TORQ2-TORQ1)/FLOAT(NTORQM)
		NPAR = NTORQ
	elif (LAMPSSET):
		NAMPSM = MAX( 1 , NAMPS-1 )
		PAR1 = AMPS1
		PAR2 = AMPS2
		DPAR = (AMPS2-AMPS1)/FLOAT(NAMPSM)
		NPAR = NAMPS
	elif (LPELESET):
		NPELEM = MAX( 1 , NPELE-1 )
		PAR1 = PELE1
		PAR2 = PELE2
		DPAR = (PELE2-PELE1)/FLOAT(NPELEM)
		NPAR = NPELE
	else:
		print  'Additional parameter not specified'
		print  ' RPM   :',  lrpmset
		print  ' Volt  :',  lvoltset
		print  ' Thrust:',  lthruset
		print  ' Torque:',  ltorqset
		print  ' Amps  :',  lampsset
		print  ' Pelec :',  lpeleset
		raise SystemExit
	#ENDIF
	#
	LRDUMP = NVEL <=1 and  NRPM <=1 and  NVOLT<=1 and  NDBET<=1 and  NTHRU<=1 and  NTORQ<=1 and  NAMPS<=1 and  NPELE<=1
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	
	#
	for IDBET in fortranRangeTwoParam( 1, NDBET ):
		DBET = DBET1 + DDBET*FLOAT(IDBET-1)
		DBE = DBET * PI/180.0
		#
		for IPAR in fortranRangeTwoParam( 1, NPAR ):
			PAR = PAR1 + DPAR*FLOAT(IPAR-1)
			#
			if (LRPMSET ):
				RPM = PAR
			elif (LVOLTSET):
				VOLT = PAR
			elif (LTHRUSET):
				THRU = PAR
			elif (LTORQSET):
				TORQ = PAR
			elif (LAMPSSET ):
				AMPS = PAR
			elif (LPELESET):
				PELE = PAR
			#ENDIF
			#
			for IVEL in fortranRangeTwoParam( 1, NVEL ):
				VEL = VEL1 + DVEL*FLOAT(IVEL-1)
				#
				#---------- set initial omega
				if (LRPMSET):
					OMG = RPM * PI/30.0
					#
				elif (LVOLTSET):
					#----------- guess using 80% radius effective pitch angle
					I = MAX( 1 , (8*N)/10 )
					RT = R[I - 1]
					BT = B[I - 1] - CL0[I - 1]/DCLDA[I - 1] + DBE
					BT = MAX( 0.02 , MIN( 0.45*PI , BT ) )
					if ( isClose(VEL, 0.0) ):
						OMG = 1.0
					else:
						OMG = VEL/(RT*TAN(BT))
					#ENDIF
					#
				else:
					#----------- guess using 80% radius effective pitch angle
					I = MAX( 1 , (8*N)/10 )
					RT = R[I - 1]
					BT = B[I - 1] - CL0[I - 1]/DCLDA[I - 1] + DBE
					BT = MAX( 0.02 , MIN( 0.45*PI , BT ) )
					if ( isClose(VEL, 0.0) ):
						OMG = 1.0
					else:
						OMG = VEL/(RT*TAN(BT))
					#ENDIF
					#
					#----------- set voltage to get zero torque
					QP = 0.
					VOLTM (OMG,QP, IMOTYPE, PARMOT,NMPAR,VM,VM_OMG,VM_QP,AM,AM_OMG,AM_QP )
					# part of multi line command.
					# part of multi line command.
					VOLT = VM
				#ENDIF
				#
				#---------- Newton iteration to converge on trimmed omega
				for ITER in fortranRangeTwoParam( 1, 25 ):
					TQCALC (N,C,B,R,DR,VA,VT,CL,CD,STALL,BLDS,RAD,VEL,OMG,DBE,RHO,RMU,VSO,CL0,DCLDA,CLMIN,CLMAX,MCRIT,CD0,CD2U,CD2L,CLCD0,REREF,REEXP,TP, TP_VEL, TP_OMG, TP_DBE, TP_C, TP_B,QP, QP_VEL, QP_OMG, QP_DBE, QP_C, QP_B )
					# part of multi line command.
					# part of multi line command.
					# part of multi line command.
					# part of multi line command.
					# part of multi line command.
					# part of multi line command.
					# part of multi line command.
					#
					if (LRPMSET):
						#------------- Residual  =  prop omega  -  prescribed omega
						RES = 0.
						RES_OMG = 1.0
						#
						#------------- Newton change
						DOMG = -RES/RES_OMG
						DVOLT = 0.
						#
						#------------- set voltage = f(w,Q) by inverting MOTORQ's Q(w,voltage) function
						VOLTM (OMG,QP, IMOTYPE, PARMOT,NMPAR,VM,VM_OMG,VM_QP,AM,AM_OMG,AM_QP )
						# part of multi line command.
						# part of multi line command.
						VOLT = VM
						AMPS = AM
						#
					elif (LVOLTSET):
						MOTORQ (OMG,VOLT, IMOTYPE, PARMOT,NMPAR,QM,QM_OMG,QM_VOLT,AM,AM_OMG,AM_VOLT )
						# part of multi line command.
						# part of multi line command.
						#
						#------------- Residual  =  prop torque - motor torque  at current omega
						RES     = QP     - QM
						RES_OMG = QP_OMG - QM_OMG
						#
						#------------- Newton change
						DOMG = -RES/RES_OMG
						DVOLT = 0.
						#
						AMPS = AM
						#
					elif (LTHRUSET):
						MOTORQ (OMG,VOLT, IMOTYPE, PARMOT,NMPAR,QM,QM_OMG,QM_VOLT,AM,AM_OMG,AM_VOLT )
						# part of multi line command.
						# part of multi line command.
						#
						#------------- Residual  =  prop torque - motor torque  at current omega
						RES1      = QP     - QM
						RES1_OMG  = QP_OMG - QM_OMG
						RES1_VOLT =        - QM_VOLT
						#
						#------------- Residual  =  prop thrust - specified thrust
						RES2      = TP     - THRU
						RES2_OMG  = TP_OMG
						RES2_VOLT = 0.
						#
						A11 = RES1_OMG
						A12 = RES1_VOLT
						A21 = RES2_OMG
						A22 = RES2_VOLT
						#
						DET   =   A11 *A22  - A12 *A21
						DOMG  = -(RES1*A22  - A12 *RES2) / DET
						DVOLT = -(A11 *RES2 - RES1*A21 ) / DET
						#
						AMPS = AM
						#
					elif (LTORQSET):
						MOTORQ (OMG,VOLT, IMOTYPE, PARMOT,NMPAR,QM,QM_OMG,QM_VOLT,AM,AM_OMG,AM_VOLT )
						# part of multi line command.
						# part of multi line command.
						#
						#------------- Residual  =  prop torque - motor torque  at current omega
						RES1      = QP     - QM
						RES1_OMG  = QP_OMG - QM_OMG
						RES1_VOLT =        - QM_VOLT
						#
						#------------- Residual  =  prop torque - specified torque
						RES2      = QP     - TORQ
						RES2_OMG  = QP_OMG
						RES2_VOLT = 0.
						#
						A11 = RES1_OMG
						A12 = RES1_VOLT
						A21 = RES2_OMG
						A22 = RES2_VOLT
						#
						DET   =   A11 *A22  - A12 *A21
						DOMG  = -(RES1*A22  - A12 *RES2) / DET
						DVOLT = -(A11 *RES2 - RES1*A21 ) / DET
						#
						AMPS = AM
						#
					elif (LAMPSSET):
						MOTORQ (OMG,VOLT, IMOTYPE, PARMOT,NMPAR,QM,QM_OMG,QM_VOLT,AM,AM_OMG,AM_VOLT )
						# part of multi line command.
						# part of multi line command.
						#
						#------------- Residual  =  prop torque - motor torque  at current omega
						RES1      = QP     - QM
						RES1_OMG  = QP_OMG - QM_OMG
						RES1_VOLT =        - QM_VOLT
						#
						#------------- Residual  = amps - specified amps
						RES2      = AM   - AMPS
						RES2_OMG  = AM_OMG
						RES2_VOLT = AM_VOLT
						#
						A11 = RES1_OMG
						A12 = RES1_VOLT
						A21 = RES2_OMG
						A22 = RES2_VOLT
						#
						DET   =   A11 *A22  - A12 *A21
						DOMG  = -(RES1*A22  - A12 *RES2) / DET
						DVOLT = -(A11 *RES2 - RES1*A21 ) / DET
						#
					elif (LPELESET):
						MOTORQ (OMG,VOLT, IMOTYPE, PARMOT,NMPAR,QM,QM_OMG,QM_VOLT,AM,AM_OMG,AM_VOLT )
						# part of multi line command.
						# part of multi line command.
						#
						#------------- Residual  =  prop torque - motor torque  at current omega
						RES1      = QP     - QM
						RES1_OMG  = QP_OMG - QM_OMG
						RES1_VOLT =        - QM_VOLT
						#
						#------------- Residual  =  Pele - specified Pele
						RES2      = VOLT*AM   - PELE
						RES2_OMG  = VOLT*AM_OMG
						RES2_VOLT = VOLT*AM_VOLT + AM
						#
						A11 = RES1_OMG
						A12 = RES1_VOLT
						A21 = RES2_OMG
						A22 = RES2_VOLT
						#
						DET   =   A11 *A22  - A12 *A21
						DOMG  = -(RES1*A22  - A12 *RES2) / DET
						DVOLT = -(A11 *RES2 - RES1*A21 ) / DET
						#
						AMPS = AM
						#
					#ENDIF
					#
					RLX = 1.0
					if (RLX*DOMG  >  1.0*OMG): RLX =  1.0*OMG/DOMG
					if (RLX*DOMG  < -0.5*OMG): RLX = -0.5*OMG/DOMG
					#
					if (RLX*DVOLT >  2.0*VOLT): RLX =  2.0*VOLT/DVOLT
					if (RLX*DVOLT < -0.5*VOLT): RLX = -0.5*VOLT/DVOLT
					
					#           write(*,'(1x,i3,2(f12.3,e12.4),f7.3)')
					#    &            iter, omg, domg, volt, dvolt, rlx
					
					#------------ convergence check
					IF(ABS(DOMG) < EPS*ABS(OMG)) GO TO 110
					#
					#------------ Newton update
					OMG  = OMG  + RLX*DOMG
					VOLT = VOLT + RLX*DVOLT
				#100 CONTINUE
				#            WRITE(*,*) 'QPROP: Convergence failed. Res =', RES
				#
				#110 CONTINUE
				
				#        Q = 1.0 / (Kv*pi/30.0) * (I-Io)
				#        I = Io + Q*(Kv*pi/30.0)
				#        P = (V-I*R) * (I-Io)
				#        eff = P / (I*V)
				#        rpm = Kv * (V-I*R)
				#
				#---------- compute thrust-average blade cl and cd
				DTSUM = 0.
				CLAVG = 0.
				CDAVG = 0.
				for I in fortranRangeTwoParam( 1, N ):
					WA = VEL + VA[I - 1]
					WT = OMG*R[I - 1] - VT[I - 1]
					WSQ = WA**2 + WT**2
					DTSUM = DTSUM + WSQ*C[I - 1]*DR[I - 1]
					CLAVG = CLAVG + WSQ*C[I - 1]*DR[I - 1]*CL[I - 1]
					CDAVG = CDAVG + WSQ*C[I - 1]*DR[I - 1]*CD[I - 1]
				#ENDDO
				CLAVG = CLAVG / DTSUM
				CDAVG = CDAVG / DTSUM
				#
				#---------- print output
				RPM = OMG*30.0/PI
				PPROP = TP*VEL
				POWER = QP*OMG
				#
				PINPUT = VOLT*AMPS
				#
				if ( not isClose(POWER, 0.0) ):
					EFFP = PPROP/POWER
				else:
					EFFP = 0.
				#ENDIF
				#
				if ( not isClose(PINPUT, 0.0) ):
					EFFM = POWER/PINPUT
				else:
					EFFM = 0.0
				#ENDIF
				#
				EFF = EFFM*EFFP
				#
				if (ABS(OMG)>0.0):
					ADV = VEL/(OMG*RAD)
				else:
					ADV = 0.
				#ENDIF
				#
				if ( isClose(OMG, 0.0) ):
					WRI = 0.
				else:
					WRI = 1.0 / (OMG*RAD)
				#ENDIF
				#
				CT = TP * WRI**2 * 2.0 / (RHO * PI * RAD**2)
				CP = QP * WRI**2 * 2.0 / (RHO * PI * RAD**3)
				DV = SQRT(VEL**2 + TP * 2.0/(RHO*PI*RAD**2)) - VEL
				#
				LU.write('{}{:8.3f}{:12.4f}{:7.3f}{:12.4f}{:12.4f}{:12.4f}{:8.3f}{:10.4f}{:9.4f}{:9.4f}{:10.5f}{:12.4f}{:12.4f}{:9.4f}{:9.4f}{:12.4f}{:12.4f}{:9.4f}{:12.4f}'.format( CHARF,VEL,   RPM,  DBET,   TP,     QP, POWER,VOLT,AMPS,  EFFM,  EFFP, ADV, CT, CP, DV,EFF, PINPUT, PPROP, CLAVG, CDAVG))
				# part of multi line command.
				# part of multi line command.
				# part of multi line command.
				# 2100       FORMAT(A,F8.3,  G12.4,  F7.3, G12.4, G12.4, G12.4,F8.3, F10.4,  F9.4, F9.4, F10.5, G12.4, G12.4, F9.4,F9.4, G12.4, G12.4, F9.4, G12.4)
				# part of multi line command.
				# part of multi line command.
				# part of multi line command.
			#ENDDO
			IF(.NOT.LRDUMP  and  NVEL>1) WRITE(LU,1000)
		#ENDDO
	#ENDDO
	#
	if (LRDUMP):
		#----- dump radial distributions
		LU.write('#{}{}{}{}'.format())
		LU.write('#{}{}{}{}'.format(' radius   chord   beta      Cl       Cd       Re    Mach','     effi     effp    Wa(m/s)     Aswirl      adv_wake'))
		# part of multi line command.
		# part of multi line command.
		#                              123456789012123456789012123456789012
		for I in fortranRangeTwoParam( 1, N ):
			WA = VEL + VA[I - 1]
			WT = OMG*R[I - 1] - VT[I - 1]
			WSQ = WA**2 + WT**2
			W = SQRT(WSQ)
			#
			#------- local Mach and Re
			AMA = W/VSO
			IRE = INT( RHO*W*C[I - 1]/RMU + 0.5 )
			#
			#------- local wake advance ratio, induced and profile efficiencies
			if ( not isClose(WA, 0.0)  and  not isClose(WT, 0.0) ):
				ADW = (WA/WT) * (R[I - 1]/RAD)
				EFFI = (VEL/(OMG*R[I - 1])) * (WT/WA)
				EFFP = (CL[I - 1] - CD[I - 1]*WA/WT)/ (CL[I - 1] + CD[I - 1]*WT/WA)
				# part of multi line command.
			else:
				ADW = 0.
				EFFI = 0.
				EFFP = 0.
			#ENDIF
			#
			EFFI = MAX( -99.0 , MIN( 99.0 , EFFI ) )
			EFFP = MAX( -99.0 , MIN( 99.0 , EFFP ) )
			#
			RU = R[I - 1]
			CU = C[I - 1]
			BU = B[I - 1] * 180.0/PI
			#
			#------- swirl flow angle in non-rotating frame
			ASWIRL = ATAN2( VT[I - 1] , WA ) * 180.0/PI
			#
			LU.write(' {:8.4f}{:8.4f}{:8.3f}{:9.4f}{:9.5f}{:9d}{:7.3f}{:9.4f}{:9.4f}{:12.4f}{:12.4f}{:12.4f}'.format(RU,   CU,   BU, CL[I - 1], CD[I - 1],IRE,  AMA, EFFI, EFFP, WA, ASWIRL, ADW))
			# part of multi line command.
			# part of multi line command.
			# 3100    FORMAT(1X,F8.4, F8.4, F8.3, F9.4,  F9.5,I9, F7.3, F9.4, F9.4, G12.4, G12.4, G12.4)
			# part of multi line command.
		#ENDDO
	#ENDIF
	#
	raise SystemExit
	#
	#900 CONTINUE
	print '\nReaderror\ninfile:{}\nline{:3d}:{}'.format( FILNAM[1:64 - 1], ILINE, LINE)
	# 9000 FORMAT(/' Read error'/'   in file:  ', A/'   line',I3,':  ', A)
	# part of multi line command.
	# part of multi line command.
	raise SystemExit
	#
	#950 CONTINUE
	print '\nUnexpectedend-of-filereached\ninfile:{}\nline{:3d}'.format( FILNAM[1:64 - 1], ILINE)
	# 9500 FORMAT(/' Unexpected end-of-file reached'/'   in file:  ', A/'   line',I3)
	# part of multi line command.
	# part of multi line command.
	raise SystemExit
	#
	#
	#980 CONTINUE
	print '\nUnexpectedend-of-filereached\ninfile:{}\nline{:3d}'.format( FILNAM[1:64 - 1], ILINE)
	# 9800 FORMAT(/' Fewer parameters than required'/'   in file:  ', A/'   line',I3)
	# part of multi line command.
	# part of multi line command.
	raise SystemExit
	#
#END # QPROP
