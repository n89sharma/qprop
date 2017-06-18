
def TPDES (TDES1,PDES1, RHO,VEL,OMG, RAD,BLDS,N, R,DR, CL,CD, CL0,DCLDA,GAM, CH, BETA, ADW):
	# part of multi line command.
	# part of multi line command.
	#---------------------------------------------------------------
	#     Computes approximate propeller geometry
	#     using a modified Larrabee algorithm
	#
	#  Input:
	#      TDES1    prescribed thrust (0 if power prescribed)
	#      PDES1    prescribed power  (0 if thrust prescribed)
	#                (if both TDES1=PDES1=0.0, impose maximum windmill power)
	#      RHO      density
	#      VEL      freestream velocity
	#      OMG      rotation rate,  in radians/time-unit
	#      RAD      tip radius
	#      BLDS     number of blades
	#
	#      N        number of radial stations
	#      R[.]     radii where geometry is to be defined
	#      DR[.]    integration radius intervals,   Int [ ] dr  =  Sum [ ]_i DR[i]
	#      CL[.]    prescribed CL
	#      CD[.]    prescribed CD
	#      CL0[.]   zero-lift angle
	#      DCLDA[.] 2D lift-curve slope  dcl/da
	#
	#  Output:
	#     GAM[.]    circulation
	#     CH[.]     chord
	#     BETA[.]   pitch angle (radians)
	#     ADW       wake advance ratio
	#
	#---------------------------------------------------------------
	R, DR, CL, CD, CL0, DCLDA = [0.0]*N ,[0.0]*N ,[0.0]*N ,[0.0]*N ,[0.0]*N ,[0.0]*N
	GAM, CH, BETA = [0.0]*N ,[0.0]*N ,[0.0]*N
	#
	PI =  3.14159265 
	#
	TDES = TDES1
	PDES = PDES1
	#
	#---- set VD = velocity through disk, using actuator-disk theory
	#
	#-----------------------------------------------------------------
	if ( not isClose(TDES1, 0.0) ):
		
		#----- Betz limit estimates
		DVDMIN = -0.50*VEL
		TMIN = RHO*PI*RAD**2* 2.0*( DVDMIN**2 + VEL*DVDMIN )
		# part of multi line command.
		#
		#----- specified thrust -- used thrust relation for actuator disk
		if (TDES < TMIN):
			print  'Limiting windmill thrust to Betz limit:', TMIN
			TDES = 0.99 * TMIN
			DVD  = 0.90 * DVDMIN
		else:
			VT2 = TDES / (0.5*RHO * PI*RAD**2)
			DVD = 0.5*( SQRT(VEL**2 + VT2) - VEL )
		#ENDIF
		#
		VD = VEL + DVD
		TC = TDES / (0.5*RHO*VD**2 * PI*RAD**2)
		#
		#-----------------------------------------------------------------
	else:
		#----- r^2 weighted cd/cl ratio
		EPSR3 = 0.
		for I in fortranRangeTwoParam( 1, N ):
			EPSR3 = EPSR3 + CD[I]/CL[I] * R[I]**2 * DR[I]
		#ENDDO
		#
		#----- additional effective velocity through disk accounting for viscous power loss
		VVIS = 2.0*OMG*EPSR3/RAD**2
		#
		#----- Betz limit estimates
		#    DVDMIN = -0.33*VEL
		DVDMIN =  0.33*(SQRT(VEL**2+VEL*VVIS+VVIS**2) - 2.0*VEL - VVIS)
		PMIN = RHO*PI*RAD**2* 2.0*(                 DVDMIN**3+ (2.0*VEL+VVIS)*DVDMIN**2+ VEL*(VEL+VVIS)*DVDMIN   )
		# part of multi line command.
		# part of multi line command.
		# part of multi line command.
		#
		#----- specified power -- use power relation for actuator disk
		if ( isClose(PDES1, 0.0)  or  PDES < PMIN):
			#------ guess power as nearly the actuator-disk Betz limit
			print  'Limiting windmill power to Betz limit:', PMIN
			PDES = 0.99 * PMIN
			DVD  = 0.90 * DVDMIN
			#
		else:
			#------ estimate DVD from power disk loading
			VP3 = PDES / (0.5*RHO * PI*RAD**2)
			DVDA = ABS(VP3)**(1.0/3.0)
			DVD = SIGN( DVDA , VP3 )
			#
			DVD = MAX( DVD , DVDMIN+0.1*DVD0 )
		#ENDIF
		#
		DVD0 = ABS(DVD)
		#
		#----- Newton iteration for actual DVD
		for ITER in fortranRangeTwoParam( 1, 20 ):
			RES     =                DVD**3+ (2.0*VEL+VVIS)*DVD**2+ VEL*(VEL+VVIS)*DVD- 0.25 * PDES/(0.5*RHO * PI*RAD**2)
			# part of multi line command.
			# part of multi line command.
			# part of multi line command.
			RES_DVD =            3.0*DVD**2+ (2.0*VEL+VVIS)*DVD * 2.0+ VEL*(VEL+VVIS)
			# part of multi line command.
			# part of multi line command.
			#
			DEL = -RES/RES_DVD
			
			#         write(*,'(1x,i3,4f12.4)') iter, dvd, res, res_dvd, del
			
			if ( DVD == DVDMIN  and  DEL<0.0): 
				print  '? Windmill power exceeds Betz limit:', PMIN
				GO TO 8
			#ENDIF
			DVD = DVD + DEL
			IF(ABS(DEL/DVD0) < 1.0E-5) GO TO 8
			DVD = MAX( DVD , DVDMIN )
		#ENDDO
		print  '***  Vd convergence failed.  dVd =', DEL
		#8 CONTINUE
		#
		VD = VEL + DVD
		PC = PDES/(0.5*RHO*VD**3 * PI*RAD**2)
	#ENDIF
	#-----------------------------------------------------------------
	#
	ADV = VEL/(OMG*RAD)
	ADW = VD /(OMG*RAD)
	#
	#
	print '\n-----------------------------------------------------\nLarrabee-methodinitialization...\n\nbladesB={:2d}\ntrueadvanceratioV\nwR={:8.4f}\nwakeadvanceratioVd\nwR={:8.4f}'.format( INT(BLDS), ADV, ADW)
	# 1050 FORMAT(/' -----------------------------------------------------'/' Larrabee-method initialization...'//'   blades  B =', I2,/'   true advance ratio  V /wR =', F8.4/'   wake advance ratio  Vd/wR =', F8.4 )
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	# part of multi line command.
	#
	#---- go over radial stations, summing I1, I2, J1, J2 integrals
	print 
	print '  r/R    wr/Vd     F       G    dI1/dx  dI2/dx  dJ1/dx  dJ2/dx'
	# part of multi line command.
	RI1 = 0.
	RI2 = 0.
	RJ1 = 0.
	RJ2 = 0.
	for I in fortranRangeTwoParam( 1, N ):
		XI = R[I]/RAD
		DXI = DR[I]/RAD
		#
		XX = XI/ADW
		#
		ARG = MIN( 20.0 , 0.5*BLDS*(1.0-XI)/ADW )
		EK = EXP(-ARG)
		F = ATAN2( SQRT(1.0 - EK*EK) , EK )*2.0/PI
		#
		FMOD = F * SQRT(1.0 + (4.0*ADW/(PI*BLDS*XI))**2)
		#
		G = FMOD * XX*XX / (1.0 + XX*XX)
		DOL = CD[I]/CL[I]
		RIX1 = 4.0*XI*G*(1.0 - DOL/XX)
		RIX2 = 2.0*XI*G*(1.0 - DOL/XX) / (1.0 + XX*XX)
		RJX1 = 4.0*XI*G*(1.0 + DOL*XX)
		RJX2 = 2.0*XI*G*(1.0 + DOL*XX) * XX*XX / (1.0 + XX*XX)
		RI1 = RI1 + RIX1*DXI
		RI2 = RI2 + RIX2*DXI
		RJ1 = RJ1 + RJX1*DXI
		RJ2 = RJ2 + RJX2*DXI
		print '  {:5.3f}7{:8.4f}'.format( XI, XX, F, G, RIX1,RIX2,RJX1,RJX2)
		# 1100   FORMAT(2X,F5.3, 7F8.4)
		#
		#------ save G for re-use later
		GAM[I] = G
	#10 CONTINUE
	#
	print '\n\n{:1d}={:8.4f}{:2d}={:8.4f}J1={:8.4f}J2={:8.4f}'.format( RI1,RI2,RJ1,RJ2)
	# 1120 FORMAT(// ' I1 = ', F8.4, '    I2 = ', F8.4,'       J1 = ', F8.4, '    J2 = ', F8.4 )
	# part of multi line command.
	#
	#---- Calculate displacement velocity and total efficiency
	if ( not isClose(TDES1, 0.0) ):
		DISCR = 1.0 - 4.0*TC*RI2/RI1**2
		if (DISCR<0.0):
			print  'Setting thrust to minimum'
			DISCR = 0.0
			TC = 0.25*RI1**2 / RI2
		#ENDIF
		ZETA = 0.5*RI1/RI2 * (1.0 - SQRT(DISCR))
		PC = RJ1*ZETA + RJ2*ZETA**2
		ETA = TC/PC * VEL/VD
		T = 0.5*RHO*VD**2 * PI*RAD**2 * TC
		P = 0.5*RHO*VD**3 * PI*RAD**2 * PC
		print '\nTc={:9.4f}T={:13.5f}=>zeta={:9.4f}\nPc={:9.4f}P={:13.5f}eff.={:9.4f}'.format( TC, T, ZETA, PC, P, ETA)
		# 1150  FORMAT(/ ' Tc =', F9.4,'  T =',G13.5,' =>   zeta =', F9.4/ ' Pc =', F9.4,'  P =',G13.5,'      eff. =', F9.4)
		# part of multi line command.
		#
	else:
		DISCR = 1.0 + 4.0*PC*RJ2/RJ1**2
		if (DISCR < 0.0  or  isClose(PDES1, 0.0) ):
			print  'Setting power to minimum'
			DISCR = 0.0
			PC = -0.25*RJ1**2 / RJ2
		#ENDIF
		ZETA = 0.5*RJ1/RJ2 * (SQRT(DISCR) - 1.0)
		TC = RI1*ZETA - RI2*ZETA**2
		ETA = TC/PC * VEL/VD
		T = 0.5*RHO*VD**2 * PI*RAD**2 * TC
		P = 0.5*RHO*VD**3 * PI*RAD**2 * PC
		print '\nPc={:9.4f}P={:13.5f}=>zeta={:9.4f}\nTc={:9.4f}T={:13.5f}eff.={:9.4f}'.format( PC, P, ZETA, TC, T, ETA)
		# 1160  FORMAT(/ ' Pc =', F9.4,'  P =',G13.5,' =>   zeta =', F9.4/ ' Tc =', F9.4,'  T =',G13.5,'      eff. =', F9.4)
		# part of multi line command.
	#ENDIF
	#
	#---------------------------------------------------------
	#---- Calculate geometry
	for I in fortranRangeTwoParam( 1, N ):
		G = GAM[I]
		#
		PHI = ATAN2( VEL + 0.5*ZETA*VD , OMG*R[I] )
		WSQ = VEL**2 + (OMG*R[I])**2 - (VD*0.5*ZETA*COS(PHI))**2
		W = SQRT(WSQ)
		
		BETA[I] = PHI + (CL[I]-CL0[I])/DCLDA[I]
		#
		GAM[I] = G * (2.0*PI/BLDS) * ZETA * VD**2/OMG
		CH[I] = 2.0*GAM[I]/(W*CL[I])
	#ENDDO
	#
	return
#END # TPDES

