#***********************************************************************
#    Module:  bnsolv.f
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

def BNSOLV (A,B,C,R,NB,N,NRHS,NRMAX):
	DIMENSION A(NB,NB,N), B(NB,NB,N), C(NB,NB,N), R(NB,NRMAX,N)
	#     **********************************************************************
	#      This routine solves an N-long block-tridiagonal system with NBxNB
	#      blocks, and with NRHS righthand sides by a standard block elimination
	#      scheme.  The solutions are returned in the Rj vectors.
	#
	#      |A C      ||d|   |R..   |
	#      |B A C    ||d|   |R..   |
	#      |  B . .  ||.| = |R.....|
	#      |    . . C||.|   |R..   |
	#      |      B A||d|   |R..   |
	#                                                  Mark Drela   10 June 89
	#     **********************************************************************
	#
	#** Forward sweep: Elimination of lower block diagonal (B's).
	for I in fortranRangeTwoParam( 1, N ):
		#
		IM = I-1
		#
		#------ don't eliminate first B block because it doesn't exist
		IF( I == 1 ) GO TO 12
		#
		#------ eliminate Bi block, thus modifying Ai and Ci blocks
		for K in fortranRangeTwoParam( 1, NB ):
			for J in fortranRangeTwoParam( 1, NB ):
				BTMP = B(K,J,I)
				for L in fortranRangeTwoParam( 1, NB ):
					A(K,L,I) = A(K,L,I) - BTMP*C(J,L,IM)
				#1111 CONTINUE
				for L in fortranRangeTwoParam( 1, NRHS ):
					R(K,L,I) = R(K,L,I) - BTMP*R(J,L,IM)
				#1112 CONTINUE
			#111 CONTINUE
		#11 CONTINUE
		#
		#                                                              -1
		#---- multiply Ci block and righthand side Ri vectors by (Ai)
		#       using Gaussian elimination.
		#
		for KPIV in fortranRangeTwoParam( 1, NB-1 ):
			KP1 = KPIV+1
			#
			#-------- find max pivot index KX
			KX = KPIV
			for K in fortranRangeTwoParam( KP1, NB ):
				IF(ABS(A(K,KPIV,I))-ABS(A(KX,KPIV,I))) 131,131,1311
				1311        KX = K
			#131 CONTINUE
			#
			if (A(KX,KPIV,I).EQ.0.0):
				print  'Singular A block, i = ',I
				raise SystemExit
			#ENDIF
			#
			PIVOT = 1.0/A(KX,KPIV,I)
			#
			#-------- switch pivots
			A(KX,KPIV,I) = A(KPIV,KPIV,I)
			#
			#-------- switch rows & normalize pivot row
			for L in fortranRangeTwoParam( KP1, NB ):
				TEMP = A(KX,L,I)*PIVOT
				A(KX,L,I) = A(KPIV,L,I)
				A(KPIV,L,I) = TEMP
			#132 CONTINUE
			#
			for L in fortranRangeTwoParam( 1, NB ):
				TEMP = C(KX,L,I)*PIVOT
				C(KX,L,I) = C(KPIV,L,I)
				C(KPIV,L,I) = TEMP
			#133 CONTINUE
			#
			for L in fortranRangeTwoParam( 1, NRHS ):
				TEMP = R(KX,L,I)*PIVOT
				R(KX,L,I) = R(KPIV,L,I)
				R(KPIV,L,I) = TEMP
			#134 CONTINUE
			#
			#-------- forward eliminate everything
			for K in fortranRangeTwoParam( KP1, NB ):
				ATMP = A(K,KPIV,I)
				for L in fortranRangeTwoParam( KP1, NB ):
					A(K,L,I) = A(K,L,I) - ATMP*A(KPIV,L,I)
				#1351 CONTINUE
				for L in fortranRangeTwoParam( 1, NB ):
					C(K,L,I) = C(K,L,I) - ATMP*C(KPIV,L,I)
				#1352 CONTINUE
				for L in fortranRangeTwoParam( 1, NRHS ):
					R(K,L,I) = R(K,L,I) - ATMP*R(KPIV,L,I)
				#1353 CONTINUE
			#135 CONTINUE
			#
		#13 CONTINUE
		#
		#------ solve for last row
		if (A(NB,NB,I).EQ.0.0):
			print  'Singular A block, i = ',I
			raise SystemExit
		#ENDIF
		PIVOT = 1.0/A(NB,NB,I)
		for L in fortranRangeTwoParam( 1, NB ):
			C(NB,L,I) = C(NB,L,I)*PIVOT
		#141 CONTINUE
		for L in fortranRangeTwoParam( 1, NRHS ):
			R(NB,L,I) = R(NB,L,I)*PIVOT
		#142 CONTINUE
		#
		#------ back substitute everything
		for KPIV in fortranRangeTwoParam( NB-1, 1 ):
			KP1 = KPIV+1
			for K in fortranRangeTwoParam( KP1, NB ):
				ATMP = A(KPIV,K,I)
				for L in fortranRangeTwoParam( 1, NB ):
					C(KPIV,L,I) = C(KPIV,L,I) - ATMP*C(K,L,I)
				#1511 CONTINUE
				for L in fortranRangeTwoParam( 1, NRHS ):
					R(KPIV,L,I) = R(KPIV,L,I) - ATMP*R(K,L,I)
				#1512 CONTINUE
			#151 CONTINUE
		#15 CONTINUE
	#1 CONTINUE
	#
	#** Backward sweep: Back substitution using upper block diagonal (Ci's).
	for I in fortranRangeTwoParam( N-1, 1 ):
		IP = I+1
		for L in fortranRangeTwoParam( 1, NRHS ):
			for K in fortranRangeTwoParam( 1, NB ):
				for J in fortranRangeTwoParam( 1, NB ):
					R(K,L,I) = R(K,L,I) - R(J,L,IP)*C(K,J,I)
				#2111 CONTINUE
			#211 CONTINUE
		#21 CONTINUE
	#2 CONTINUE
	#
	return
#END # BNSOLV
