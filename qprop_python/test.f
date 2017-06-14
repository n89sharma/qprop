DO 11 K=1, NB
  DO 111 J=1, NB
    BTMP = B(K,J,I)
    DO 1111 L=1, NB
      A(K,L,I) = A(K,L,I) - BTMP*C(J,L,IM)
1111       CONTINUE
    DO 1112 L=1, NRHS
      R(K,L,I) = R(K,L,I) - BTMP*R(J,L,IM)
1112       CONTINUE
111     CONTINUE
11   CONTINUE
asdf
