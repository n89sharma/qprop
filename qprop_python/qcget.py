def QCGET(RHO,RMU,VSO):
    fname = '../qprop_source/runs/qcon.def'
    with open(fname, 'r') as f:
        for line in f:
            print line


    # OPEN(LU,FILE=FNAME,STATUS='OLD',ERR=90)
    # ILINE = 0
    #
    # #---- extract parameters on data lines
    # NVAL = 1
    # CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
    # RHO = RVAL
    #
    # #---- extract parameters on data lines
    # NVAL = 1
    # CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
    # RMU = RVAL
    #
    # #---- extract parameters on data lines
    # NVAL = 1
    # CALL RREAD(LU,LINE,ILINE,IERR,NVAL,RVAL)
    # VSO = RVAL


if __name__ == '__main__':
    QCGET(1,1,1)
