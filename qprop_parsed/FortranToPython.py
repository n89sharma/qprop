import re
import sys

def beginningOfLoopOrConditional(lineContent):
    return  re.match(r'\bPROGRAM\b', lineContent) or\
            re.match(r'\bSUBROUTINE\b', lineContent) or\
            re.match(r'\bDO\b', lineContent) or\
            re.match(r'\bIF\b.*\bTHEN\b', lineContent)

def intermediateConditionalStatements(lineContent):
    return  re.match(r'\bELSEIF\b', lineContent) or\
            re.match(r'\bELSE\b',   lineContent)

def terminationOfLoopOrConditional(lineContent):
    return  re.match(r'\bEND\b', lineContent) or\
            re.match(r'\bENDDO\b', lineContent) or\
            re.match(r'\bENDIF\b', lineContent)

def continueLine(lineContent):
    reg = re.compile(r'^.*(?P<lineLabel>\d+)\s+CONTINUE.*$')
    match = reg.match(lineContent)
    if match:
        return match.group('lineLabel')
    else:
        return False

def doLoopWithLineLabel(lineContent):
    reg = re.compile(r'^.*DO\s+(?P<lineLabel>\d+).*$')
    match = reg.match(lineContent)
    if match:
        return match.group('lineLabel')
    else:
        return False

def partOfMultilineCommand(lineContent):
    if re.match(r'^&', lineContent):
        return True, lineContent.replace('&', '').strip()
    else:
        return False, lineContent

def writeListToFile (lineList, filePath):
    with open(filePath, 'w') as fw:
        for line in lineList:
            fw.write(line)

def collapseMultiLines(sourceFilePath, tempFilePath):
    # COLLAPSE ALL MULTILINE COMMANDS (USING '&'')
    fileData = []
    mainCommandLine = 0
    with open(sourceFilePath, 'r') as fr:
        for lineNumber, lineContent in enumerate(fr):
            lineContent = lineContent.strip()

            isPartOfMultiLineComman, lineContent = partOfMultilineCommand(lineContent)
            if(isPartOfMultiLineComman):
                fileData.append('# part of multi line command.\n')
                fileData[mainCommandLine] = fileData[mainCommandLine].strip() + lineContent + '\n'
                # print  fileData[mainCommandLine]
            else:
                fileData.append(lineContent + '\n')
                mainCommandLine = lineNumber

    writeListToFile(fileData, tempFilePath)


def addTabs(tempFilePath, formattedFilePath):
    # ADD THE TABS
    mainList = [[]]
    currLevel = 0
    totalLines = 0
    doLoopLineLabels = []
    with open(tempFilePath, 'r') as fr:
        for lineNumber, lineContent in enumerate(fr):
            lineContent = lineContent.strip()

            # try:
            lineLabelLoop       = doLoopWithLineLabel(lineContent)
            lineLabelContinue   = continueLine(lineContent)

            if(beginningOfLoopOrConditional(lineContent)):
                mainList[currLevel].append((lineNumber, lineContent))
                currLevel += 1
                if len(mainList) == currLevel:
                    mainList.append([])

                if lineLabelLoop:
                    doLoopLineLabels.append(lineLabelLoop)

            elif(intermediateConditionalStatements(lineContent)):
                mainList[currLevel - 1].append((lineNumber, lineContent))

            elif(terminationOfLoopOrConditional(lineContent) or
                    (lineLabelContinue and
                    lineLabelContinue in doLoopLineLabels)):
                mainList[currLevel - 1].append((lineNumber, lineContent))
                currLevel -= 1

            else:
                mainList[currLevel].append((lineNumber, lineContent))
            # except:
            #     print sys.exc_info()
            #     print 'ERROR!'
            #     print 'currLevel ', currLevel
            #     print lineNumber, lineContent

            totalLines = lineNumber

    finalFileList = [i for i in range(0, totalLines + 1)]
    for tabNumber, lineDataList in enumerate(mainList):
        for lineNum, lineContent in lineDataList:
            finalFileList[lineNum] = '\t' * tabNumber + lineContent.strip() + '\n'

    writeListToFile(finalFileList, formattedFilePath)

def replaceCommands(tempFilePath, formattedFilePath):

    pythonLines = []
    with open( tempFilePath, 'r' ) as rf:
        for line in rf:
            pline = line
            pline = pline.replace('!', '#')
            pline = re.sub(r'^(?P<tab>\t*)[Cc]+(?P<commentValue>(?![a-zA-Z0-9\(]).*)$',                                           '\g<tab>#\g<commentValue>',                                           pline)
            pline = re.sub(r'^(?P<tab>\t*)PROGRAM\s+(?P<programName>\w+).*$',                                                   '\g<tab>def \g<programName> ():',                                       pline)
            pline = re.sub(r'^(?P<tab>\t*)SUBROUTINE\s+(?P<routineName>\w+)\s*\((?P<routineParameters>.*)\).*$',                '\g<tab>def \g<routineName> (\g<routineParameters>):',                  pline)
            pline = re.sub(r'^(?P<tab>\t*)DATA\s+(?P<variableName>\w+)\s+/(?P<variableValue>.*)/.*$',                           '\g<tab>\g<variableName> = \g<variableValue>',                          pline)
            pline = re.sub(r'^(?P<tab>\t*)DO\s+(?P<variableName>\w+)\s*=\s*(?P<fromNum>[\d\w]+)\s*,\s*(?P<toNum>[\d\w]+).*$',   '\g<tab>for \g<variableName> in range( \g<fromNum>, \g<toNum> ):',      pline)
            pline = re.sub(r'^(?P<tab>\t*)CALL\s+(?P<routineName>\w+)\s*\((?P<routineParameters>.*)\).*$',                      '\g<tab>\g<routineName> (\g<routineParameters>)',                       pline)
            pline = re.sub(r'^(?P<tab>\t*)IF\s*\((?P<logicalComparison>.*)\)\s*THEN.*$',                                        '\g<tab>if (\g<logicalComparison>):',                                   pline)
            pline = re.sub(r'^(?P<tab>\t*)ELSEIF\s*\((?P<logicalComparison>.*)\)\s*THEN.*$',                                    '\g<tab>elif (\g<logicalComparison>):',                                 pline)
            pline = re.sub(r'^(?P<tab>\t*)ELSE IF\s*\((?P<logicalComparison>.*)\)\s*THEN.*$',                                   '\g<tab>elif (\g<logicalComparison>):',                                 pline)
            pline = re.sub(r'^(?P<tab>\t*)ELSE.*$',                                                                             '\g<tab>else:',                                                         pline)
            pline = re.sub(r'^(?P<tab>\t*)ENDIF.*$',                                                                            '\g<tab>#ENDIF',                                                        pline)
            pline = re.sub(r'^(?P<tab>\t*)ENDDO.*$',                                                                            '\g<tab>#ENDDO',                                                        pline)
            pline = re.sub(r'\s*(?P<variableName>\w+)\s*\.EQ\.\s*(?P<variableValue>\-*\d+\.\d+)\s*',                            ' isClose(\g<variableName>, \g<variableValue>) ',                       pline)
            pline = re.sub(r'\s*(?P<variableName>\w+)\s*\.EQ\.\s*(?P<variableValue>[\-\+\d]+(?!\.))\s*',                        ' \g<variableName> == \g<variableValue> ',                              pline)
            pline = re.sub(r'\s*(?P<variableName>\w+)\s*\.NE\.\s*(?P<variableValue>[\-\+\d]+\.\d+)\s*',                          ' not isClose(\g<variableName>, \g<variableValue>) ',                   pline)

            pline = re.sub(r'\.FALSE\.',    'False',    pline)
            pline = re.sub(r'\.TRUE\.',     'True',     pline)
            pline = re.sub(r'\.GE\.',       '>=',       pline)
            pline = re.sub(r'\.GT\.',       '>',        pline)
            pline = re.sub(r'\.LE\.',       '<=',       pline)
            pline = re.sub(r'\.LT\.',       '<',        pline)
            pline = re.sub(r'\.NE\.',       '!=',       pline)
            pline = re.sub(r'\.OR\.',       ' or ',     pline)
            pline = re.sub(r'\.AND\.',      ' and ',    pline)

            pline = re.sub(r'^(?P<tab>\t*)WRITE\s*\(\s*\*\s*,\s*\*\s*\)(?P<printVariables>.*)$', '\g<tab>print \g<printVariables>', pline)
            pline = re.sub(r'^(?P<tab>\t*)DO\s+\d+\s+(?P<variableName>\w+)\s*=\s*(?P<firstParameter>[\-\+\d\w]+)\s*,\s*(?P<secondParameter>[\-\+\d\w]+).*$', '\g<tab>for \g<variableName> in range( \g<firstParameter>, \g<secondParameter> ):', pline)
            pline = re.sub(r'^(?P<tab>\t*)DO\s+\d+\s+(?P<variableName>\w+)\s*=\s*(?P<firstParameter>[\-\+\d\w]+)\s*,\s*(?P<secondParameter>[\-\+\d\w]+)\s*,\s*(?P<thirdParameter>[-+\d]+).*$', '\g<tab>for \g<variableName> in range( \g<firstParameter>, \g<secondParameter>, \g<thirdParameter> ):', pline)
            pline = re.sub(r'^(?P<tab>\t*)(?P<lineLabel>\d+)\s+(?P<formatLine>CONTINUE.*)$', '\g<tab>#\g<lineLabel> CONTINUE', pline)

            pline = re.sub(r'^(?P<tab>\t*)STOP',        '\g<tab>raise SystemExit',  pline)
            pline = re.sub(r'^(?P<tab>\t*)RETURN',      '\g<tab>return',            pline)
            pline = re.sub(r'^(?P<tab>\t*)END',         '\g<tab>#END',              pline)
            # pline = re.sub(r'^\t*REAL', '#REAL', pline)
            # pline = re.sub(r'WRITE\(\*,\*\)', 'print', pline)
            # pline = re.sub(r'^WRITE\(\*,\*\)$', 'print \'\'', pline)

            pythonLines.append(pline)

    writeListToFile(pythonLines, formattedFilePath)

def extractFormattingLines(filePaths, outputResultFile):

    writeRegex      = re.compile(r'^\s*WRITE\s*\(\s*.*\s*,\s*(?P<formatLineLabel>\d+)\s*\).*$')
    formatRegex     = re.compile(r'^\s*(?P<lineLabel>\d+)\s+(?P<formatLine>FORMAT.*)$')

    for filePath in filePaths:

        fileData    = []
        lineLabels  = {}

        with open(filePath, 'r') as fr:
            for lineNumber, lineContent in enumerate(fr):
                writeMatch  = writeRegex.match(lineContent)
                formatMatch = formatRegex.match(lineContent)
                if writeMatch:
                    lineLabel = writeMatch.group('formatLineLabel')
                    if not (lineLabel in lineLabels):
                        lineLabels[lineLabel] = {'usages':[], 'format':''}
                    lineLabels[lineLabel]['usages'].append((lineNumber, lineContent))
                elif formatMatch:
                    lineLabel = formatMatch.group('lineLabel')
                    formatLine = formatMatch.group('formatLine')
                    if not (lineLabel in lineLabels):
                        lineLabels[lineLabel] = {'usages':[], 'format':''}
                    lineLabels[lineLabel]['format'] = (lineNumber, formatLine)

        with open(outputResultFile, 'a') as fw:
            fw.write('\n')
            fw.write(filePath + '\n')
            fw.write('\n')
            for key in lineLabels:
                fw.write(key + '\n')
                fw.write(str(lineLabels[key]['format']) + '\n')
                for usage in lineLabels[key]['usages']:
                    fw.write('\t' + str(usage) + '\n')

if __name__ == '__main__':
    sourceFilePath      = 'qprop_source/src/motor.f'
    sourceFilePath      = 'qprop_source/src/spline.f'
    sourceFilePath      = 'qprop_source/src/qmil.f'


    sourceFilePath      = 'qprop_source/src/bnsolv.f'
    sourceFilePath      = 'qprop_source/src/qprop.f'
    tempFilePath        = 'qprop_parsed/parsedFile1.f'
    tempFilePath2       = 'qprop_parsed/parsedFile2.py'
    formattedFilePath   = 'qprop_parsed/finalParsedFile.py'



    collapseMultiLines(sourceFilePath, tempFilePath)
    # extractFormattingLines([tempFilePath], 'qprop_parsed/formattingLines.txt')
    addTabs(tempFilePath, tempFilePath2)
    replaceCommands(tempFilePath2, formattedFilePath)
