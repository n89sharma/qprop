import re
import sys

def beginningOfLoopOrConditional(lineContent):
    return  re.match(r'\bPROGRAM\b', lineContent) or\
            re.match(r'\bSUBROUTINE\b', lineContent) or\
            re.match(r'\bDO\b', lineContent) or\
            re.match(r'\bIF\b.*\bTHEN\b', lineContent) or\
            re.match(r'\bFUNCTION\b', lineContent)


def intermediateConditionalStatements(lineContent):
    return  re.match(r'\bELSEIF\b', lineContent) or\
            re.match(r'\bELSE\b',   lineContent)

def terminationOfLoopOrConditional(lineContent):
    return  re.match(r'\bEND\b', lineContent) or\
            re.match(r'\bENDDO\b', lineContent) or\
            re.match(r'\bENDIF\b', lineContent)

def continueLine(lineContent):
    reg = re.compile(r'^\s*(?P<lineLabel>\d+)\s+CONTINUE.*$')
    match = reg.match(lineContent)
    if match:
        return match.group('lineLabel')
    else:
        return False

def doLoopWithLineLabel(lineContent):
    reg = re.compile(r'^\s*DO\s+(?P<lineLabel>\d+).*$')
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
            pline = re.sub(r'^(?P<tab>\t*)[Cc]+(?P<commentValue>(?![a-zA-Z0-9\(\[]).*)$',                                         '\g<tab>#\g<commentValue>',                                                             pline)
            pline = re.sub(r'^(?P<tab>\t*)PROGRAM\s+(?P<programName>\w+).*$',                                                   '\g<tab>def \g<programName> ():',                                                       pline)
            pline = re.sub(r'^(?P<tab>\t*)SUBROUTINE\s+(?P<routineName>\w+)\s*\((?P<routineParameters>.*)\).*$',                '\g<tab>def \g<routineName> (\g<routineParameters>):',                                  pline)
            pline = re.sub(r'^(?P<tab>\t*)FUNCTION\s+(?P<routineName>\w+)\s*\((?P<routineParameters>.*)\).*$',                  '\g<tab>def \g<routineName> (\g<routineParameters>):',                                  pline)
            pline = re.sub(r'^(?P<tab>\t*)DATA\s+(?P<variableName>\w+)\s+/(?P<variableValue>.*)/.*$',                           '\g<tab>\g<variableName> = \g<variableValue>',                                          pline)
            pline = re.sub(r'^(?P<tab>\t*)DO\s+(?P<variableName>\w+)\s*=\s*(?P<fromNum>[\d\w]+)\s*,\s*(?P<toNum>[\d\w]+).*$',   '\g<tab>for \g<variableName> in fortranRangeTwoParam( \g<fromNum>, \g<toNum> ):',       pline)
            pline = re.sub(r'^(?P<tab>\t*)CALL\s+(?P<routineName>\w+)\s*\((?P<routineParameters>.*)\).*$',                      '\g<tab>\g<routineName> (\g<routineParameters>)',                                       pline)
            pline = re.sub(r'^(?P<tab>\t*)IF\s*\((?P<logicalComparison>.*)\)\s*THEN.*$',                                        '\g<tab>if (\g<logicalComparison>):',                                                   pline)
            pline = re.sub(r'^(?P<tab>\t*)ELSEIF\s*\((?P<logicalComparison>.*)\)\s*THEN.*$',                                    '\g<tab>elif (\g<logicalComparison>):',                                                 pline)
            pline = re.sub(r'^(?P<tab>\t*)ELSE IF\s*\((?P<logicalComparison>.*)\)\s*THEN.*$',                                   '\g<tab>elif (\g<logicalComparison>):',                                                 pline)
            pline = re.sub(r'^(?P<tab>\t*)ELSE.*$',                                                                             '\g<tab>else:',                                                                         pline)
            pline = re.sub(r'^(?P<tab>\t*)ENDIF.*$',                                                                            '\g<tab>#ENDIF',                                                                        pline)
            pline = re.sub(r'^(?P<tab>\t*)ENDDO.*$',                                                                            '\g<tab>#ENDDO',                                                                        pline)
            pline = re.sub(r'\s*(?P<variableName>\w+)\s*\.EQ\.\s*(?P<variableValue>\-*\d+\.\d+)\s*',                            ' isClose(\g<variableName>, \g<variableValue>) ',                                       pline)
            pline = re.sub(r'\s*(?P<variableName>\w+)\s*\.EQ\.\s*(?P<variableValue>[\-\+\d]+(?!\.))\s*',                        ' \g<variableName> == \g<variableValue> ',                                              pline)
            pline = re.sub(r'\s*(?P<variableName>\w+)\s*\.EQ\.\s*(?P<variableValue>.*)',                                        ' \g<variableName> == \g<variableValue> ',                                              pline)
            pline = re.sub(r'\s*(?P<variableName>\w+)\s*\.NE\.\s*(?P<variableValue>[\-\+\d]+\.\d+)\s*',                         ' not isClose(\g<variableName>, \g<variableValue>) ',                                   pline)

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
            pline = re.sub(r'^(?P<tab>\t*)DO\s+\d+\s+(?P<variableName>\w+)\s*=\s*(?P<firstParameter>[\-\+\d\w]+)\s*,\s*(?P<secondParameter>[\-\+\d\w]+).*$', '\g<tab>for \g<variableName> in fortranRangeTwoParam( \g<firstParameter>, \g<secondParameter> ):', pline)
            pline = re.sub(r'^(?P<tab>\t*)DO\s+\d+\s+(?P<variableName>\w+)\s*=\s*(?P<firstParameter>[\-\+\d\w]+)\s*,\s*(?P<secondParameter>[\-\+\d\w]+)\s*,\s*(?P<thirdParameter>[-+\d]+).*$', '\g<tab>for \g<variableName> in fortranRangeThreeParam( \g<firstParameter>, \g<secondParameter>, \g<thirdParameter> ):', pline)
            pline = re.sub(r'^(?P<tab>\t*)(?P<lineLabel>\d+)\s+(?P<formatLine>CONTINUE.*)$', '\g<tab>#\g<lineLabel> CONTINUE', pline)

            pline = re.sub(r'^(?P<tab>\t*)STOP',        '\g<tab>raise SystemExit',  pline)
            pline = re.sub(r'^(?P<tab>\t*)RETURN',      '\g<tab>return',            pline)
            pline = re.sub(r'^(?P<tab>\t*)END',         '\g<tab>#END',              pline)

            pline = re.sub(r'^(?P<tab>\t*)IF\s*\((?P<logicalComparison>.*)\)\s*STOP\s*(?P<exitStatement>\'.*\')[^\S\n]*$',  '\g<tab>if (\g<logicalComparison>): raise SystemExit(\g<exitStatement>)',   pline)
            pline = re.sub(r'^(?P<tab>\t*)IF\s*\((?P<logicalComparison>.*)\)\s*STOP[^\S\n]*$',                              '\g<tab>if (\g<logicalComparison>): raise SystemExit',                      pline)
            pline = re.sub(r'^(?P<tab>\t*)IF\s*\((?P<logicalComparison>.*)\)\s*(?P<assignmentStatement>((?!STOP).*=.*))$',  '\g<tab>if (\g<logicalComparison>): \g<assignmentStatement>',               pline)

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

def fortranRangeTwoParam(fromNum, toNum):
    return range(fromNum-1, toNum)

def replaceVariableParenthesis(variableNames, sourceFile, tempFile):
    # variableNames = getVariableListForFile(mainSourceFile)
    varRegExStrings = [ (var, r'\b' + var + r'\((?P<index>((?!\)).)*)\)') for var in variableNames]
    print varRegExStrings
    with open(sourceFile, 'r') as fr:
        sourceData = fr.read()

    formattedData = sourceData
    for var, varRegExString in varRegExStrings:
        formattedData = re.sub(varRegExString, var + '[\g<index> - 1]', formattedData)

    with open(tempFile, 'w') as fw:
        fw.write(formattedData)

def initialValueForVarType(real, dimension, logical, character, integer):
    if(real or dimension):
        return '0.0'
    elif(logical):
        return 'False'
    elif(character):
        return '\'\''
    else:
        return '0'

def replaceVariableDeclerations(sourceFile, tempFile):

    parameterRegEx  = re.compile(r'^\s*PARAMETER\s*\((?P<parameterName>.+)=(?P<parameterValue>.+)\)$')
    realRegEx       = re.compile(r'^\s*REAL(?P<variables>.*)$')
    dimensionRegEx  = re.compile(r'^\s*DIMENSION(?P<variables>.*)$')
    logicalRegEx    = re.compile(r'^\s*LOGICAL(?P<variables>.*)$')
    characterRegEx  = re.compile(r'^\s*CHARACTER\*\d+(?P<variables>.*)$')
    integerRegEx    = re.compile(r'^\s*INTEGER(?P<variables>.*)$')
    varArrayRegEx   = re.compile(r'(?P<varName>.+)\s*\((?P<arrayLength>((?!\)).)+)\)')

    fileVariableList =[]
    formattedLines = []
    with open(sourceFile, 'r') as fr:
        for line in fr:
            formattedLine = line
            real        = realRegEx.match(line)
            dimension   = dimensionRegEx.match(line)
            logical     = logicalRegEx.match(line)
            character   = characterRegEx.match(line)
            integer     = integerRegEx.match(line)
            parameter   = parameterRegEx.match(line)

            if real:
                data = real.group('variables')
            elif dimension:
                data = dimension.group('variables')
            elif logical:
                data = logical.group('variables')
            elif character:
                data = character.group('variables')
            elif integer:
                data = integer.group('variables')
            else:
                data = None

            if data != None:
                initialValue = initialValueForVarType(real, dimension, logical, character, integer)
                isMultiVariableLine = len(re.findall(',', data)) > 0

                if isMultiVariableLine:
                    varList = []
                    varArrayList = []
                    varArrayLengthList = []

                    variables = re.split(',', data)
                    for variable in variables:
                        varArray    = varArrayRegEx.match(variable)
                        if varArray:
                            varArrayList.append(varArray.group('varName'))
                            fileVariableList.append(varArray.group('varName'))
                            varArrayLengthList.append(varArray.group('arrayLength'))
                        else:
                            varList.append(variable)
                            fileVariableList.append(variable)

                    # multiple arrays declared in one line
                    if len(varArrayList) > 0:
                        formattedLine = ''
                        for varName in varArrayList[:-1]:
                            formattedLine += varName + ','
                        formattedLine += varArrayList[-1] + ' = '
                        for arrayLength in varArrayLengthList[:-1]:
                            formattedLine += '[' + str(initialValue) + ']*' + arrayLength + ' ,'
                        formattedLine += '[' + str(initialValue) + ']*' + varArrayLengthList[-1] + '\n'

                    # multiple single valued variables declared in one line
                    else:
                        formattedLine = ''
                        for varName in varList[:-1]:
                            formattedLine += varName + ','
                        formattedLine += varList[-1] + ' = '
                        for varName in varList[:-1]:
                            formattedLine += str(initialValue) + ','
                        formattedLine += str(initialValue) + '\n'

                # Single declaration
                else:
                    varArray    = varArrayRegEx.match(data)

                    #Single array declared
                    if varArray:
                        fileVariableList.append(varArray.group('varName'))
                        formattedLine = varArray.group('varName') + ' = ' + '[' + str(initialValue) + ']*' + varArray.group('arrayLength') + '\n'

                    #Single variable declared
                    else:
                        fileVariableList.append(data)
                        formattedLine = data + ' = ' + initialValue + '\n'

            if parameter:
                varName         = parameter.group('parameterName')
                varValue        = parameter.group('parameterValue')
                formattedLine   = varName + ' = ' + varValue + '\n'

            formattedLines.append(formattedLine)

    with open(tempFile, 'w') as fw:
        for line in formattedLines:
            fw.write(line)

    return [var.strip() for var in fileVariableList]

def replaceFormatLines(sourceFile, tempFile):
    formattingLines = {}
    writeLines = []
    fileLines = {}
    spacingIndicator = re.compile(r'(?P<indicator>(?P<width>\d+)X)')
    with open(sourceFile, 'r') as fr:
        for lineNumber, line in enumerate(fr):
            writeMatch  = re.match(r'^\s*WRITE\s*\(\s*(?P<outputTarget>.+)\s*,\s*(?P<lineLabel>\d+)\s*\)(?P<writeOptions>.*)$', line)
            formatMatch = re.match(r'^\s*(?P<lineLabel>\d+)\s+FORMAT\s*\((?P<formattingOptions>.*)\).*$', line)

            if formatMatch:
                lineLabel                   = formatMatch.group('lineLabel')
                formattingOptions           = formatMatch.group('formattingOptions')

                pythonFormatting    = formattingOptions.replace(' ', '')
                pythonFormatting    = pythonFormatting.replace('/', '\\n')
                pythonFormatting    = re.sub(r'G(?P<width>\d+)\.(?P<decimal>\d+)', '{:\g<width>.\g<decimal>f}', pythonFormatting)
                pythonFormatting    = re.sub(r'I(?P<width>\d+)', '{:\g<width>d}', pythonFormatting)
                pythonFormatting    = re.sub(r'F(?P<width>\d+)\.(?P<decimal>\d+)', '{:\g<width>.\g<decimal>f}', pythonFormatting)
                pythonFormatting    = re.sub(r'A(?P<width>\d+)', '{:>\g<width>}', pythonFormatting)
                pythonFormatting    = re.sub(r'A', '{}', pythonFormatting)
                spacingMatches      = re.findall(spacingIndicator, pythonFormatting)
                if len(spacingMatches) > 0:
                    for declaration, width in spacingMatches:
                        spacing = '\'' + ' '*int(width) + '\''

                        pythonFormatting = re.sub(declaration, spacing, pythonFormatting)

                pythonFormatting            = ''.join(iter(pythonFormatting.split(',')))
                pythonFormatting            = pythonFormatting.replace('\'', '')
                pythonFormatting            = '\'' + pythonFormatting + '\'\n'
                formattingLines[lineLabel]  = pythonFormatting
                fileLines[lineNumber]       = '# ' + line

            elif writeMatch:
                lineLabel               = writeMatch.group('lineLabel')
                writeOptions            = writeMatch.group('writeOptions')
                outputTarget            = writeMatch.group('outputTarget')
                writeLines.append((lineNumber, lineLabel, outputTarget, writeOptions))

                fileLines[lineNumber]   = line

            else:
                fileLines[lineNumber]   = line

    for lineNumber, lineLabel, outputTarget, writeOptions in writeLines:
        if outputTarget == '*':
            fileLines[lineNumber] = 'print ' + formattingLines[lineLabel].strip() + '.format(' + writeOptions + ')\n'
        else:
            fileLines[lineNumber] = outputTarget + '.write(' + formattingLines[lineLabel].strip() + '.format(' + writeOptions + '))\n'

    finalFile = ['']*len(fileLines)
    for lineNumber in fileLines:
        finalFile[lineNumber] = fileLines[lineNumber]

    writeListToFile(finalFile, tempFile)

if __name__ == '__main__':
    # sourceFilePath      = 'qprop_source/src/motor.f'


    sourceFilePath      = 'qprop_source/src/bnsolv.f'
    sourceFilePath      = 'qprop_python/test.f'
    sourceFilePath      = 'qprop_source/src/gvcalc.f'
    sourceFilePath      = 'qprop_source/src/tpdes.f'
    sourceFilePath      = 'qprop_source/src/qmil.f'
    sourceFilePath      = 'qprop_source/src/spline.f'
    sourceFilePath      = 'qprop_source/src/tqcalc.f'
    sourceFilePath      = 'qprop_source/src/qprop.f'

    tempFilePath        = 'qprop_parsed/parsedFile1.f'
    tempFilePath2       = 'qprop_parsed/parsedFile2.py'
    tempFilePath3       = 'qprop_parsed/parsedFile3.py'
    tempFilePath4       = 'qprop_parsed/parsedFile4.py'
    tempFilePath5       = 'qprop_parsed/parsedFile5.py'
    formattedFilePath   = 'qprop_parsed/finalParsedFile.py'



    collapseMultiLines(                             sourceFilePath, tempFilePath)
    #extractFormattingLines([tempFilePath], 'qprop_parsed/formattingLines.txt')
    fileVariableList = replaceVariableDeclerations( tempFilePath,   tempFilePath2)
    replaceFormatLines(                             tempFilePath2,  tempFilePath3)
    addTabs(                                        tempFilePath3,  tempFilePath4)
    replaceVariableParenthesis(fileVariableList,    tempFilePath4,  tempFilePath5)
    replaceCommands(                                tempFilePath5,  formattedFilePath)
