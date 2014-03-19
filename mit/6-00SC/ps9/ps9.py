# 6.00 Problem Set 9
#
# Intelligent Course Advisor
#
# Name: Jonathan Poler
# Collaborators: None
# Time:
#

from operator import itemgetter

SUBJECT_FILENAME = "subjects.txt"
SHORT_SUBJECT_FILENAME = "shortened_subjects.txt"
VALUE, WORK = 0, 1

#
# Problem 1: Building A Subject Dictionary
#
def loadSubjects(filename):
    """
    Returns a dictionary mapping subject name to (value, work), where the name
    is a string and the value and work are integers. The subject information is
    read from the file named by the string filename. Each line of the file
    contains a string of the form "name,value,work".

    returns: dictionary mapping subject name to (value, work)
    """

    result = {}
    for line in open(filename):
        name, value, work = line.split(',')
        result[name] = (int(value), int(work))
    return result
        

def printSubjects(subjects):
    """
    Prints a string containing name, value, and work of each subject in
    the dictionary of subjects and total value and work of all subjects
    """
    totalVal, totalWork = 0,0
    if len(subjects) == 0:
        return 'Empty SubjectList'
    res = 'Course\tValue\tWork\n======\t====\t=====\n'
    subNames = subjects.keys()
    subNames.sort()
    for s in subNames:
        val = subjects[s][VALUE]
        work = subjects[s][WORK]
        res = res + s + '\t' + str(val) + '\t' + str(work) + '\n'
        totalVal += val
        totalWork += work
    res = res + '\nTotal Value:\t' + str(totalVal) +'\n'
    res = res + 'Total Work:\t' + str(totalWork) + '\n'
    print res

#
# Problem 2: Subject Selection By Greedy Optimization
#

def cmpValue(subInfo1, subInfo2):
    """
    Returns True if value in (value, work) tuple subInfo1 is GREATER than
    value in (value, work) tuple in subInfo2
    """
    return bool(subInfo1[0] > subInfo2[0])

def cmpWork(subInfo1, subInfo2):
    """
    Returns True if work in (value, work) tuple subInfo1 is LESS than than work
    in (value, work) tuple in subInfo2
    """
    return bool(subInfo1[1] < subInfo2[1])

def cmpRatio(subInfo1, subInfo2):
    """
    Returns True if value/work in (value, work) tuple subInfo1 is 
    GREATER than value/work in (value, work) tuple in subInfo2
    """
    return bool((subInfo1[0]/float(subInfo1[1])) > (subInfo2[0]/float(subInfo2[1])))

def greedyAdvisor(subjects, maxWork, comparator):
    """
    Returns a dictionary mapping subject name to (value, work) which includes
    subjects selected by the algorithm, such that the total work of subjects in
    the dictionary is not greater than maxWork.  The subjects are chosen using
    a greedy algorithm.  The subjects dictionary should not be mutated.

    subjects: dictionary mapping subject name to (value, work)
    maxWork: int >= 0
    comparator: function taking two tuples and returning a bool
    returns: dictionary mapping subject name to (value, work)
    """
    assert type(subjects) == dict, "subjects parameter should be a tuple"
    assert (type(maxWork) == int) and (maxWork >= 0), "maxWork needs to be an integer greater than or equal to 0"

    def findTruest(l):
        truest = 0
        for i in range(len(l)):
            if comparator(l[i][1], l[truest][1]):
                truest = i
        return truest

    def sortItems(items):
        i = len(items)
        result = []
        while i > 0:
            truestIndex = findTruest(items)
            result.append(items.pop(truestIndex))
            i -= 1
        return result
                                  
    sortedItems = sortItems(subjects.items())
    
    totalWork = 0
    result = {}
    for key, values in sortedItems:
        if (totalWork + values[1] <= maxWork):
            result[key] = values
            totalWork += values[1]
    return result
#
# Problem 3: Subject Selection By Brute Force
#
def bruteForceAdvisor(subjects, maxWork):
    """
    Returns a dictionary mapping subject name to (value, work), which
    represents the globally optimal selection of subjects using a brute force
    algorithm.

    subjects: dictionary mapping subject name to (value, work)
    maxWork: int >= 0
    returns: dictionary mapping subject name to (value, work)
    """
    def intToByte(i):
        """
        This one was for fun.
        """
        assert (type(i) == int) and ((i <= 256) and (i >= 0))
        binaries = [128,64,32,16,8,4,2,1]
        result = [0]*8
        while i > 0:
            for j in range(8):
                if (i >= binaries[j]):
                    i = i - binaries[j]
                    result[j] = 1
                print i
        return result
    
    def iToB(i, n):
        """
        Faster version of integer to byte conversion.
        """
        assert (type(i) == int) and ((i <= 256) and (i >= 0))
        result = ""
        while (i > 0):
            result = str(i % 2) + result
            i = i/2
        if len(result) != 8:
            diff = n - len(result)
            result = "0"*diff + result
        return result

    def createPermutations(subjects):
        items = subjects.items()
        perms = []
        for i in range(2**len(items)):
            permDict = {}
            permTemplate = iToB(i, len(items))
            counter = 0
            for key, values in items:
                if permTemplate[counter] == "1":
                    permDict[key] = values
                counter += 1
            perms.append(permDict)
        return perms

    def findMax(perms):
        maxPerm = {
            'value': 0,
            'permDict': None
            }
        for permDict in perms:
            totalWork = 0
            totalValue = 0
            for value in permDict.values():

                if totalWork + value[1] > maxWork:
                    break
                totalWork += value[1]
                totalValue += value[0]
            if totalValue > maxPerm['value']:
                maxPerm['value'] = totalValue
                maxPerm['permDict'] = permDict
        return maxPerm['permDict']

    return findMax(createPermutations(subjects))
    
