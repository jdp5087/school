import numpy as np
from math import sqrt

def sim_distance(prefs,person1,person2):
    # Get the list of shared_items
    si={}
    for item in prefs[person1]:
        if item in prefs[person2]:
            si[item]=1
    if len(si) == 0:
        return 0
    sum_of_squares = sum([pow(prefs[person1][item]-prefs[person2][item], 2)
                          for item in si])

    return 1/(1+sqrt(sum_of_squares))

def sim_pearson(prefs,p1,p2):
    si = {}
    for item in prefs[p1]:
        if item in prefs[p2]:
            si[item] = 1
            
    n = len(si)

    # If n is 0, return 0
    if n ==0: return 0

    sumX = sum([prefs[p1][it] for it in si])
    sumY = sum([prefs[p2][it] for it in si])
    sumXSquared = sum([pow(prefs[p1][it], 2) for it in si])
    sumYSquared = sum([pow(prefs[p2][it], 2) for it in si])
    sumXY = sum([prefs[p1][it]*prefs[p2][it] for it in si])

    num = sumXY - (sumX*sumY/n)
    den = sqrt((sumXSquared - pow(sumX,2)/n)*(sumYSquared - pow(sumY,2)/n))

    if den == 0: return 0

    return num/den

def calc_distances(prefs, func):
    rows = []
    people = prefs.keys()
    print people
    for person1 in people:
        row = []
        for person2 in people:
            row.append(func(prefs,person1,person2))
        rows.append(row)
    return np.array(rows)

def topMatches(prefs,person,n=5,similarity=sim_pearson):
    scores = [similarity(prefs,person,other)
              for other in prefs if other!=person]
    
    scores.sort()
    scores.reverse()
    return scores[0:n]
    
                         


# A DICTIONARY OF MOVIE CRITICS AND THEIR RATINGS OF A SMALL SET OF MOVIES
critics = {
    'Lisa Rose': {
        'Lady in the Water': 2.5,
        'Snakes on a Plane': 3.5,
        'Just My Luck': 3.0,
        'Superman Returns': 3.5,
        'You, Me and Dupree': 2.5,
        'The Night Listener': 3.0
        },
    'Gene Seymour': {
        'Lady in the Water': 3.0,
        'Snakes on a Plane': 3.5,
        'Just My Luck': 1.5,
        'Superman Returns': 3.5,
        'The Night Listener': 3.0,
        'You, Me and Dupree': 3.5,
        },
    'Michael Phillips': {
        'Lady in the Water': 2.5,
        'Snakes on a Plane': 3.0,
        'Superman Returns': 3.5,
        'The Night Listener': 4.0,
        },
    'Claudia Puig': {
        'Snakes on a Plane': 3.5,
        'Just My Luck': 3.0,
        'Superman Returns': 4.0,
        'The Night Listener': 4.5,
        'You, Me and Dupree': 2.5,
        },
    'Mick LaSalle': {
        'Lady in the Water': 3.0,
        'Snakes on a Plane': 4.0,
        'Just My Luck': 2.0,
        'Superman Returns': 3.0,
        'You, Me and Dupree': 2.0,
        'The Night Listener': 3.0
        },
    'Jack Matthews': {
        'Lady in the Water': 3.0,
        'Snakes on a Plane': 4.0,
        'Just My Luck': 2.0,
        'Superman Returns': 5.0,
        'You, Me and Dupree': 3.5,
        'The Night Listener': 3.0
        },
    'Toby': {
        'Snakes on a Plane': 4.5,
        'Superman Returns': 4.0,
        'You, Me and Dupree': 1.0,
        },
    }

if __name__ == '__main__':
    print topMatches(critics, 'Toby')
