import numpy as np
from math import sqrt

from critics import critics

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
    scores = [(similarity(prefs,person,other), other)
              for other in prefs if other!=person]
    
    scores.sort()
    scores.reverse()
    return scores[0:n]
    
def getRecommendations(prefs, person, similarity=sim_pearson):
    totals = {}
    simSums = {}
    ## Iterate all critics other than person
    for other in prefs:
        if other == person: continue
        sim = similarity(prefs, person, other)
        ## if similarity is less than 0, skip
        if sim <= 0: continue
        for item in prefs[other]:
            ## if person hasn't viewed or if they rated item as 0, calculate recommendations
            if item not in prefs[person] or prefs[person][item] == 0:
                totals.setdefault(item, 0)
                ## add other's rating of item * similarity as a weight
                totals[item] += prefs[other][item] * sim
                simSums.setdefault(item, 0)
                ## add up all sims used in th
                simSums[item] += sim
        ## Normalize by weight of sums that apply to each particular item
        rankings = [(total/simSums[item], item) for item, total in totals.items()]

    rankings.sort()
    rankings.reverse()
    return rankings
            
def transformPrefs(prefs):
    result = {}
    for person in prefs:
        for item in prefs[person]:
            result.setdefault(item, {})
            result[item][person] = prefs[person][item]
    return result

def calculateSimilarItems(prefs,n=10):
    #Create a dictionary of items showing which other items
    #they are most similar to.
    itemPrefs = {}
    #invert prefs
    invertedPrefs = transformPrefs(prefs)

    c=0
    for item in invertedPrefs:
        c += 1
        if c%100 == 0:
            print('{}/{}'.format(c,len(invertedPrefs)))
        for rating in invertedPrefs[item]:
            itemPrefs[item] = topMatches(invertedPrefs,item,n=n,similarity=sim_distance)
    return itemPrefs

def getRecommendedItems(prefs, itemMatch, user):
    userRatings = prefs[user]
    scores = {}
    totalSim = {}

    # Loop over items rated by this user
    for (item, rating) in userRatings.items():
        # Loop over items similar to this one
        for (similarity, item2) in  itemMatch[item]:
            # Ignore if this user has already rated this item
            if item2 in userRatings: continue

            # Weighted sum of ratings times similarity
            scores.setdefault(item2, 0)
            scores[item2] += similarity*rating

            # Sum of all the similarities
            totalSim.setdefault(item2, 0)
            totalSim[item2] += similarity

    # Divide each total score by total weighting to get an average
    rankings = [(score/totalSim[item],item) for item, score in scores.items()]

    rankings.sort()
    rankings.reverse()
    return rankings
            
    


if __name__ == '__main__':
    print getRecommendedItems(
        critics,
        calculateSimilarItems(critics,n=5),
        'Toby',
        )
        
