from pydelicious import get_popular, get_userposts, get_urlposts
from recommendations import *
import random
def initializeUserDict(tag, count=5):
    user_dict = {}
    #get the top "count" popular posts
    for p1 in get_popular(tag=tag)[0:count]:
        #find all users who posted this
        for p2 in get_urlposts(p1['url']):
            user = p2['user']
            user_dict[user] = {}
    return user_dict

def fillItems(user_dict):
    all_items = {}
    #find links posted by all users
    for user in user_dict:
        for i in range(3):
            try:
                posts = get_userposts(user)
                break
            except:
                print('Failed user {}, retrying'.format(user))
                time.sleep(4)
        for post in posts:
            url = post['url']
            user_dict[user][url] = 1.0
            all_items[url] = 1
    for ratings in user_dict.values():
        for item in all_items:
            if item not in ratings:
                ratings[item] =  0.0                  

def findOnes(user_dict):
    urls = set()
    for user in user_dict.keys():
        counter = 0
        for url in user_dict[user].keys():
            if user_dict[user][url] == 1.0:
                counter += 1
                urls.add(url)
        print('{}: {}'.format(user, counter))
    print urls
        
if __name__ == '__main__':
    from critics import critics
    print(calculateSimilarItems(critics))
    # delusers = initializeUserDict('programming')
    # fillItems(delusers)
    # print(randUser)
    # print(delusers.keys())
    # print(topMatches(delusers, randUser))
    # print(getRecommendations(delusers, randUser))[0:10]
    
