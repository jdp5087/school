from collections import defaultdict
import json
import codecs

class TweetSentiment:

    def __init__(self, tweetData, sentimentData):
        self.tweetData = tweetData
        self.sentimentData = sentimentData
        self.sentiments = {}
        self.scores = []

    def loadSentiments(self, sentimentData):
        with codecs.open(sentimentData, 'rU', 'utf-8') as afinn:
            for row in afinn.readlines():
                key, value = row.strip().split('\t')
                self.sentiments[unicode(key)] = int(value)
        
    def loadTweets(self, tweetData):
        with codecs.open(tweetData, 'rU', 'utf-16') as json_data:
            self.tweets = []
            for line in json_data.readlines():
                self.tweets.append(json.loads(line))
        return self.tweets

    def checkWord(self, w):
        if self.sentiments.get(w) != None:
            return self.sentiments.get(w)
        else:
            return 0

    def sumScore(self, s):
        score = 0
        for word in s.split(' '):
            score += self.checkWord(word)
        return score

    def scoreTweet(self, tweet):
        try:
            if tweet.get('lang') == u'en':
                self.scores.append({
                        'text': tweet.get('text'),
                        'score': self.sumScore(tweet.get('text')),
                        })
        except UnicodeEncodeError as e:
            pass

    def run(self):
        self.loadSentiments(self.sentimentData)
        for tweet in self.loadTweets(self.tweetData):
            self.scoreTweet(tweet)
        return self.scores
        
if __name__ == '__main__':
    totals = defaultdict(lambda: 0)
    ts = TweetSentiment('output.txt', 'AFINN-111.txt')
    for tweet in ts.run():
        totals[tweet['score']] += 1
    print totals
    
    



         
