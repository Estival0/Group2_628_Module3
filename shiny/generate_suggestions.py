#!/usr/bin/env python
# coding: utf-8

#generate dictionary with pickle
from collections import defaultdict
import pickle
import pandas as pd
import numpy as np
import sys

# In[148]:
# shop = sys.argv[1]
# city = sys.argv[2]
# state = sys.argv[3]
# period = sys.argv[4]

def words(reviews):
  # read from topic table
    topic = pd.read_csv('/Users/maritmcquaig/Documents/GitHub/Group2_628_Module3/shiny/topics.csv')
    sorted_reviews = reviews.sort_values(by=['compound'], ascending=False)
    total_size = len(sorted_reviews)
    good_reviews = sorted_reviews.iloc[:total_size // 3, :]
    bad_reviews = sorted_reviews.iloc[2 * total_size // 3 :, :]
    good_reviews_frequency = good_reviews.text.str.split(expand=True).stack().value_counts().reset_index()
    good_reviews_frequency.columns = ['Word', 'Frequency']
    bad_reviews_frequency = bad_reviews.text.str.split(expand=True).stack().value_counts().reset_index()
    bad_reviews_frequency.columns = ['Word', 'Frequency']
    # join topic table
    good_intersection = pd.merge(good_reviews_frequency, topic, on='Word')
    good_intersection = good_intersection.sort_values(by=['Frequency'], ascending=False)
    bad_intersection = pd.merge(bad_reviews_frequency, topic, on='Word')
    bad_intersection = bad_intersection.sort_values(by=['Frequency'], ascending=False)
    good_list = set()
    bad_list = set()
    for i in range(len(good_intersection)):
        word = good_intersection.iloc[i, 0]
        if word not in bad_intersection['Word'].to_list() or good_intersection.iloc[i, 1] >= bad_intersection.loc[bad_intersection['Word'] == word, 'Frequency'].values[0]:
            good_list.add(word)
            if len(good_list) > 2:
                break
    for i in range(len(bad_intersection)):
        word = bad_intersection.iloc[i, 0]
        if word not in good_intersection['Word'].to_list() or bad_intersection.iloc[i, 1] > good_intersection.loc[good_intersection['Word'] == word, 'Frequency'].values[0]:
            bad_list.add(word)
            if len(bad_list) > 2:
                break
    return good_list, bad_list


# In[149]:


def generate_suggestions(good_list, bad_list):
    with open('/Users/maritmcquaig/Documents/GitHub/Group2_628_Module3/shiny/suggestion.pickle', 'rb') as handle:
        suggestion_dict = pickle.load(handle)
    good_suggestion = []
    bad_suggestion = []
    for good in good_list:
        good_suggestion.append(suggestion_dict[good][0][0])
    for bad in bad_list:
        bad_suggestion.append(suggestion_dict[bad][0][1])
    return good_suggestion, bad_suggestion


# In[150]:


def filters(shop, city, state, period):
    if period == "pre covid":
        reviews = pd.read_csv('/Users/maritmcquaig/Documents/GitHub/Group2_628_Module3/shiny/pre_covid_combined.csv')
    else:
        reviews = pd.read_csv('/Users/maritmcquaig/Documents/GitHub/Group2_628_Module3/shiny/after_covid_combined.csv')
    reviews = reviews[reviews['name'] == shop]
    reviews = reviews[reviews['city'] == city]
    reviews = reviews[reviews['state'] == state]
    return reviews


# In[151]:


def main_func(shop, city, state, period):
    reviews = filters(shop, city, state, period)
    good_list, bad_list = words(reviews)
    good_suggestion, bad_suggestion = generate_suggestions(good_list, bad_list)
    return good_suggestion, bad_suggestion

# good_suggestion, bad_suggestion = main_func(shop, city, state, period)
