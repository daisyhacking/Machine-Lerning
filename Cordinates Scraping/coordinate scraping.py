# -*- coding: utf-8 -*-
"""
Created on Thu Oct 11 22:14:34 2018

@author: 64254
"""

import pandas as pd
import string
import requests
import re

from bs4 import BeautifulSoup

region_list = pd.read_csv('city_name.csv')

def remove_punctuations(text):
    for punctuation in string.punctuation:
        text = text.replace(punctuation, '')
    return text

region_list['Name'] = region_list['Name'].apply(lambda x: remove_punctuations(x))


def get_corrdinates(city_name):
    url = 'https://www.google.com/search?q=' + str(city_name)+'%20Coordinates&cad=h'
    page = requests.get(url)
    soup = BeautifulSoup(page.text, 'html.parser')
    corrdinates = soup.find_all("div", class_ = "R8KuR")
    return corrdinates

region_list['Corrdinates'] = region_list['Name'].apply(lambda x: get_corrdinates(x))
region_list.to_csv('1.csv')


def search_ll(soup_findall):
    s = str(soup_findall)
    result = re.search('ll=(.*)&amp;z=', s)
    if result !=None:
        return result.group(1)
    else:
        return (',')
region_list['Corrdinates_ll']  = region_list['Corrdinates']
region_list['Corrdinates_ll'] = region_list['Corrdinates'].apply(lambda x: search_ll(x))

region_list['latitude'], region_list['longtitude'] = region_list['Corrdinates_ll'].str.split(',', 1).str

region_list.to_csv('additional_city_coordinates.csv')



'''
Open your web browser automatically

'''

import webbrowser

new = 2

tabUrl = "http://google.com/?#q=";
term = "london cordination";

webbrowser.open(tabUrl + term, new = new);


