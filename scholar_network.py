import time
from collections import namedtuple
from bs4 import BeautifulSoup
import peewee
import networkx as nx
import requests

SITE = "scholar.google"
USER_AGENT = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:70.0) Gecko/20100101 Firefox/70.0"
SLEEP_INTERVAL = 0.06

headers = {"User-Agent": USER_AGENT}

# una funcion que determine si un articulo dado esta disponible para descarga

# Scrapping

def read_query_from_json():
    pass

def make_query_dict_interactively():
    '''Fill a query dictionary asking questions to the user.'''
    query_dict = make_query_dict()
    query_dict["all"] = []
    query_dict["exact"] = []
    query_dict["none"] = []
    query_dict["language"] = []
    query_dict["region"] = []
    query_dict["date"] = []
    query_dict["site"] = []
    return query_dict

def make_query_dict():
    '''Return a dictionary with keys for all advanced search fields.'''
    query_dict = {}
    query_dict["all"] = []
    query_dict["any"] = []
    query_dict["exact"] = []
    query_dict["none"] = []
    query_dict["language"] = []
    query_dict["region"] = []
    query_dict["date"] = []
    return query_dict

def query_from_title(title_string):
    '''Input the title of an article and return a query dict with the title
    words as query words.'''
    title_query = make_query_dict()
    title_query["all"] = title_string.split(" ")
    return title_query

def get_query_url(query_dict):
    '''Return a search URL using the words at each field in the query dictionary.'''
    query = "https://scholar.google.com/scholar?"
    if query_dict["all"]:
        query = query + "as_q=" + "+".join(query_dict["all"]) + "&"
    if query_dict["exact"]:
        query = query + "as_epq=" + "+".join(query_dict["exact"]) + "&"
    if query_dict["any"]:
        query = query + "as_oq=" + "+OR+".join(query_dict["any"]) + "&"
    if query_dict["none"]:
        query = query + "as_eq=" + "+".join(query_dict["none"]) + "&"
    return query

def get_soup_from_url(query_url):
    '''Given a query URL, return the contents of the BeautifulSoup object
    for that URL.'''
    results = requests.get(query_url, headers=headers)
    soup = BeautifulSoup(results.content, 'html.parser')
    return soup

def site_in_url(url):
    '''Check if the SITE string is part of the first 40 characters of an url.'''
    return SITE in url["href"][0:40]

def return_true(url):
    '''Return True for any argument.'''
    return True

def get_div_by_class(soup, identifier_string):
    '''Given a BeautifulSoup object, return all html elements with the requested
    identifier.'''
    values = soup.find_all("div", class_=identifier_string)
    return values

def get_links(results_soup, acceptance_condition):
    urls = results_soup.find_all('a', href=True)
    urls = [url["href"] for url in urls if acceptance_condition(url)]
    return urls

def get_article_divs(results_soup):
    '''Return all divs for articles.'''
    article_divs = get_div_by_class(results_soup, "gs_r gs_or gs_scl")
    return article_divs

def get_author_divs(results_soup):
    pass

def get_next_page(results_soup):
    pass

def get_author(article_div):
    pass

def get_abstract(article_div):
    pass

def get_references(article_div):
    pass

def get_citations_count(article_div):
    pass

def get_citations_link(article_div):
    '''Given an article div, return an url linking to the citations page.'''
    citations_div = article_div.find_all("a", href=True)
    citations_links = [i["href"] for i in citations_div if "scholar?cites" in i["href"][0:30]]
    if citations_links:
        citations_link = "http://scholar.google.com" + citations_links[0]
    else:
        citations_link = ""
    return citations_link

def get_citations(article_div):
    '''Given an article div, get the divs of citator articles.'''
    citation_link = get_citations_link(article_div)
    citations_soup = get_soup_from_url(citation_link)
    citations = get_article_divs(citations_soup)
    return citations

def get_bibtex(article_div):
    pass

def query_from_article(article_name):
    pass

# Printing

def get_title_text(article_div):
    title = article_div.find("h3", class_="gs_rt")
    title = title.get_text()
    return title

# Text

# Graph

article = namedtuple("Article", ["title", "author", "year", "keywords", "citations"])

class GraphManager:
    def __init__(self):
        self.citations_graph = nx.Graph()

    def populate(self, query_dict, depth):
        current_depth = depth
        search_url = get_query_url(query_dict)
        first_soup = get_soup_from_url(search_url)
        articles = get_article_divs(first_soup)
        for current_article in articles:
            article_title = get_title_text(current_article)
            citations_link = get_citations_link(current_article)
            if citations_link != "":
                citations = get_soup_from_url(citations_link)
                citing_articles = get_article_divs(citations)
                for citation in citing_articles:
                    citation_title = get_title_text(citation)
                    self.citations_graph.add_edge(article_title, citation_title)
                    time.sleep(SLEEP_INTERVAL)
                    if current_depth > 1:
                        self.populate(query_from_title(citation_title) , current_depth-1)
            time.sleep(SLEEP_INTERVAL)

    def rank(self):
        ranking = nx.pagerank(self.citations_graph)
        ranking = [(key, ranking[key]) for key in sorted(ranking, key=ranking.get, reverse=True)]
        return ranking


def make_edge(article_1, article_2):
    pass

def edges_to_graph(edges):
    pass

# Graph operations

def page_rank(scholar_network):
    pass

# Visualization


# Test

sample_query_dict = make_query_dict()
sample_query_dict_2 = make_query_dict()
sample_query_dict_3 = make_query_dict()
sample_query_dict_4 = make_query_dict()
sample_query_dict["all"] = ["attention", "transformers", "neural"]
sample_query_dict_2["all"] = ["video", "understanding", "deep", "learning"]
sample_query_dict_3["all"] = ["causal", "inference", "graphical", "models"]
sample_query_dict_4["all"] = ["regular", "graph", "grammars"]


my_url = get_query_url(sample_query_dict_4)
my_soup = get_soup_from_url(my_url)
my_articles = get_article_divs(my_soup)
for current_article in my_articles:
    print(get_title_text(current_article))
    print(get_citations_link(current_article))
    print("CITATIONS:")
    for current_article in my_articles:
        article_title = get_title_text(current_article)
        citations_link = get_citations_link(current_article)
        if citations_link != "":
            citations = get_soup_from_url(citations_link)
            citing_articles = get_article_divs(citations)
            for citation in citing_articles:
                citation_title = get_title_text(citation)
                print((article_title, citation_title))
                time.sleep(SLEEP_INTERVAL)
        time.sleep(SLEEP_INTERVAL)


# whole item: gs_r.gs_or.gs_scl
# title: gs_rt
# author: gs_a
# citations, bibtex, etc: gs_fl
