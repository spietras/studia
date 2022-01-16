# import nltk
# nltk.download('punkt')
# nltk.download("stopwords")
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
from nltk.collocations import BigramAssocMeasures, BigramCollocationFinder
import networkx as nx
import matplotlib.pyplot as plt
import numpy as np


text1 = "I do not like green eggs and ham, I do not like them Sam I am!"
text2 = "Sir, I protest. I am not a merry man!"

stop_words = stopwords.words("english")  # 179 words

# Read from file
#f = open('second_variety.txt')
#raw_text = f.read()


# Prepare data and draw first graph
tokens = word_tokenize(text1)
bigram_measures = BigramAssocMeasures()
finder = BigramCollocationFinder.from_words(tokens)
#finder.apply_freq_filter(5)
#finder.apply_word_filter(lambda w: len(w) < 3 or w.casefold() in stop_words)
scored = finder.score_ngrams(bigram_measures.raw_freq)  # now simply bigram's freq is used as edge's weight
print('Bigrams with weights (corpus 1): ',scored)
nodes = [(node[0][0], node[0][1], {'weight':node[1]}) for node in scored]
G1 = nx.Graph()
G1.add_edges_from(nodes)
plt.figure()
plt.title('Graph 1')
nx.draw(G1, with_labels=True, font_weight='bold')
#pos = nx.spring_layout(G)
#nx.draw_networkx_edge_labels(G,pos)


# Prepare data and draw second graph
tokens2 = word_tokenize(text2)
bigram_measures2 = BigramAssocMeasures()
finder2 = BigramCollocationFinder.from_words(tokens2)
scored2 = finder2.score_ngrams(bigram_measures2.raw_freq)  # now simply bigram's freq is used as edge's weight
print('Bigrams with weights (corpus 2): ', scored2)
nodes2 = [(node[0][0], node[0][1], {'weight': node[1]}) for node in scored2]
G2 = nx.Graph()
G2.add_edges_from(nodes2)
plt.figure()
plt.title('Graph 2')
nx.draw(G2, with_labels=True, font_weight='bold')


# Create copies of graphs with no edges
H1 = nx.create_empty_copy(G1)
H2 = nx.create_empty_copy(G2)
H1_nodes = list(H1.nodes)
H2_nodes = list(H2.nodes)
# Remove vertices that are in both graphs
H2.remove_nodes_from(H1_nodes)
H1.remove_nodes_from(H2_nodes)
#plt.figure()
#nx.draw(H1, with_labels=True, font_weight='bold')
#plt.figure()
#nx.draw(H2, with_labels=True, font_weight='bold')


# Add missing nodes to each graph as isolated vertices (now each graph has the same set of nodes)
G11 = nx.union(G1,H2)
G22 = nx.union(G2,H1)
plt.figure()
plt.title('Graph 1 (common set of nodes)')
nx.draw(G11, with_labels=True, font_weight='bold')
plt.figure()
plt.title('Graph 2 (common set of nodes)')
nx.draw(G22, with_labels=True, font_weight='bold')
assert G11.number_of_nodes()==G22.number_of_nodes()


# Create and sort list of all words in graphs
word_list = list(G11.nodes())
word_list.sort()
print("Common word list: ", word_list)
word_list2 = list(G22.nodes())
word_list2.sort()
assert word_list==word_list2


# Adjacency matrices with weights
A1 = nx.convert_matrix.to_numpy_matrix(G11, nodelist=word_list)
A2 = nx.convert_matrix.to_numpy_matrix(G22, nodelist=word_list)


# Calculate cosine similarity
cos_sim = {}  # key - word; val - cosine similarity
for i in range(A1.shape[0]):
    norm1 = np.linalg.norm(A1[i,:])
    norm2 = np.linalg.norm(A2[:,i])
    if norm1 == 0 or norm2 == 0:
        cos_sim[word_list[i]]=0.0
    else:
        cos_sim[word_list[i]] = float( np.dot(A1[i,:], A2[:,i]) / (norm1*norm2) )
# Sort dict by values
cos_sim_sorted = sorted(cos_sim.items(), key=lambda a:a[1], reverse=True)
print("Cos similarity dict: ", cos_sim_sorted)

plt.show()
