import networkx as nx
import numpy as np
import pandas as pd
from itertools import groupby
from typing import List


# == Global variable == #
# Keep the k most frequent graphs of n nodes
nb_top_k_graph = 3
# ======================#


def extract_daily_pattern(l: List[str]) -> nx.DiGraph:
    g = nx.DiGraph()
    for i in range(len(l)-1):
        g.add_edge(l[i], l[i+1])
    return g


def filter_nodes(k: int, g: nx.DiGraph) -> bool:
    return True if len(list(g.nodes())) == k else False


def daily_pattern_freq(path: str) -> List[int]:

    path_daily_pat = "./Daily_pattern/"

    df_mk = pd.read_csv(path, sep=";", header=0, keep_default_na=False)
    max_seq = max(df_mk["Id_seq"]) + 1
    seq_mk = [[x for x in df_mk[df_mk["Id_seq"] == i].iloc[:, 3].values.tolist()]
              for i in range(1, max_seq)]
              
    dict_graph = dict()
    daily_pat_freq = [0] * (1 + 2 + 3 * (nb_top_k_graph + 1) + 1)
    # daily_pat_freq[0] -> nb graphs of 1 node
    # daily_pat_freq[1] -> nb graphs of 2 nodes most freq
    # daily_pat_freq[2] -> nb graphs of 2 nodes | daily_pat_freq[1] > daily_pat_freq[2]
    # daily_pat_freq[3] -> nb graphs of 3 nodes most freq
    # daily_pat_freq[4] -> nb graphs of 3 nodes | daily_pat_freq[3] > daily_pat_freq[4]
    # daily_pat_freq[5] -> nb graphs of 3 nodes | daily_pat_freq[4] > daily_pat_freq[5]
    # daily_pat_freq[6] -> freq of other graphs of 3 nodes
    # ...
    # daily_pat_freq[16] -> nb graphs with more than 5 nodes

    for l in seq_mk:
        l = list(filter(lambda a: a != '', l))
        l = [x[0] for x in groupby(l)]
        if len(l) > 1:
            if len(set(l)) < 6:
                # Extract daily pattern from the sequence l
                graph = extract_daily_pattern(l)
                exist_isomorph = graph
                for g in dict_graph.keys():
                    # Test is graph has already an isomorphism
                    if nx.is_isomorphic(g, graph):
                        exist_isomorph = g
                        break
                # Set up dictionary freq
                dict_graph[exist_isomorph] = dict_graph.get(exist_isomorph, 0) + 1
            else:
                # For graph with more than 5 nodes
                daily_pat_freq[15] += 1
        else:
            # Atomic graph
            daily_pat_freq[0] += 1

    # Sort dictionary by frequence
    sort_dict_graph = sorted(dict_graph.items(), key=lambda x: x[1], reverse=True)
    
    # Filter graph with 2 nodes
    l_graph = list(filter(lambda x: filter_nodes(2, x[0]), sort_dict_graph))
    daily_pat_freq[1] = l_graph[0][1]
    graph_numpy = nx.adjacency_matrix(l_graph[0][0]).todense()
    np.savetxt(path_daily_pat+"graph2-1.csv", graph_numpy, delimiter=";")
    daily_pat_freq[2] = l_graph[1][1]
    graph_numpy = nx.adjacency_matrix(l_graph[1][0]).todense()
    np.savetxt(path_daily_pat+"graph2-2.csv", graph_numpy, delimiter=";")

    for k in range(3, 6):
        # Indice for k nodes graph
        b_inf = 3 + (k - 3) * (nb_top_k_graph + 1)
        # Filter graph with k nodes
        l_graph = list(filter(lambda x: filter_nodes(k, x[0]), sort_dict_graph))
        for i in range(min(nb_top_k_graph, len(l_graph))):
            # Freq of daily pattern
            daily_pat_freq[b_inf + i] = l_graph[i][1]
            # Export in adjacency matrix format
            graph_numpy = nx.adjacency_matrix(l_graph[i][0]).todense()
            np.savetxt(path_daily_pat+"graph"+str(k)+"-"+str(i+1)+".csv", graph_numpy, delimiter=";")
        if len(l_graph) > nb_top_k_graph:
            # For other graphs not in top k
            daily_pat_freq[b_inf + nb_top_k_graph] = np.sum([x[1] for x in l_graph[nb_top_k_graph:]])
    return [x/len(seq_mk) for x in daily_pat_freq]


def main():
    l_seq = [["1", "3", "1", "2", "1"],
             ["1", "2"],
             ["1", "2", "1"],
             ["1", "2", "3", "1"],
             ["1", "2", "3", "1"],
             ["1", "2", "3", "1"],
             ["1", "3", "2", "1"],
             ["1", "2", "1", "3"],
             ["1", "2", "3", "4", "1"],
             ["1", "2", "1", "3", "1", "4", "1"],
             ["1", "3", "1", "3", "1"],
             ["1"],
             ["1", "3", "2", "3", "2", "1", "2"],
             ["1", "2", "3", "2", "4", "5", "1", "6", "1"]]
            
    #print(daily_pattern_freq(l_seq))


if __name__ == '__main__':
    main()
