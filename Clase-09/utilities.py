import networkx as nx
import matplotlib.pyplot as plt
from nltk.featstruct import FeatDict

def plot_graph(graph):
    # define la posición de los nodos
    pos = nx.spring_layout(graph)
    # recupera la información de los atributos (que irá en las aristas)
    edges_attrs = nx.get_edge_attributes(graph, 'attribute')
    # recupera la información de los valores (que irá en las nodos)
    nodes_attrs = nx.get_node_attributes(graph, 'value')
    # dibuja el grafo
    g = nx.draw(graph, pos=pos, alpha=.5)
    # agrega los nombres de los atributos (edges)
    g = nx.draw_networkx_edge_labels(graph, pos, edge_labels=edges_attrs)
    # agrega los nombres de los valores (nodes)
    g = nx.draw_networkx_labels(graph, pos, labels=nodes_attrs)
    plt.show()
    
def build_nodes(graph, fs, source_node, total_prev_nodes):
    for node_id, attr in enumerate(fs.keys(), start=total_prev_nodes):
        if not isinstance(fs[attr],FeatDict):
            graph.add_node(node_id, value=fs[attr])
            graph.add_edge(source_node, node_id, attribute=attr)
        else:
            graph.add_node(node_id)
            graph.add_edge(source_node, node_id, attribute=attr)
            sub_fs = fs[attr]
            sub_prev_nodes = total_prev_nodes if total_prev_nodes > graph.number_of_nodes() else graph.number_of_nodes()+1
            build_nodes(graph, sub_fs, source_node=node_id, total_prev_nodes=sub_prev_nodes)
            
            
def make_graph(fs):
    graph = nx.Graph()
    source = 0
    graph.add_node(source)
    build_nodes(graph, fs, source, total_prev_nodes=1)
    plot_graph(graph)