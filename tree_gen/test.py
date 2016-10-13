#!/usr/bin/python

class Node(object):
    
    def __init__(self, id):
        self.id = id
        self.children = []

    def get_id(self):
        return self.id

    def get_children(self):
        return self.children

    def add_child(self, node):
        self.children.append(node)

    def children_num(self):
        return len(self.children)
    
class Tree:

    def __init__(self, root):
        self.nodes = {root.get_id():root}
        self.root = root

    def get_nodes(self):
        return self.nodes

    def get_root(self):
        return self.root

    def add_node(self, id, parent):
        node = Node(id)
        self.nodes[id] = node
        self.nodes[parent].add_child(node)
        return node

def to_newick(root):
    output = ""
    if root.children_num() == 2:
        output += "("
        output += to_newick(root.children[0])
        output += ","
        output += to_newick(root.children[1])
        output += ")" + root.get_id()
            
    elif root.children_num() == 1:
        output += "("
        output += to_newick(root.children[0])
        output += ")" + root.get_id()
    else:
        return root.get_id()

    return output
            
if __name__ == "__main__":

    tree = Tree(Node("A"))
    tree.add_node("B","A")
    tree.add_node("C","A")
    tree.add_node("D","B")
    tree.add_node("E","B")
    tree.add_node("F","C")
    tree.add_node("G","C")

    print(to_newick(tree.get_root()) + ';')

    with open("test.tre", "w+") as o:
        o.write(to_newick(tree.get_root())+";")
