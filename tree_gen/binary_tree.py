#!/usr/bin/python

class Tree():

    def __init__(self,rootid):
      self.left = None
      self.right = None
      self.rootid = rootid

    def getL(self):
        return self.left
    def getR(self):
        return self.right
    def setVal(self,value):
        self.rootid = value
    def getVal(self):
        return self.rootid

    def insertR(self,newNode):
        if self.right == None:
            self.right = Tree(newNode)
        else:
            tree = BinaryTree(newNode)
            tree.right = self.right
            self.right = tree
        
    def insertL(self,newNode):
        if self.left == None:
            self.left = Tree(newNode)
        else:
            tree = Tree(newNode)
            tree.left = self.left
            self.left = tree
        
