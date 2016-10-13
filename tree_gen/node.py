#!/usr/bin/env python

class Node:
    def __init__(self, id):
        self.__id = id
        self.__children = []

    def identifier(self):
        return self.__id
    
    def children(self):
        return self.__children

    def add_child(self, obj):
        self.__children.append(obj)
