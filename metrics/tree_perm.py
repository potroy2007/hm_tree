import itertools

perms = list(itertools.permutations(["MPP", "CMP", "EryA", "GMP", "MEP"]))

trees = ["((({},{}){}){}){};".format(*x) for x in perms]
trees += ["((({}){},{}){}){};".format(*x) for x in perms]
trees += ["((({}){}){},{}){};".format(*x) for x in perms]
trees += ["(({},{}){},{}){};".format(*x) for x in perms]
trees += ["(({}){},({}){}){};".format(*x) for x in perms]

print(len(trees))

with open ("trees.tre", "w+") as o:
    for t in trees:
        o.write(t + "\n")
