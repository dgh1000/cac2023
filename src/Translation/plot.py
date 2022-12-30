import matplotlib.pyplot as plt
import numpy as np
import random
import re

def readFile():
    f = open("out.csv", "r")
    list1 = []
    list2 = []
    for line in f.readlines():
        mo = re.match(r"\s*([\d.]+)\s*,\s*([\d.]+)", line)
        list1.append(float(mo.group(1)))
        list2.append(float(mo.group(2)))
    return (list1, list2)

readFile()

# print(random.choice(["compose", "program", "checks"]))



def test():
    list1, list2 = readFile()
    # t = np.arange(0.0, 2.0, 0.01)
    # s = 1 + np.sin(2*np.pi*t)
    fig, ax = plt.subplots()
    ax.plot(list1, list2, 'r.')
    ax.set(xlabel="loc", ylabel="slice dur", title="out.csv")
    ax.grid()
    fig.savefig("test.png")
    plt.show()

test()
