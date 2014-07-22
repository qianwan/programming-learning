#!/user/bin/env python

import numpy as np
from StringIO import StringIO

data = '1, 2, 3\n4, 5, 6'
print np.genfromtxt(StringIO(data), delimiter = ',')
data = "123456789\n   4  7 9\n   4567 9"
print np.genfromtxt(StringIO(data), delimiter = (4, 3, 2))
data = """#
# skip me
1, 2, 3
4, 5, 6 # skip
7, 8, 9
"""
print np.genfromtxt(StringIO(data), delimiter = ',', comments = '#')
data = "1 2 3\n4 5 6"
print np.genfromtxt(StringIO(data), usecols=(-1, 0))
print np.genfromtxt(StringIO(data), names="a, b, c", usecols=("a", "c"))
