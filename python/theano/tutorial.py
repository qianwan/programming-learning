import theano.tensor as T

from theano.tensor.shared_randomstreams import RandomStreams
from theano import function
from theano import pp

import cPickle

x = T.dscalar('x')
y = T.dscalar('y')
z = x + y
f = function([x, y], z)

#print f(2.3, 4.9)

x = T.dmatrix('x')
y = T.dmatrix('y')
z = x + y
f = function([x, y], z)

#print f([[1, 2]], [[3, 1.2]])

srng = RandomStreams(seed=234)
rv_n = srng.normal((2,2))
f = function([], rv_n)
print f()
