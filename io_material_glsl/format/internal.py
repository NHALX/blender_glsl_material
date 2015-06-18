# Internal format of our expressions
class Tree(object):
    def __init__(self, v, *children):
        self.val      = v
        self.children = children
            
    def transform(self, n):
        return n(self.val, tuple(x.transform(n) for x in self.children))

    def evaluate(self):        
        return self.transform(lambda f,xs: f(*xs))


class Expression(object):
    def __init__(self,v,n=None):
        if callable(v):
            self.name = n if n else v.__name__
            self.func = v
            self.leaf_nf = False
        else:
            self.name = n
            self.func = lambda *_: v
            self.leaf_nf = True # expression can't be further reduced
    
    def __call__(self,*xs):
        return self.func(*xs)

Expr = lambda v,*xs: Tree(Expression(v), *xs)
Val  = lambda n,v:   Tree(Expression(v,n))

