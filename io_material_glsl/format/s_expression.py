# converts from internal format to S-EXPR format
if __name__ == '__main__':
    class Matrix: pass
    class Color: pass
    class Vector: pass
else:
    from mathutils import Matrix, Vector, Color

from io_material_glsl.dispatch import match


#valid_id = lambda v: "|%s|" % v -- some schemes dont support this syntax

# TODO: kill this global state
valid_id_counter = -1
valid_id_dict    = {}
def valid_id(v):
    global valid_id_counter
    global valid_id_dict
    if v not in valid_id_dict:
        valid_id_counter += 1
        valid_id_dict[v]  = valid_id_counter
        return valid_id_counter
    else:
        return valid_id_dict[v]

#symbol: {str || (str,str) || (str,int)} -> str
symbol = match({ (str,str) : lambda fmt,v: fmt.format(valid_id(v)),
                 (str,int) : lambda fmt,v: fmt.format(v),
                 (tuple,)  : lambda x: symbol(*x),
                 (str,)    : lambda x: x })




str_seq = lambda v: (" ".join(["%r"]*len(v))) % tuple(v)

concat = lambda z: [x for y in z for x in y]

def vector(v):
    if len(v) == 4:
        return "(v4 %r %r %r %r)" % tuple(v)
    elif len(v) == 3:
        return "(v4-from3 %r %r %r)" % tuple(v)
    else:
        raise "vector: bad length"
    
#value: BlenderType -> {vector || string || float || int}
value = match(
    { Matrix : lambda v: "(m44 %s)" % str_seq(concat(v.transposed().col)),
      Color  : lambda v: vector(v),
      Vector : lambda v: vector(v),
      tuple  : lambda v: vector(v),
      str    : lambda v: "\"%s\"" % v,
      bool   : lambda v: "#t" if v else "#f",
      float  : lambda v: repr(v),
      int    : lambda v: repr(v) },
    va_index=0)


def attribute(t,v,channel,attr_name):
    return "(attribute-%s \"%s\" %d \"%s\" usr)" % (t,v,channel,attr_name.replace("_","-"))

def group_defines(xs):
    xs.sort()
    return '\n'.join(xs) + "\n"
            
def group_uniforms(xs):
    xs.sort()
    return "(define (bind-uniforms usr) \n  (begin\n    %s))\n\n" % '\n    '.join(xs)

def group_attributes(xs):
    xs.sort()
    return "(define (bind-attributes usr) \n  (begin\n    %s))\n\n" % '\n    '.join(xs)

def uniform(t,v,expr):
        
    def show(x, xs):
        if x.leaf_nf:
            return symbol(x.name)
        else:
            b = " ".join(xs)
            return "(%s%s%s)" % (x.name.replace("_","-"), " " if b else "", b)
        
    return "(uniform-%s \"%s\" %s usr)" % (t,v,expr.transform(show))

def default(expr, seen = set()):
    
    def show(x, xs):
        if x.leaf_nf:
            sym = symbol(x.name)
            if sym in seen:
                return ""
            else:
                seen.add(sym)
                # x.__call__ evaluates fully only because were at a leaf node
                return "(define %s %s)" % (sym, value(x()))
        else:
            return "\n".join(filter(lambda x: x != "", xs))

    return expr.transform(show)


if __name__ == '__main__':
    def f(): return 0
    def g(): return 0
    from internal import Expr,Val
    expr = Expr(f,
                Expr(g, Val(("lamp-{}-spot-size", 0), 1)),
                Val(("lamp-{}-shadow-buffer-clip-start", 0), 2),
                Val(("lamp-{}-shadow-buffer-clip-end", 0), 3),
                Val(("lamp-{}-matrix<obj=>world>", 0), 4),
                Val(("lamp-{}-matrix<obj=>world>", 0), 4),
                Val("matrix<camera=>world>", 5))

    v1 = uniform("test","test",expr)
    v2 = default(expr)
    
    r1 = '(uniform-test "test" (f (g (lamp-0-spot-size)) (lamp-0-shadow-buffer-clip-start) (lamp-0-shadow-buffer-clip-end) (lamp-0-matrix<obj=>world>) (lamp-0-matrix<obj=>world>) (matrix<camera=>world>)))'
    r2 = "\n".join(['(define lamp-0-spot-size 1)',
                    '(define lamp-0-shadow-buffer-clip-start 2)',
                    '(define lamp-0-shadow-buffer-clip-end 3)',
                    '(define lamp-0-matrix<obj=>world> 4)',
                    '(define matrix<camera=>world> 5)'])

    print(v2)
    print(v1)
    
    assert v1 == r1
    assert v2 == r2, "\n>%s\n>%s" % (v2,r2)
