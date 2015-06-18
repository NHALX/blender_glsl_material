from sys import stderr

#TODO: rename to match_lambda?
def match(d,va_index=slice(None)):
    """
Alternative to python-3's singledispatch generic function. Similer to haskell style case statement.
usage examples: 
f = match({ int : lambda v: "A:"+str(v),
            str : lambda v: "B:"+v })

f(1)   == "A:1"
f("X") == "B:X"

# match on multiple args:
g = match({ (int,str)    : lambda a,b: "A",
            (tuple,list) : lambda a,b: "B" })

g(1,"S") == "A"
g((1,2),[3,4]) == "B"

# match on specific args via slice:
h = match({ int : lambda x,y,z: y,
            str : lambda x,y,z: z }, 
       va_index=slice(1,2))

g(None,1,None) == 1
g(None,"1",x)  == x

"""
    if type(va_index) == slice:
        i = lambda v: tuple(type(x) for x in v[va_index])
    else:
        i = lambda v: type(v[va_index])
        
    def match_dispatch(*v):
       try:
            return d[i(v)](*v)
        
       except KeyError as e:
          err  = ["ERROR: match_t: partial function:",
                  ">\tvalue out of domain:",
                  ">\t    " + str(e),
                  ">\tcovered domain is:"]
          for x in d:
              err += [">\t    %s" % str(x)]
              
          stderr.write("\n".join(err) + "\n")
          raise(e)  
        
    return match_dispatch



