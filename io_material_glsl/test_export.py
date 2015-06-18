import sys,imp,struct
import gpu
from io_material_glsl.strtab import dynamic_type_name
from mathutils import Vector,Matrix,Color

#TODO: delete this retarded shit
def equiv(a,b):
    cmp = lambda x: abs(x[0] - x[1]) < 0.000005
    
    def cmpvec(a,b):
        for x in zip(a,b):
            if not cmp(x):
                print("CMPVEC", x[0],x[1],abs(x[0]-x[1]))
                return False
            
        return True
    
    if type(a) is Matrix and type(b) is Matrix:
        for r in zip(a,b):
            if cmpvec(r[0],r[1]) == False:
                return False;
            
        return True
    
    try:       
        if len(a) == len(b):
            return cmpvec(a,b)
        print("fallthrough", len(a), len(b))
        return False
    except:        
        pass

    try:
        print("CMP")
        return cmp((a,b))
    
    except:
        return False


def init_dump():
    with open("gpu_codegen_debug.py") as f:
        txt = f.read()  #= open("gpu_codegen_debug.py").read()
        
    dump = imp.new_module('dump')
    exec(txt, dump.__dict__)
        
    matrix3x3 = lambda x: Matrix([x[:3],x[3:6],x[6:9]]).transposed()
    matrix4x4 = lambda x: Matrix([x[:4],x[4:8],x[8:12],x[12:]]).transposed()

    parse = {
        gpu.GPU_DATA_1I  : lambda v: struct.unpack("i",v[:1*4])[0],
        gpu.GPU_DATA_1F  : lambda v: struct.unpack("f",v[:1*4])[0],
        gpu.GPU_DATA_2F  : lambda v: struct.unpack("ff",v[:2*4]),
        gpu.GPU_DATA_3F  : lambda v: struct.unpack("fff",v[:3*4]),
        gpu.GPU_DATA_4F  : lambda v: struct.unpack("ffff",v[:4*4]),
        gpu.GPU_DATA_9F  : lambda v: matrix3x3(struct.unpack("fffffffff", v[:9*4])),
        gpu.GPU_DATA_16F : lambda v: matrix4x4(struct.unpack("ffffffffffffffff",v[:16*4]))
    }

    def get(u):
        if u["type"] in dump.uniform_type:
            v = dump.uniform_type[u["type"]]
            return parse[u["datatype"]](v)
        else:
            return None
        
    dump.reference = get
    return dump


#del(dump)
    
    
def test_conversion(uniform, expr):
    dump = init_dump()
    name = dynamic_type_name[uniform["type"]]
    reference = dump.reference(uniform)
    
    if reference:
        value     = expr.evaluate()
        if value != reference:
            if not equiv(value,reference):
                print("MISMATCH: %s:\n value: <%r>\n !=\n reference: <%r>\n" %
                      (name, value, reference))
            else: 
                print("WARNING: %s is similar but not equal" % name)
        else:
            print("pass:", name, value, reference);
    else:
        print("missing:", name)
