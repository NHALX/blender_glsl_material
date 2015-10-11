SHELL := /bin/bash
RACKET=racket -l errortrace -t
OBJDIR = ../obj/render-osg
SRCDIR = ./src

INCLUDES:=-I../ -I../../osg/include/ -I../embed/s7/ -I$(SRCDIR)
CFLAGS := $(INCLUDES) 
#CFLAGS+=-flto -fdata-sections -ffunction-sections -Wl,--gc-sections -fmerge-all-constants
CFLAGS += -ggdb -g3 -g -O0 -std=c++1y
#CFLAGS += -DWITH_LINEAR_ALGEBRA -DWITH_SHADER_LINK -DWITH_UNIFORM 
CFLAGS += -DWITH_LINEAR_ALGEBRA -DWITH_UNIFORM

LDFLAGS=-L../../osg/lib -L../obj/
#LDFLAGS+=-Wl,-Bstatic -llinear-algebra
LDFLAGS+=-Wl,-Bdynamic -losg -losgViewer -losgManipulator -losgGA -losgDB -losg -losgShadow -losgUtil 

OBJS = 	../obj/embed/s7/s7.o \
#		../obj/shader-link/shader-link.o \
#		../obj/liblinear-algebra.a



BIND_OBJS =	$(OBJDIR)/BindUniform.o \
			$(OBJDIR)/Shadow.o \
			$(OBJDIR)/ShaderTest.o \
			$(OBJDIR)/BlenderObject.o \
			$(OBJDIR)/Matrix.o \
			$(OBJDIR)/BlenderRenderState.o \
			$(OBJDIR)/RenderTexture.o 

#generated/%.cc : $(SRCDIR)/%.cc.rkt
#	$(RACKET) $< > $@

$(OBJDIR)/%.o : $(SRCDIR)/%.cc.rkt
	(set -o pipefail && $(RACKET) $< \
		| (g++ -xc++ -c $(CFLAGS) -o $@ - || ($(RACKET) $< && exit 1)))



all : bind

test/% : test/%.in 
	cat test/header $< > $@
#sed -i 's/gl_\([a-zA-Z0-9_]*\)Matrix/my_\1Matrix/g' $@
	sed -i 's/gl_ModelViewMatrix/my_ModelViewMatrix/g' $@
	sed -i 's/gl_ModelViewMatrixInverse/my_ModelViewMatrixInverse/g' $@
	sed -i 's/gl_ProjectionMatrix/my_ProjectionMatrix/g' $@
	sed -i 's/gl_ProjectionMatrixInverse/my_ProjectionMatrixInverse/g' $@
	sed -i 's/gl_NormalMatrix/my_NormalMatrix/g' $@
	cat test/header $< > $@

shader: test/material.frag test/material.vert
	ls -l $^



$(SRCDIR)/%.hh.gch : precompile/%.hh
	g++ -c $(CFLAGS) $< -o $@

depends: $(SRCDIR)/OpenSceneGraph.hh.gch

bind: $(OBJS) $(BIND_OBJS)
	g++ ../generated/s7-ffi-linear-algebra.c \
	    ../generated/s7-ffi-uniform.c \
	    ../generated/s7-scheme.c \
			$(CFLAGS) $(LDFLAGS) $(OBJS) $(BIND_OBJS) -o $@

##################################
eval-script: $(OBJS) $(OBJDIR)/Matrix.o
	g++ ../generated/s7-ffi-linear-algebra.c \
	    ../generated/s7-scheme.c \
        $(OBJDIR)/Matrix.o \
			$(CFLAGS) $(LDFLAGS) $(OBJS) eval/eval.cc -o $@

clean:
	find ../obj/render-osg/ -iname \*.o | xargs rm
	rm $(SRCDIR)/OpenSceneGraph.hh.gch
