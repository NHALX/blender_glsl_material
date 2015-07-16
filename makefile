RACKET=racket -l errortrace -t
INCLUDES=-Iembed/s7/ -I.
CFLAGS_UNUSED=-flto -fdata-sections -ffunction-sections -Wl,--gc-sections -fmerge-all-constants
CFLAGS=-ggdb -g3 -g -O0 $(INCLUDES) 

OBJS = obj/shader-link/shader-link.o \
		obj/embed/s7/s7.o \
		obj/liblinear-algebra.a

all: generated benchmark eval bind

generated: generated/shader-link.h \
			generated/linear-algebra.h \
			generated/uniform.h \
			generated/s7-ffi-linear-algebra.c \
			generated/s7-ffi-uniform.c \
            generated/s7-scheme.h \
            generated/s7-scheme.c
	ls -l $^
	(cd generated && ln -sf s7-scheme.h scheme.h)

#%/scheme-s7.h %/scheme-s7.c: binding/export/linear-algebra.scm \
#							 binding/export/shader-link.scm \
#							 binding/export/uniform.scm
#
#	racket -l errortrace -t binding/s7-ffi.c.rkt $^ > $*/scheme-s7.c

generated/s7-scheme.h : binding/export/linear-algebra.scm binding/export/uniform.scm
	$(RACKET) binding/s7-scheme.h.rkt $^ > $@

generated/s7-scheme.c : binding/export/linear-algebra.scm binding/export/uniform.scm
	$(RACKET) binding/s7-scheme.c.rkt $^ > $@

generated/s7-ffi-%.c : binding/export/%.scm binding/s7-ffi.rkt
	$(RACKET) binding/s7-ffi.rkt $< > $@

generated/%.h : binding/export/%.scm
	$(RACKET) binding/c-header.rkt $< > $@

obj/%.o : %.c
	gcc -c $(CFLAGS) $< -o $@

obj/%.o : %.cpp
	g++ -c $(CFLAGS) $< -o $@


eval: $(OBJS) generated/scheme-s7.c eval.c
	gcc -DWITH_LINEAR_ALGEBRA \
		-Wl,-Bstatic -llinear-algebra -Lobj \
		$(CFLAGS) $^ -lm -llinear-algebra -lm \
		-Wl,-Bdynamic -o $@

#		-Wl,-Bdynamic 

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


benchmark: $(OBJS) generated/scheme-s7.c benchmark.c
	gcc -DWITH_LINEAR_ALGEBRA -DWITH_SHADER_LINK \
		-static -Lobj $(CFLAGS) $^ -lm -llinear-algebra -lm -o $@

obj/liblinear-algebra.a: obj/linear-algebra/impl_sse.o \
					 obj/linear-algebra/matrix.o \
					 obj/linear-algebra/vector.o \
				  	 obj/linear-algebra/mem_alloc.o

	gcc-ar cr $@ $^



clean:
	-mv benchmark trash/
	-mv bind trash/
	-mv eval trash/
	find obj -iname '*.o' -exec mv '{}' trash/ \;
