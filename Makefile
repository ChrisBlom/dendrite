


# (define chipmunk-sources `(,@chipmunk-includes
#                            ./chipmunk-6.2.1/src/*.c
#                            ./chipmunk-6.2.1/src/constraints/*.c))

window:
	csc -framework OpenGL  window.scm ; and rlwrap ./window

chip:
	csc -k -s -C --std=gnu99 -C -DCP_PI=3.14159265358979323846264338327950288 \
		-I"./Chipmunk2D/include/chipmunk" ./Chipmunk2D/src/*.c chipmunk.scm -v

bind:
	csc -k -s -C --std=gnu99 -C -DCP_PI=3.14159265358979323846264338327950288 \
		-I"./Chipmunk2D/include/chipmunk" chipmunk.scm -v

test:
	csi chipmunk-test.scm


.PHONY: window

all: chip test
