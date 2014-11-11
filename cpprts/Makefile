include ../config.mk

OBJS = bigint.o io.o main.o types.o utility.o vm.o
HDRS = bigint.h box.h callbacks.h exceptions.h idris_runtime.h io.h lambdas.h main.h mem.h types.h types_aliases.h types_extern.h utility.h vm.h
CXXFLAGS:=-fPIC $(CPPFLAGS:)
CXXFLAGS += $(GMP_INCLUDE_DIR) $(GMP) -DIDRIS_TARGET_OS="\"$(OS)\""
CXXFLAGS += "-std=c++11"
CXXFLAGS += -DIDRIS_TARGET_TRIPLE="\"$(MACHINE)\""
CXXFLAGS += "-O2"
CXXFLAGS += "-DNDEBUG"
CXXFLAGS += "-ftree-vectorize"
CXXFLAGS += "-fno-rtti"
CXXFLAGS += "-fno-exceptions"
CXXFLAGS += "-fomit-frame-pointer"

LIBTARGET = libidris_cpp_rts.a

build: $(LIBTARGET) $(DYLIBTARGET)

$(LIBTARGET) : $(OBJS)
	ar r $(LIBTARGET) $(OBJS)
	ranlib $(LIBTARGET)

install :
	install -d $(TARGET)/include $(TARGET)/lib
	install $(HDRS) $(TARGET)/include/
	install $(LIBTARGET) $(TARGET)/lib/

clean :
	rm -f $(OBJS) $(LIBTARGET) $(DYLIBTARGET)

.PHONY: build install clean
