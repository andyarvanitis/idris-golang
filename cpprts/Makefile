include ../config.mk

OBJS = bigint.o io.o main.o types.o utility.o vm.o
HDRS = box.h exceptions.h io.h main.h mem.h types.h utility.h vm.h
CPPFLAGS:=-fPIC $(CPPFLAGS:)
CPPFLAGS += $(GMP_INCLUDE_DIR) $(GMP) -DIDRIS_TARGET_OS="\"$(OS)\""
CPPFLAGS += "-std=c++11"
CPPFLAGS += -DIDRIS_TARGET_TRIPLE="\"$(MACHINE)\""

LIBTARGET = libidris_cpp_rts.a

build: $(LIBTARGET) $(DYLIBTARGET)

$(LIBTARGET) : $(OBJS)
	ar r $(LIBTARGET) $(OBJS)
	ranlib $(LIBTARGET)

install :
	mkdir -p $(TARGET)
	install $(LIBTARGET) $(HDRS) $(TARGET)

clean :
	rm -f $(OBJS) $(LIBTARGET) $(DYLIBTARGET)

.PHONY: build install clean
