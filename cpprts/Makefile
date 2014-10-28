include ../config.mk

OBJS = *.o
HDRS = *.h
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
	mkdir -p $(TARGET)/include $(TARGET)/lib
	install $(HDRS) $(TARGET)/include/
	install $(LIBTARGET) $(TARGET)/lib/

clean :
	rm -f $(OBJS) $(LIBTARGET) $(DYLIBTARGET)

.PHONY: build install clean
