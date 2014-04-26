# Makefile for (>>>project<<<)
# Create Date: (>>>DATE<<<)

TARGET = (>>>project<<<)
OBJS = (>>>POINT<<<)
CC = gcc
CFLAGS = -Wall
RM = rm -f

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $^ -o $@


%.o: %.c %.h

.PHONY: clean
clean:
	$(RM) $(OBJS) $(TARGET)

>>>TEMPLATE-DEFINITION-SECTION<<<
("project" "project name: ")
