CFLAGS = -std=c99 -pedantic -Wall -Werror -Wextra -D_FORTIFY_SOURCE=2 -O3 -fPIC -fstack-protector-all -pie -s

.PHONY: all

all: export

export: export.c
	$(CC) $(CFLAGS) -o $@ $<
