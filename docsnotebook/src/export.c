#include<unistd.h>

int main(int argc, char *argv[], char *envp[]) {
	if (argc == 2) {
		char *args[] = {"./export.el", argv[1], NULL};
		execve("./export.el", args, envp);
	}
}
