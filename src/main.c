/* -*- mode: c; c-file-style: "linux" -*-
 */

#include <stdio.h>
#include <stdlib.h>
#include <err.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdint.h>
#include <stdbool.h>
#include <arpa/inet.h>
#include "ssh.h"

bool send_to_ssh(FILE *h, uint32_t length, const void *data);
int read_from_ssh(FILE *h, uint32_t buf_size, void *buf);
int send_fd(int unix_sock, int fd);

bool send_to_ssh(FILE *h, uint32_t length, const void *data)
{
	uint32_t length_net = htonl(length);
	if (fwrite(&length_net, sizeof(length_net), 1, h) < 1) return false;
	if (fwrite(data, length, 1, h) < 1) return false;
	if (fflush(h) != 0) return false;
	return true;
}

int read_from_ssh(FILE *h, uint32_t buf_size, void *buf)
{
	uint32_t length_net;
	if (fread(&length_net, sizeof(length_net), 1, h) < 1) return -1;
	uint32_t length = ntohl(length_net);
	if (length > buf_size) return -2;
	if (fread(buf, length, 1, h) < 1) return -1;
	return length;
}

int send_fd(int unix_sock, int fd)
{
	// From https://blog.cloudflare.com/know-your-scm_rights/
	struct iovec iov = {.iov_base = "\0", // Must send at least one byte
			    .iov_len = 1};

	union {
		char buf[CMSG_SPACE(sizeof(fd))];
		struct cmsghdr align;
	} u;

	struct msghdr msg = {.msg_iov = &iov,
			     .msg_iovlen = 1,
			     .msg_control = u.buf,
			     .msg_controllen = sizeof(u.buf)};

	struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
	*cmsg = (struct cmsghdr){.cmsg_level = SOL_SOCKET,
				 .cmsg_type = SCM_RIGHTS,
				 .cmsg_len = CMSG_LEN(sizeof(fd))};

	memcpy(CMSG_DATA(cmsg), &fd, sizeof(fd));

	return sendmsg(unix_sock, &msg, 0);
}

int main(int argc, char **argv)
{
	if (argc != 4) {
		printf("Usage: %s controlmaster_socket host port\n", argv[0]);
		exit(1);
	}
	
	// Magic to create UNIX socket to SSH control master

	struct sockaddr_un addr;
	int sock_ssh;

	if ( (sock_ssh = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
		err(2, "UNIX socket functionality not available");
	}

	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strncpy(addr.sun_path, argv[1], sizeof(addr.sun_path)-1);

	if (connect(sock_ssh, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
		err(2, "Unable to connect to socket '%s'", argv[1]);
	}

	FILE *h = fdopen(sock_ssh, "r+");
	if (h == NULL) {
		err(2, "Unable to handle socket");
	}

	// Saying hello with protocol version 4
	
	const char helloseq[] = {0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x04};
	if (!send_to_ssh(h, sizeof(helloseq), helloseq)) {
		errx(3, "SSH socket write failed");
	}

	// Cutting some corners with the protocol
	char reply[sizeof(helloseq)];
	if (read_from_ssh(h, sizeof(reply), reply) < 8) {
		errx(3, "Didn't get enough data");
	}

	if (memcmp(helloseq, reply, sizeof(helloseq)) != 0) {
		errx(3, "Socket reply is invalid. Invalid version?");		
	}

	// Now we have an up and running SSH controlmaster connection
	char *host = argv[2];
	uint32_t port = atoi(argv[3]);

	// Allocate 2*32bit integers and 3*null terminators
	int host_len = strlen(host);
	int all_len = 20 + host_len;
	void *data = malloc(all_len);
	if (data == NULL) {
		err(4, "malloc failed");
	}

	// Fill in the data with this quite dangerous-looking pointer magic.
	*((uint32_t*)(data+0)) = htonl(MUX_C_NEW_STDIO_FWD); // MUX_C_NEW_STDIO_FWD
	*((uint32_t*)(data+4)) = 13;                         // request id
	*((uint32_t*)(data+8)) = 0;                          // reserved
	*((uint32_t*)(data+12)) = htonl(host_len);
	strcpy((char*)(data+16), host);                      // connect host
	*((uint32_t*)(data+16+host_len)) = htonl(port);      // connect port

	if (!send_to_ssh(h, all_len, data)) {
		errx(3, "SSH socket write failed");
	}

	send_fd(sock_ssh, STDIN_FILENO);
	send_fd(sock_ssh, STDOUT_FILENO);

	// Cutting some corners with the protocol
	char reply2[256];
	int reply2_len = read_from_ssh(h, sizeof(reply2), reply2);

	if (reply2_len < 4) {
		errx(3, "Didn't get enough data, got %d", reply2_len);
	}

	int retcode = ntohl(*(uint32_t*)reply2);
	if (retcode == MUX_S_SESSION_OPENED) {
		// All is fine
		printf("Prööt\n");
	} else if (retcode == MUX_S_FAILURE) {
		fputs("Forwarding failed: ", stdout);
		fwrite(reply2 + 12, ntohl(*(uint32_t*)(reply2 + 8)), 1, stdout);
		fputs("\n", stdout);
	} else {
		errx(3, "Mysterious error code %08x", retcode);
	}

	// dumppaa kaikki paske
	int arvo;
	while((arvo = fgetc(h)) >= 0) {
		printf("got %02x\n", arvo);
	}
	return 0;
}
