/* -*- mode: c; c-file-style: "linux" -*-
 */

#include <stdio.h>
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

int main(int argc, char **argv)
{
	if (argc != 2) {
		errx(1, "Missing ControlMaster socket name");
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
	
	printf("Prööt\n");

	// dumppaa kaikki paske
	int arvo;
	while((arvo = fgetc(h)) >= 0) {
		printf("got %02x\n", arvo);
	}
	return 0;
}
