/* -*- mode: c; c-file-style: "linux" -*-
 */

#include <stdio.h>
#include <err.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdint.h>
#include "ssh.h"

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

	// Saying hello with protocol version 4
	
	const char helloseq[] = {0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x04};
	write(sock_ssh, helloseq, sizeof(helloseq));

	// Cutting some corners with the protocol
	char reply[sizeof(helloseq)];
	if (read(sock_ssh, reply, sizeof(reply)) <= 0) {
		err(3, "Socket reply is too short");
	}

	if (memcmp(helloseq, reply, sizeof(helloseq)) != 0) {
		errx(3, "Socket reply is invalid. Invalid version?");		
	}

	// Now we have an up and running SSH controlmaster connection
	
	printf("Prööt\n");

	// dumppaa kaikki paske
	uint8_t arvo;
	while( read(sock_ssh, &arvo, 1) > 0) {
		printf("got %02x\n", arvo);
	}
}
