/* -*- mode: c; c-file-style: "linux" -*-
 *
 *  Copyright 2019 Joel Lehtonen
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <sys/socket.h>
#include <string.h>

int send_fd(int unix_sock, int fd)
{
	// From https://blog.cloudflare.com/know-your-scm_rights/
	// SSH requires 1 byte and that's why not using sendFd from Network.Socket
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
