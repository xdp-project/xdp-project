/*
 * ssh-mirror - SSH self redirector
 * Copyright (C) 2019 Matteo Croce <mcroce@redhat.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

/*
 * compile with:
 * clang -O3 -c ssh-mirror.c -o - -emit-llvm |llc -march=bpf - -filetype=obj -o ssh-mirror.o
 * run with:
 * ip link set eth0 xdp object ssh-mirror.o
 */

#include <stdint.h>
#include <string.h>
#include <arpa/inet.h>
#include <linux/bpf.h>
#include <linux/if_ether.h>
#include <linux/ip.h>
#include <linux/tcp.h>

#define SEC(NAME) __attribute__((section(NAME), used))

#define PORT 22

SEC("prog")
int xdp_main(struct xdp_md *ctx)
{
	void *data_end = (void *)(uintptr_t)ctx->data_end;
	void *data = (void *)(uintptr_t)ctx->data;

	struct ethhdr *eth = data;
	struct iphdr *iph = (struct iphdr *)(eth + 1);
	struct tcphdr *tcph = (struct tcphdr *)(iph + 1);

	/* keep the preverivier happy and skip short packets */
	if (tcph + 1 > (struct tcphdr *)data_end)
		return XDP_PASS;

	/* skip non TCP packets */
	if (eth->h_proto != ntohs(ETH_P_IP) || iph->protocol != IPPROTO_TCP)
		return XDP_PASS;

	/* match! */
	if (tcph->dest == ntohs(PORT) || tcph->source == ntohs(PORT)) {
		char teth[ETH_ALEN];
		uint32_t tip;

		/* swap MAC addresses */
		memcpy(teth, eth->h_dest, ETH_ALEN);
		memcpy(eth->h_dest, eth->h_source, ETH_ALEN);
		memcpy(eth->h_source, &teth, ETH_ALEN);

		/* swap IP addresses */
		tip = iph->daddr;
		iph->daddr = iph->saddr;
		iph->saddr = tip;

		/* retransmit */
		return XDP_TX;
	}

	return XDP_PASS;
}

char _license[] SEC("license") = "GPL";
