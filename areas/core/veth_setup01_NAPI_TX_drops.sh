#!/bin/bash -x
#
# Reproducer for veth TX drops in threaded NAPI mode
#
# Script adapted from Yan Zhai
#
function root_check_run_with_sudo() {
    # Trick so, program can be run as normal user, will just use "sudo"
    #  call as root_check_run_as_sudo "$@"
    if [ "$EUID" -ne 0 ]; then
        if [ -x $0 ]; then # Directly executable use sudo
            echo "Not root, running with sudo"
            sudo "$0" "$@"
            exit $?
        fi
        err 4 "cannot perform sudo run of $0"
    fi
}
root_check_run_with_sudo "$@"

# Cleanup any old netns
ip netns delete n1
ip netns delete n2
ip netns delete n3

# Commands from Yan
# -----------------
# setting up three ns and create a bottleneck veth pair "v23-v32"
ip netns add n1
ip netns add n2
ip netns add n3

for n in n1 n2 n3; do
        ip -n $n link set lo up
done

# To easier show the problem
#  - limit veth to single TX + RX queues
ip -n n1 link add v12 numtxqueues 1 numrxqueues 1 type veth peer name v21 numtxqueues 1 numrxqueues 1 netns n2
ip -n n3 link add v32 numtxqueues 1 numrxqueues 1 type veth peer name v23 numtxqueues 1 numrxqueues 1 netns n2

ip -n n1 link set v12 up
ip -n n3 link set v32 up
ip -n n2 link set v21 up
ip -n n2 link set v23 up

ip -n n1 addr add dev lo 1.1.1.1/32
ip -n n2 addr add dev lo 2.2.2.2/32
ip -n n3 addr add dev lo 3.3.3.3/32
ip netns exec n2 sysctl -w net.ipv4.ip_forward=1

ip -n n1 addr add dev v12 192.168.1.2/31
ip -n n2 addr add dev v21 192.168.1.3/31

ip -n n2 addr add dev v23 192.168.2.2/31
ip -n n3 addr add dev v32 192.168.2.3/31

ip -n n1 route add default via 192.168.1.3 src 1.1.1.1
ip -n n2 route add 3.3.3.3 via 192.168.2.3 src 2.2.2.2

ip -n n2 route add 1.1.1.1 via 192.168.1.2 src 2.2.2.2
ip -n n3 route add default via 192.168.2.2 src 3.3.3.3

# Setting GRO on will force using NAPI rather than backlog
#  - hint we don't need to load XDP-prog
ip netns exec n3 ethtool -K v32 gro on
ip netns exec n2 ethtool -K v23 tso off # if testing with UDP
# set up threaded NAPI
ip netns exec n3 bash -c "echo 1 > /sys/class/net/v32/threaded"

# Making NAPI thread slower via many iptables rules
ip netns exec n3 bash -c '
 for n in `seq 1 1000`; do
  iptables -I INPUT -d 3.3.3.3;
 done
'

#### Manual test commands
## first run UDP server
# ip netns exec n3 iperf -i 1 -s -4 -l 1450 -u

## and client UDP test
# ip netns exec n1 iperf -i 1 -c 3.3.3.3 -u -l 1450 -b 2g -P 10

## TCP test
# ip netns exec n3 iperf -i 1 -s -4 -l 1450
# ip netns exec n1 iperf -i 1 -c 3.3.3.3

## Ping latency observations
# ip netns exec n1 ping 3.3.3.3
# ip netns exec n1 ping -i 0.005 -c 100 3.3.3.3 -q

## Enter
# ip netns exec n1 bash

### TC qdisc setup
## Add a qdisc to v23 (in n2)
##
# ip netns exec n2 tc qdisc replace dev v23 root sfq
# ip netns exec n2 tc qdisc replace dev v23 root fq_codel
# ip netns exec n2 tc qdisc replace dev v23 root fq
# ip netns exec n2 tc qdisc replace dev v23 root pfifo
#
## Clear existing setup
# ip netns exec n2 tc qdisc del dev v23 root
#
## Monitor
# ip netns exec n2 watch -d ifconfig  # See TX dropped counter
# ip netns exec n2 watch -d tc -s qdisc
