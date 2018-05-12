# Networking

Attempting to play the [Vortex Wargames](http://overthewire.org/wargames/vortex/vortex0.html) I had to read up on my networking. [This](http://beej.us/guide/bgnet/pdf/bgnet_A4.pdf) is what I read, and this is what I learned.

## TCP vs UDP

There are two types of internet sockets: stream and dgram. (`SOCK_STREAM` & `SOCK_DGRAM`). The latter is a connectionless socket. For stream sockets the data transmission protocol used is TCP: Transmission Control Protocol. It ensures damn well that messages arrive in order and error-free. Datagram sockets use UDP: User Datagram Protocol. The main difference between both is the connectionless nature of `SOCK_DGRAM`. With these sockets you can just slap an IP address on a packet and send it off. It may or may not arrive. TCP is way more reliable, but for some things where packet loss is less of an issue than performance (multiplayer games, video conferencing, audio streaming) UDP might be the way to go. Sending of packets and not managing the connection etc.. is a great boost in performance.

## IPv4 and IPv6

IPv4 are 4 bytes, separated by dots: 192.0.0.111.
IPv6 are 16 bytes, in hexadecimal, separated by a colon every two bytes:

2001:0db8:c9d2:0012:0000:0000:0000:0051

IPv6 has an insane amount of addresses (over a million IPv4 internets per star in the galaxy :D), so most of the time there will be lots of zeroes in the adress. You can compress an adress with lots of zeros by using double colons, and also skip leading '0's:

2001:db8:c9d2:12::51

0000:0000:0000:0000:0000:0000:0000:0001 or ::1 is the loopback address, meaning "the machine I'm working on right now" eeg localhost.

IPv6 addresses are compatible with IPv4, with this notation you can reference an IPv4 address in IPv6: ::ffff:192.0.2.33

## Subnets

Sometimes you'd want a bunch of addresses on the HOST side, eg: 192.0.10.[0-20] has 20 addresses on the host side. 192.0.10.0 reaches that network and then proceeds to find the machine on that subnet. A part of the address would then be the network side, and a part host. Most of the times you'd have 3 bits network side, and 1 bit host side. This is a class C network, and you can have 256 machines on it. If you were lucky enough you could have gotten a class A network: only 1 byte network, and 3 host side meaning 24 bits of machines (millions of machines on a network). Some class B's exist in the middle.

The network portion of the IP is called the *netmask*, which you bitwise AND with the IP to get the network number out of it. Class C might look like: 255.255.255.0, bitwise AND with 192.0.2.12 gives you 192.0.2.0, the network address for the subnet. Since we were also running out of class C networks, you could have something like this as subnet mask: 255.255.255.252, which is 30 bits of network and 2 bits of host allowing 4 hosts on the network.

Netmask is always a bunch of 1's followed by a bunch of 0's. But using decimal numbers is a bit unwieldy for humans, since you don't really know how many 1 bits that is, and it's not compact. So New Style came along, which is the host IP address, then a slash, then a number of bits in decimal.

192.0.2.12/30 means host is at 192.0.2.12 keeping 30bits for network, and 2 bits (4 machines) on the host.

IPv6 has something similar: 2001:db8::/32 or 2001:db8:5413:4028::9db9/64.

Nice analogy for ports: Think of the IP address as the street address of a hotel, and the port number as the room number. That's a decent analogy; maybe later I'll come up with one involving the automobile industry.
Nice anecdote for ports: DOOM used port 666.

## Byte order

On some machines (Intel, ..) byte storage is reversed. The two bytes hex value b34f is logically stored as the byte b3 followed by 4f, called Big-Endian, but usually stored as 4f then b3, Little Endian. Network prefers Big Endian (more logical), but processors sometimes prefer Little Endian. You get to assume Host Byte Order is wrong, and use a function to flip them around when needed for portable code. Network Byte Order is almost always Big Endian.

There are two types of numbers you can convert: short (two bytes) and long (4 bytes). The functions work for signed and unsigned versions. Function names stat with 'h' (host), then 'to', then 'n' (network), then 's' or 'l' for short or long. 'h' and 'n' can be flipped for conversion order. Examples: `htons()`, `htonl()`, `ntohs()`, `ntohl()`
